const std = @import("std");

const wayland = @import("wayland");
const wl = wayland.client.wl;
const zriver = wayland.client.zriver;

const c = @cImport({
    @cInclude("systemd/sd-bus.h");
});

const WlContext = struct {
    status_manager: ?*zriver.StatusManagerV1 = null,
    seats: std.ArrayList(*wl.Seat) = std.ArrayList(*wl.Seat).init(std.heap.c_allocator),
};

pub fn main() !void {
    var args = std.process.args();
    _ = args.next();

    const mode = args.next() orelse return error.ArgRequired;
    if (std.mem.eql(u8, mode, "river-mode")) {
        try riverMode();
    } else if (std.mem.eql(u8, mode, "dunst")) {
        try dunst();
    } else if (std.mem.eql(u8, mode, "idle-inhibit")) {
        try idleInhibit();
    }
}

fn riverMode() !void {
    const display = try wl.Display.connect(null);
    const registry = try display.getRegistry();

    var context = WlContext{};
    registry.setListener(*WlContext, registryListener, &context);
    if (display.roundtrip() != .SUCCESS)
        return error.RoundtripFailed;

    const status_manager = context.status_manager orelse return error.RiverStatusManagerNotAdvertised;
    for (context.seats.items) |seat| {
        const seat_status = try status_manager.getRiverSeatStatus(seat);
        seat_status.setListener(?*anyopaque, seatStatusListener, null);
    }

    while (true) {
        if (display.dispatch() != .SUCCESS)
            return error.DispatchFailed;
    }
}

fn dunst() !void {
    var user_bus: ?*c.sd_bus = null;
    if (c.sd_bus_default_user(&user_bus) < 0) {
        return error.DBusConnectError;
    }

    const bus = user_bus orelse unreachable;
    if (c.sd_bus_add_match(
        bus,
        null,
        "path='/org/freedesktop/Notifications',interface='org.freedesktop.DBus.Properties',member='PropertiesChanged'",
        handleNotificationPropertiesChanged,
        bus,
    ) < 0) {
        return error.DBusAddMatchError;
    }

    // Print initial state immediately
    try dunstPrintIsPaused(bus);

    while (true) {
        _ = c.sd_bus_wait(bus, std.math.maxInt(u64));
        _ = c.sd_bus_process(bus, null);
    }
}

fn handleNotificationPropertiesChanged(_: ?*c.sd_bus_message, userdata: ?*anyopaque, _: [*c]c.sd_bus_error) callconv(.C) c_int {
    const bus: *c.sd_bus = @ptrCast(@alignCast(userdata));
    dunstPrintIsPaused(bus) catch return -1;
    return 0;
}

fn dunstPrintIsPaused(bus: *c.sd_bus) !void {
    const w = std.io.getStdOut().writer();
    const is_paused = try dunstIsPaused(bus);
    const state = if (is_paused) "paused" else "unpaused";
    try w.print("{{\"text\":\"{s}\", \"alt\":\"{s}\", \"class\":\"{s}\"}}\n", .{ state, state, state });
}

fn dunstIsPaused(bus: *c.sd_bus) anyerror!bool {
    var msg: *c.sd_bus_message = undefined;
    var err: c.sd_bus_error = c.sd_bus_error{
        .name = undefined,
        .message = undefined,
        ._need_free = 0,
    };
    defer {
        c.sd_bus_error_free(&err);
        _ = c.sd_bus_message_unref(msg);
    }

    if (c.sd_bus_call_method(
        bus,
        "org.freedesktop.Notifications",
        "/org/freedesktop/Notifications",
        "org.freedesktop.DBus.Properties",
        "Get",
        &err,
        @ptrCast(&msg),
        "ss",
        "org.dunstproject.cmd0",
        "paused",
    ) < 0)
        return error.DBusMethodCallError;

    var is_paused: c_int = undefined;
    const res = c.sd_bus_message_read(msg, "v", "b", &is_paused);
    if (res < 0) {
        std.debug.print("{d}: {any}\n", .{ res, msg });
        return error.DBusMessageReadError;
    }

    return is_paused == 1;
}

fn idleInhibit() !void {
    while (true) {
        std.time.sleep(5 * std.time.ns_per_s);
    }
}

fn registryListener(registry: *wl.Registry, event: wl.Registry.Event, context: *WlContext) void {
    switch (event) {
        .global => |global| {
            const interface = global.interface;
            if (std.mem.orderZ(u8, interface, zriver.StatusManagerV1.interface.name) == .eq) {
                context.status_manager = registry.bind(global.name, zriver.StatusManagerV1, 4) catch return;
            } else if (std.mem.orderZ(u8, interface, wl.Seat.interface.name) == .eq) {
                const seat = registry.bind(global.name, wl.Seat, 9) catch return;
                context.seats.append(seat) catch @panic("out of memory");
            }
        },
        .global_remove => {},
    }
}

fn seatStatusListener(_: *zriver.SeatStatusV1, event: zriver.SeatStatusV1.Event, _: ?*anyopaque) void {
    switch (event) {
        .mode => |mode| {
            const w = std.io.getStdOut().writer();
            if (!std.mem.eql(u8, std.mem.span(mode.name), "normal")) {
                const s = mode.name;
                w.print("{{\"text\":\"{s}\", \"alt\":\"{s}\", \"class\":\"{s}\"}}\n", .{ s, s, s }) catch return;
            } else {
                w.print("{{}}\n", .{}) catch return;
            }
        },
        .focused_output => {},
        .unfocused_output => {},
        .focused_view => {},
    }
}
