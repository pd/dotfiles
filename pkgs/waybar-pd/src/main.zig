const std = @import("std");

const wayland = @import("wayland");
const wl = wayland.client.wl;
const zriver = wayland.client.zriver;

const Context = struct {
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

    var context = Context{};
    registry.setListener(*Context, registryListener, &context);
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
    // Should surely use a dbus lib instead but this is the most direct port for now
    const alloc = std.heap.page_allocator;
    const argv = [_][]const u8{
        "dbus-monitor",
        "--profile",
        "path='/org/freedesktop/Notifications',interface='org.freedesktop.DBus.Properties',member='PropertiesChanged'",
    };

    var dbusMonitor = std.process.Child.init(&argv, alloc);
    dbusMonitor.stdout_behavior = .Pipe;
    try dbusMonitor.spawn();

    const stdout = dbusMonitor.stdout orelse unreachable;
    var buffered_reader = std.io.bufferedReader(stdout.reader());
    var line = std.ArrayList(u8).init(alloc);

    while (true) {
        // We don't actually care what it prints, just that it did
        line.clearRetainingCapacity();
        buffered_reader.reader().streamUntilDelimiter(line.writer(), '\n', null) catch |err| switch (err) {
            error.EndOfStream => {
                _ = try dbusMonitor.wait();
                return;
            },
            else => return err,
        };

        // TODO use dbus-send which is all dunstctl is doing?
        // dbus-send --print-reply=literal --dest=org.freedesktop.Notifications /org/freedesktop/Notifications org.freedesktop.DBus.Properties.Get string:org.dunstproject.cmd0 string:paused

        // for now just call dunstctl itself
        const dunstArgv = [_][]const u8{ "dunstctl", "is-paused" };
        const result = try std.process.Child.run(.{
            .allocator = alloc,
            .argv = &dunstArgv,
        });

        const w = std.io.getStdOut().writer();
        if (std.mem.eql(u8, result.stdout, "true\n")) {
            w.print("paused\n", .{}) catch return;
        } else {
            w.print("not paused\n", .{}) catch return;
        }
    }
}

fn idleInhibit() !void {
    while (true) {
        std.time.sleep(5 * std.time.ns_per_s);
    }
}

fn registryListener(registry: *wl.Registry, event: wl.Registry.Event, context: *Context) void {
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
                w.print("{{\"text\":\"{s}\", \"class\":\"{s}\"}}\n", .{ mode.name, mode.name }) catch return;
            } else {
                w.print("{{}}\n", .{}) catch return;
            }
        },
        .focused_output => {},
        .unfocused_output => {},
        .focused_view => {},
    }
}
