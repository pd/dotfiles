const std = @import("std");

const wayland = @import("wayland");
const wl = wayland.client.wl;
const zriver = wayland.client.zriver;

const WlContext = struct {
    status_manager: ?*zriver.StatusManagerV1 = null,
    seats: std.ArrayList(*wl.Seat) = .empty,
};

pub fn monitor() !void {
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

fn registryListener(registry: *wl.Registry, event: wl.Registry.Event, context: *WlContext) void {
    switch (event) {
        .global => |global| {
            const interface = global.interface;
            if (std.mem.orderZ(u8, interface, zriver.StatusManagerV1.interface.name) == .eq) {
                context.status_manager = registry.bind(global.name, zriver.StatusManagerV1, 4) catch return;
            } else if (std.mem.orderZ(u8, interface, wl.Seat.interface.name) == .eq) {
                const seat = registry.bind(global.name, wl.Seat, 9) catch return;
                context.seats.append(std.heap.c_allocator, seat) catch @panic("out of memory");
            }
        },
        else => {},
    }
}

fn seatStatusListener(_: *zriver.SeatStatusV1, event: zriver.SeatStatusV1.Event, _: ?*anyopaque) void {
    switch (event) {
        .mode => |mode| {
            var buf: [256]u8 = undefined;
            var stdout_writer = std.fs.File.stdout().writer(&buf);
            const w = &stdout_writer.interface;
            if (!std.mem.eql(u8, std.mem.span(mode.name), "normal")) {
                const s = mode.name;
                w.print("{{\"text\":\"{s}\", \"alt\":\"{s}\", \"class\":\"{s}\"}}\n", .{ s, s, s }) catch return;
            } else {
                w.print("{{}}\n", .{}) catch return;
            }
            w.flush() catch return;
        },
        else => {},
    }
}
