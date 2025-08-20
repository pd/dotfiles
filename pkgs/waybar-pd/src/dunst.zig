const std = @import("std");

const c = @cImport({
    @cInclude("systemd/sd-bus.h");
});

const DBusError = error{
    Connect,
    AddMatch,
    MethodCall,
    MessageRead,
};

pub fn monitor() !void {
    var user_bus: ?*c.sd_bus = null;
    if (c.sd_bus_default_user(&user_bus) < 0)
        return DBusError.Connect;

    const bus = user_bus orelse unreachable;
    if (c.sd_bus_add_match(
        bus,
        null,
        "path='/org/freedesktop/Notifications',interface='org.freedesktop.DBus.Properties',member='PropertiesChanged'",
        propertiesChanged,
        bus,
    ) < 0)
        return DBusError.AddMatch;

    // Print initial state immediately
    try emit(bus);

    while (true) {
        _ = c.sd_bus_wait(bus, std.math.maxInt(u64));
        _ = c.sd_bus_process(bus, null);
    }
}

fn propertiesChanged(
    _: ?*c.sd_bus_message,
    userdata: ?*anyopaque,
    _: [*c]c.sd_bus_error,
) callconv(.C) c_int {
    const bus: *c.sd_bus = @ptrCast(@alignCast(userdata));
    emit(bus) catch return -1;
    return 0;
}

fn emit(bus: *c.sd_bus) !void {
    const w = std.io.getStdOut().writer();
    const is_paused = try isPaused(bus);
    const state = if (is_paused) "paused" else "unpaused";
    try w.print("{{\"text\":\"{s}\", \"alt\":\"{s}\", \"class\":\"{s}\"}}\n", .{ state, state, state });
}

fn isPaused(bus: *c.sd_bus) DBusError!bool {
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
        return DBusError.MethodCall;

    var is_paused: c_int = undefined;
    if (c.sd_bus_message_read(msg, "v", "b", &is_paused) < 0)
        return DBusError.MessageRead;

    return is_paused == 1;
}
