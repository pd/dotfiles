const std = @import("std");

const river = @import("./river.zig");
const dunst = @import("./dunst.zig");

pub fn main() !void {
    var args = std.process.args();
    const cmd = args.next().?;

    const mode = args.next() orelse {
        std.debug.print("usage: {s} [river-mode | dunst]\n", .{cmd});
        std.process.exit(1);
    };

    if (std.mem.eql(u8, mode, "river-mode")) {
        try river.monitor();
    } else if (std.mem.eql(u8, mode, "dunst")) {
        try dunst.monitor();
    }
}
