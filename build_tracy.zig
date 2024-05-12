const std = @import("std");
const join = std.fs.path.join;

/// Build required sources, use tracy by importing "tracy.zig"
pub fn link(b: *std.Build, step: *std.Build.Step.Compile, opt_path: ?[]const u8) void {
    const options = b.addOptions();
    options.addOption(bool, "tracy_enabled", opt_path != null);
    step.root_module.addImport("build_options", options.createModule());

    if (opt_path) |path| {
        const alloc = b.allocator;
        const public_path = join(alloc, &.{ path, "public" }) catch unreachable;
        step.addIncludePath(.{ .path = public_path });
        const tracy_path = join(alloc, &.{ path, "public", "tracy" }) catch unreachable;
        step.addIncludePath(.{ .path = tracy_path });
        const tracy_client_source_path = join(alloc, &.{ public_path, "TracyClient.cpp" }) catch unreachable;
        step.addCSourceFile(.{
            .file = b.path(tracy_client_source_path),
            .flags = &.{
                "-DTRACY_ENABLE",
                // MinGW doesn't have all the newfangled windows features,
                // so we need to pretend to have an older windows version.
                "-D_WIN32_WINNT=0x601",
                "-fno-sanitize=undefined",
            },
        });

        step.linkLibC();
        step.linkSystemLibrary("c++");

        if (step.target.isWindows()) {
            step.linkSystemLibrary("Advapi32");
            step.linkSystemLibrary("User32");
            step.linkSystemLibrary("Ws2_32");
            step.linkSystemLibrary("DbgHelp");
        }
    }
}
