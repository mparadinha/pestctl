const std = @import("std");
const tracy = @import("build_tracy.zig");

pub fn build(b: *std.build.Builder) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();

    const use_tracy = b.option(bool, "tracy", "Enable Tracy profiling") orelse false;

    var exe = b.addExecutable("pestctl", "src/main.zig");
    exe.setTarget(target);
    exe.setBuildMode(mode);
    exe.linkLibC();
    // glfw
    exe.linkSystemLibrary("glfw");
    // intelXED
    exe.addObjectFile("xed/obj/libxed.a");
    exe.addIncludePath("xed/obj");
    exe.addIncludePath("xed/include/public");
    exe.addIncludePath("xed/include/public/xed");
    // stb libs
    exe.addIncludePath("src");
    exe.addCSourceFile("src/stb_impls.c", &[_][]u8{""});
    exe.install();
    _ = tracy.link(b, exe, if (use_tracy) "tracy-0.8.2" else null);

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const exe_tests = b.addTest("src/main.zig");
    exe_tests.setTarget(target);
    exe_tests.setBuildMode(mode);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&exe_tests.step);
}
