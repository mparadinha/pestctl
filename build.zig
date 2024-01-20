const std = @import("std");
const tracy = @import("build_tracy.zig");

pub fn build(b: *std.build.Builder) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const use_tracy = b.option(bool, "tracy", "Enable Tracy profiling") orelse false;

    var exe = b.addExecutable(.{
        .name = "pestctl",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    exe.linkLibC();
    {
        const zig_ui_dep = b.dependency("zig_ui", .{ .target = target, .optimize = optimize });
        const zig_ui_mod = zig_ui_dep.module("zig-ui");
        exe.linkLibC();
        exe.addModule("zig-ui", zig_ui_mod);
        @import("zig_ui").link(zig_ui_dep.builder, exe);
    }
    {
        const zydis_dep = b.dependency("Zydis", .{ .target = target, .optimize = .ReleaseSafe });
        exe.linkLibrary(zydis_dep.artifact("Zydis"));
    }
    { // tracy
        _ = tracy.link(b, exe, if (use_tracy) "tracy" else null);
    }
    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| run_cmd.addArgs(args);
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const test_step = b.step("test", "Run unit tests");
    const exe_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    test_step.dependOn(&exe_tests.step);
}
