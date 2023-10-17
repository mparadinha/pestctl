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
        const glfw_dep = b.dependency("mach_glfw", .{ .target = target, .optimize = .ReleaseSafe });
        exe.addModule("mach-glfw", glfw_dep.module("mach-glfw"));
        @import("mach_glfw").link(glfw_dep.builder, exe);
    }
    {
        const zydis_dep = b.dependency("Zydis", .{ .target = target, .optimize = .ReleaseSafe });
        exe.linkLibrary(zydis_dep.artifact("Zydis"));
    }
    { // stb libs
        exe.addIncludePath(.{ .path = "src" });
        exe.addCSourceFiles(&.{"src/stb_impls.c"}, &.{""});
    }
    { // tracy
        _ = tracy.link(b, exe, if (use_tracy) "tracy" else null);
    }
    b.installArtifact(exe);

    const options = b.addOptions();
    const this_dir = comptime std.fs.path.dirname(@src().file) orelse ".";
    options.addOption([]const u8, "resource_dir", this_dir ++ "/resources");
    exe.addOptions("build_opts", options);

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
