const std = @import("std");
const tracy = @import("build_tracy.zig");

pub fn build(b: *std.build.Builder) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const use_tracy = b.option(bool, "tracy", "Enable Tracy profiling") orelse false;

    const glfw_dep = b.dependency("libglfw3", .{ .target = target, .optimize = optimize });

    var exe = b.addExecutable(.{
        .name = "pestctl",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    exe.linkLibC();
    exe.linkLibrary(glfw_dep.artifact("glfw3"));
    { // intelXED
        exe.addObjectFile("xed/obj/libxed.a");
        exe.addIncludePath("xed/obj");
        exe.addIncludePath("xed/include/public");
        exe.addIncludePath("xed/include/public/xed");
    }
    { // stb libs
        exe.addIncludePath("src");
        exe.addCSourceFile("src/stb_impls.c", &[_][]u8{""});
    }
    { // tracy
        _ = tracy.link(b, exe, if (use_tracy) "tracy-0.8.2" else null);
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
