const std = @import("std");
const tracy = @import("build_tracy.zig");

pub fn build(b: *std.build.Builder) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const use_tracy = b.option(bool, "tracy", "Enable Tracy profiling") orelse false;

    const glfw_dep = b.dependency("libglfw3", .{ .target = target, .optimize = optimize });
    const zydis_dep = b.dependency("Zydis", .{ .target = target, .optimize = optimize });

    var exe = b.addExecutable(.{
        .name = "pestctl",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    exe.linkLibC();
    exe.linkLibrary(glfw_dep.artifact("glfw3"));
    // hack: Zydis depends on the headers of Zycore (and builds fine on
    // its own), but because *we* don't explicitly depend on Zycore the build
    // system doesn't include this path in the `-I` options
    exe.addIncludePath("zig-cache/i/5ee76773ecd672df5c2a451f45bf8661/include");
    exe.linkLibrary(zydis_dep.artifact("Zydis"));
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
