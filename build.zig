const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const tracy_path = b.option([]const u8, "tracy", "Enable Tracy profiling by proving the path to it's source");

    const build_opts = b.addOptions();

    var exe = b.addExecutable(.{
        .name = "pestctl",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    exe.linkLibC();
    {
        const zig_ui_dep = b.dependency("zig_ui", .{ .target = target, .optimize = optimize });
        exe.root_module.addImport("zig-ui", zig_ui_dep.module("zig-ui"));
    }
    {
        const zydis_dep = b.dependency("zydis", .{});
        const zycore_dep = b.dependency("zycore", .{});
        const zydis_artifact = build_zydis(zydis_dep, zycore_dep, .{ .target = target, .optimize = .ReleaseSafe });
        exe.linkLibrary(zydis_artifact);
        b.installArtifact(zydis_artifact);
    }
    if (tracy_path) |path| {
        exe.addIncludePath(joinedPath(b, &.{ path, "public" }));
        exe.addIncludePath(joinedPath(b, &.{ path, "public/tracy" }));
        exe.addCSourceFile(.{
            .file = joinedPath(b, &.{ path, "public/tracy/TracyClient.cpp" }),
            .flags = &.{
                "-DTRACY_ENABLE",
                // MinGW doesn't have all the newfangled windows features,
                // so we need to pretend to have an older windows version.
                "-D_WIN32_WINNT=0x601",
                "-fno-sanitize=undefined",
            },
        });
        exe.linkSystemLibrary("c++");
        if (target.result.os.tag == .windows) {
            exe.linkSystemLibrary("Advapi32");
            exe.linkSystemLibrary("User32");
            exe.linkSystemLibrary("Ws2_32");
            exe.linkSystemLibrary("DbgHelp");
        }
    }
    exe.root_module.addImport("build_options", build_opts.createModule());
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

const TargetOptimize = struct {
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
};

fn build_zydis(
    dep: *std.Build.Dependency,
    zycore_dep: *std.Build.Dependency,
    args: TargetOptimize,
) *std.Build.Step.Compile {
    const dep_b = dep.builder;

    const lib = dep_b.addStaticLibrary(.{
        .name = "zydis",
        .target = args.target,
        .optimize = args.optimize,
    });
    lib.linkLibC();
    lib.addIncludePath(dep.path("include"));
    lib.addIncludePath(dep.path("src")); // for the `Generated/*.inc` files
    lib.addCSourceFiles(.{
        .root = dep.path("src"),
        .files = &.{
            // main sources
            "MetaInfo.c",
            "Mnemonic.c",
            "Register.c",
            "Segment.c",
            "SharedData.c",
            "String.c",
            "Utils.c",
            "Zydis.c",
            // decoder
            "Decoder.c",
            "DecoderData.c",
            // encoder
            "Encoder.c",
            "EncoderData.c",
            // formatter
            "Disassembler.c",
            "Formatter.c",
            "FormatterBuffer.c",
            "FormatterATT.c",
            "FormatterBase.c",
            "FormatterIntel.c",
            // segment
            "Segment.c",
        },
    });
    lib.installHeadersDirectory(dep_b.path("include/Zydis"), "Zydis", .{});
    {
        const zycore_artifact = build_zycore(zycore_dep, args);
        lib.linkLibrary(zycore_artifact);
        lib.installLibraryHeaders(zycore_artifact);
    }

    return lib;
}

fn build_zycore(
    dep: *std.Build.Dependency,
    args: TargetOptimize,
) *std.Build.Step.Compile {
    const dep_b = dep.builder;

    const lib = dep_b.addStaticLibrary(.{
        .name = "zycore",
        .target = args.target,
        .optimize = args.optimize,
    });
    lib.linkLibC();
    lib.addIncludePath(dep.path("include"));
    lib.addCSourceFiles(.{
        .root = dep.path("src"),
        .files = &.{
            // API
            "API/Memory.c",
            "API/Process.c",
            "API/Synchronization.c",
            "API/Terminal.c",
            "API/Thread.c",
            // Common
            "Allocator.c",
            "ArgParse.c",
            "Bitset.c",
            "Format.c",
            "List.c",
            "String.c",
            "Vector.c",
            "Zycore.c",
        },
    });
    lib.installHeadersDirectory(dep_b.path("include/Zycore"), "Zycore", .{});

    return lib;
}

fn joinedPath(b: *std.Build, paths: []const []const u8) std.Build.LazyPath {
    const joined = std.fs.path.join(b.allocator, paths) catch unreachable;
    return b.path(joined);
}
