// most of the constants in this file were lifted from the zig 0.10.0 std library

const std = @import("std");

const DW = @This();

pub const TAG = struct {
    pub fn asStr(value: u16) []const u8 {
        return dwarfString(@This(), value);
    }

    pub fn isValid(value: u16) bool {
        return dwarfIsValid(@This(), value);
    }

    pub const padding = 0x00;
    pub const array_type = 0x01;
    pub const class_type = 0x02;
    pub const entry_point = 0x03;
    pub const enumeration_type = 0x04;
    pub const formal_parameter = 0x05;
    pub const imported_declaration = 0x08;
    pub const label = 0x0a;
    pub const lexical_block = 0x0b;
    pub const member = 0x0d;
    pub const pointer_type = 0x0f;
    pub const reference_type = 0x10;
    pub const compile_unit = 0x11;
    pub const string_type = 0x12;
    pub const structure_type = 0x13;
    pub const subroutine = 0x14;
    pub const subroutine_type = 0x15;
    pub const typedef = 0x16;
    pub const union_type = 0x17;
    pub const unspecified_parameters = 0x18;
    pub const variant = 0x19;
    pub const common_block = 0x1a;
    pub const common_inclusion = 0x1b;
    pub const inheritance = 0x1c;
    pub const inlined_subroutine = 0x1d;
    pub const module = 0x1e;
    pub const ptr_to_member_type = 0x1f;
    pub const set_type = 0x20;
    pub const subrange_type = 0x21;
    pub const with_stmt = 0x22;
    pub const access_declaration = 0x23;
    pub const base_type = 0x24;
    pub const catch_block = 0x25;
    pub const const_type = 0x26;
    pub const constant = 0x27;
    pub const enumerator = 0x28;
    pub const file_type = 0x29;
    pub const friend = 0x2a;
    pub const namelist = 0x2b;
    pub const namelist_item = 0x2c;
    pub const packed_type = 0x2d;
    pub const subprogram = 0x2e;
    pub const template_type_param = 0x2f;
    pub const template_value_param = 0x30;
    pub const thrown_type = 0x31;
    pub const try_block = 0x32;
    pub const variant_part = 0x33;
    pub const variable = 0x34;
    pub const volatile_type = 0x35;

    // DWARF 3
    pub const dwarf_procedure = 0x36;
    pub const restrict_type = 0x37;
    pub const interface_type = 0x38;
    pub const namespace = 0x39;
    pub const imported_module = 0x3a;
    pub const unspecified_type = 0x3b;
    pub const partial_unit = 0x3c;
    pub const imported_unit = 0x3d;
    pub const condition = 0x3f;
    pub const shared_type = 0x40;

    // DWARF 4
    pub const type_unit = 0x41;
    pub const rvalue_reference_type = 0x42;
    pub const template_alias = 0x43;

    // DWARF 5
    pub const coarray_type = 0x44;
    pub const generic_subrange = 0x45;
    pub const dynamic_type = 0x46;
    pub const atomic_type = 0x47;
    pub const call_site = 0x48;
    pub const call_site_parameter = 0x49;
    pub const skeleton_unit = 0x4a;
    pub const immutable_type = 0x4b;

    pub const lo_user = 0x4080;
    pub const hi_user = 0xffff;

    // SGI/MIPS Extensions.
    pub const MIPS_loop = 0x4081;

    // HP extensions.  See: ftp://ftp.hp.com/pub/lang/tools/WDB/wdb-4.0.tar.gz .
    pub const HP_array_descriptor = 0x4090;
    pub const HP_Bliss_field = 0x4091;
    pub const HP_Bliss_field_set = 0x4092;

    // GNU extensions.
    pub const format_label = 0x4101; // For FORTRAN 77 and Fortran 90.
    pub const function_template = 0x4102; // For C++.
    pub const class_template = 0x4103; //For C++.
    pub const GNU_BINCL = 0x4104;
    pub const GNU_EINCL = 0x4105;

    // Template template parameter.
    // See http://gcc.gnu.org/wiki/TemplateParmsDwarf .
    pub const GNU_template_template_param = 0x4106;

    // Template parameter pack extension = specified at
    // http://wiki.dwarfstd.org/index.php?title=C%2B%2B0x:_Variadic_templates
    // The values of these two TAGS are in the DW_TAG_GNU_* space until the tags
    // are properly part of DWARF 5.
    pub const GNU_template_parameter_pack = 0x4107;
    pub const GNU_formal_parameter_pack = 0x4108;
    // The GNU call site extension = specified at
    // http://www.dwarfstd.org/ShowIssue.php?issue=100909.2&type=open .
    // The values of these two TAGS are in the DW_TAG_GNU_* space until the tags
    // are properly part of DWARF 5.
    pub const GNU_call_site = 0x4109;
    pub const GNU_call_site_parameter = 0x410a;
    // Extensions for UPC.  See: http://dwarfstd.org/doc/DWARF4.pdf.
    pub const upc_shared_type = 0x8765;
    pub const upc_strict_type = 0x8766;
    pub const upc_relaxed_type = 0x8767;
    // PGI (STMicroelectronics; extensions.  No documentation available.
    pub const PGI_kanji_type = 0xA000;
    pub const PGI_interface_block = 0xA020;
};

pub const AT = struct {
    pub fn asStr(value: u16) []const u8 {
        return dwarfString(@This(), value);
    }

    pub fn isValid(value: u16) bool {
        return dwarfIsValid(@This(), value);
    }

    pub const sibling = 0x01;
    pub const location = 0x02;
    pub const name = 0x03;
    pub const ordering = 0x09;
    pub const subscr_data = 0x0a;
    pub const byte_size = 0x0b;
    pub const bit_offset = 0x0c;
    pub const bit_size = 0x0d;
    pub const element_list = 0x0f;
    pub const stmt_list = 0x10;
    pub const low_pc = 0x11;
    pub const high_pc = 0x12;
    pub const language = 0x13;
    pub const member = 0x14;
    pub const discr = 0x15;
    pub const discr_value = 0x16;
    pub const visibility = 0x17;
    pub const import = 0x18;
    pub const string_length = 0x19;
    pub const common_reference = 0x1a;
    pub const comp_dir = 0x1b;
    pub const const_value = 0x1c;
    pub const containing_type = 0x1d;
    pub const default_value = 0x1e;
    pub const @"inline" = 0x20;
    pub const is_optional = 0x21;
    pub const lower_bound = 0x22;
    pub const producer = 0x25;
    pub const prototyped = 0x27;
    pub const return_addr = 0x2a;
    pub const start_scope = 0x2c;
    pub const bit_stride = 0x2e;
    pub const upper_bound = 0x2f;
    pub const abstract_origin = 0x31;
    pub const accessibility = 0x32;
    pub const address_class = 0x33;
    pub const artificial = 0x34;
    pub const base_types = 0x35;
    pub const calling_convention = 0x36;
    pub const count = 0x37;
    pub const data_member_location = 0x38;
    pub const decl_column = 0x39;
    pub const decl_file = 0x3a;
    pub const decl_line = 0x3b;
    pub const declaration = 0x3c;
    pub const discr_list = 0x3d;
    pub const encoding = 0x3e;
    pub const external = 0x3f;
    pub const frame_base = 0x40;
    pub const friend = 0x41;
    pub const identifier_case = 0x42;
    pub const macro_info = 0x43;
    pub const namelist_items = 0x44;
    pub const priority = 0x45;
    pub const segment = 0x46;
    pub const specification = 0x47;
    pub const static_link = 0x48;
    pub const @"type" = 0x49;
    pub const use_location = 0x4a;
    pub const variable_parameter = 0x4b;
    pub const virtuality = 0x4c;
    pub const vtable_elem_location = 0x4d;

    // DWARF 3
    pub const allocated = 0x4e;
    pub const associated = 0x4f;
    pub const data_location = 0x50;
    pub const byte_stride = 0x51;
    pub const entry_pc = 0x52;
    pub const use_UTF8 = 0x53;
    pub const extension = 0x54;
    pub const ranges = 0x55;
    pub const trampoline = 0x56;
    pub const call_column = 0x57;
    pub const call_file = 0x58;
    pub const call_line = 0x59;
    pub const description = 0x5a;
    pub const binary_scale = 0x5b;
    pub const decimal_scale = 0x5c;
    pub const small = 0x5d;
    pub const decimal_sign = 0x5e;
    pub const digit_count = 0x5f;
    pub const picture_string = 0x60;
    pub const mutable = 0x61;
    pub const threads_scaled = 0x62;
    pub const explicit = 0x63;
    pub const object_pointer = 0x64;
    pub const endianity = 0x65;
    pub const elemental = 0x66;
    pub const pure = 0x67;
    pub const recursive = 0x68;

    // DWARF 4
    pub const signature = 0x69;
    pub const main_subprogram = 0x6a;
    pub const data_bit_offset = 0x6b;
    pub const const_expr = 0x6c;
    pub const enum_class = 0x6d;
    pub const linkage_name = 0x6e;

    // DWARF 5
    pub const string_length_bit_size = 0x6f;
    pub const string_length_byte_size = 0x70;
    pub const rank = 0x71;
    pub const str_offsets_base = 0x72;
    pub const addr_base = 0x73;
    pub const rnglists_base = 0x74;
    pub const dwo_name = 0x76;
    pub const reference = 0x77;
    pub const rvalue_reference = 0x78;
    pub const macros = 0x79;
    pub const call_all_calls = 0x7a;
    pub const call_all_source_calls = 0x7b;
    pub const call_all_tail_calls = 0x7c;
    pub const call_return_pc = 0x7d;
    pub const call_value = 0x7e;
    pub const call_origin = 0x7f;
    pub const call_parameter = 0x80;
    pub const call_pc = 0x81;
    pub const call_tail_call = 0x82;
    pub const call_target = 0x83;
    pub const call_target_clobbered = 0x84;
    pub const call_data_location = 0x85;
    pub const call_data_value = 0x86;
    pub const @"noreturn" = 0x87;
    pub const alignment = 0x88;
    pub const export_symbols = 0x89;
    pub const deleted = 0x8a;
    pub const defaulted = 0x8b;
    pub const loclists_base = 0x8c;

    pub const lo_user = 0x2000; // Implementation-defined range start.
    pub const hi_user = 0x3fff; // Implementation-defined range end.

    // SGI/MIPS extensions.
    pub const MIPS_fde = 0x2001;
    pub const MIPS_loop_begin = 0x2002;
    pub const MIPS_tail_loop_begin = 0x2003;
    pub const MIPS_epilog_begin = 0x2004;
    pub const MIPS_loop_unroll_factor = 0x2005;
    pub const MIPS_software_pipeline_depth = 0x2006;
    pub const MIPS_linkage_name = 0x2007;
    pub const MIPS_stride = 0x2008;
    pub const MIPS_abstract_name = 0x2009;
    pub const MIPS_clone_origin = 0x200a;
    pub const MIPS_has_inlines = 0x200b;

    // HP extensions.
    pub const HP_block_index = 0x2000;
    pub const HP_unmodifiable = 0x2001; // Same as AT.MIPS_fde.
    pub const HP_prologue = 0x2005; // Same as AT.MIPS_loop_unroll.
    pub const HP_epilogue = 0x2008; // Same as AT.MIPS_stride.
    pub const HP_actuals_stmt_list = 0x2010;
    pub const HP_proc_per_section = 0x2011;
    pub const HP_raw_data_ptr = 0x2012;
    pub const HP_pass_by_reference = 0x2013;
    pub const HP_opt_level = 0x2014;
    pub const HP_prof_version_id = 0x2015;
    pub const HP_opt_flags = 0x2016;
    pub const HP_cold_region_low_pc = 0x2017;
    pub const HP_cold_region_high_pc = 0x2018;
    pub const HP_all_variables_modifiable = 0x2019;
    pub const HP_linkage_name = 0x201a;
    pub const HP_prof_flags = 0x201b; // In comp unit of procs_info for -g.
    pub const HP_unit_name = 0x201f;
    pub const HP_unit_size = 0x2020;
    pub const HP_widened_byte_size = 0x2021;
    pub const HP_definition_points = 0x2022;
    pub const HP_default_location = 0x2023;
    pub const HP_is_result_param = 0x2029;

    // GNU extensions.
    pub const sf_names = 0x2101;
    pub const src_info = 0x2102;
    pub const mac_info = 0x2103;
    pub const src_coords = 0x2104;
    pub const body_begin = 0x2105;
    pub const body_end = 0x2106;
    pub const GNU_vector = 0x2107;
    // Thread-safety annotations.
    // See http://gcc.gnu.org/wiki/ThreadSafetyAnnotation .
    pub const GNU_guarded_by = 0x2108;
    pub const GNU_pt_guarded_by = 0x2109;
    pub const GNU_guarded = 0x210a;
    pub const GNU_pt_guarded = 0x210b;
    pub const GNU_locks_excluded = 0x210c;
    pub const GNU_exclusive_locks_required = 0x210d;
    pub const GNU_shared_locks_required = 0x210e;
    // One-definition rule violation detection.
    // See http://gcc.gnu.org/wiki/DwarfSeparateTypeInfo .
    pub const GNU_odr_signature = 0x210f;
    // Template template argument name.
    // See http://gcc.gnu.org/wiki/TemplateParmsDwarf .
    pub const GNU_template_name = 0x2110;
    // The GNU call site extension.
    // See http://www.dwarfstd.org/ShowIssue.php?issue=100909.2&type=open .
    pub const GNU_call_site_value = 0x2111;
    pub const GNU_call_site_data_value = 0x2112;
    pub const GNU_call_site_target = 0x2113;
    pub const GNU_call_site_target_clobbered = 0x2114;
    pub const GNU_tail_call = 0x2115;
    pub const GNU_all_tail_call_sites = 0x2116;
    pub const GNU_all_call_sites = 0x2117;
    pub const GNU_all_source_call_sites = 0x2118;
    // Section offset into .debug_macro section.
    pub const GNU_macros = 0x2119;
    // Extensions for Fission.  See http://gcc.gnu.org/wiki/DebugFission.
    pub const GNU_dwo_name = 0x2130;
    pub const GNU_dwo_id = 0x2131;
    pub const GNU_ranges_base = 0x2132;
    pub const GNU_addr_base = 0x2133;
    pub const GNU_pubnames = 0x2134;
    pub const GNU_pubtypes = 0x2135;
    // VMS extensions.
    pub const VMS_rtnbeg_pd_address = 0x2201;
    // GNAT extensions.
    // GNAT descriptive type.
    // See http://gcc.gnu.org/wiki/DW_AT_GNAT_descriptive_type .
    pub const use_GNAT_descriptive_type = 0x2301;
    pub const GNAT_descriptive_type = 0x2302;
    // UPC extension.
    pub const upc_threads_scaled = 0x3210;
    // PGI (STMicroelectronics) extensions.
    pub const PGI_lbase = 0x3a00;
    pub const PGI_soffset = 0x3a01;
    pub const PGI_lstride = 0x3a02;
};

pub const OP = struct {
    pub fn asStr(value: u16) []const u8 {
        return dwarfString(@This(), value);
    }

    pub fn isValid(value: u16) bool {
        return dwarfIsValid(@This(), value);
    }

    pub const addr = 0x03;
    pub const deref = 0x06;
    pub const const1u = 0x08;
    pub const const1s = 0x09;
    pub const const2u = 0x0a;
    pub const const2s = 0x0b;
    pub const const4u = 0x0c;
    pub const const4s = 0x0d;
    pub const const8u = 0x0e;
    pub const const8s = 0x0f;
    pub const constu = 0x10;
    pub const consts = 0x11;
    pub const dup = 0x12;
    pub const drop = 0x13;
    pub const over = 0x14;
    pub const pick = 0x15;
    pub const swap = 0x16;
    pub const rot = 0x17;
    pub const xderef = 0x18;
    pub const abs = 0x19;
    pub const @"and" = 0x1a;
    pub const div = 0x1b;
    pub const minus = 0x1c;
    pub const mod = 0x1d;
    pub const mul = 0x1e;
    pub const neg = 0x1f;
    pub const not = 0x20;
    pub const @"or" = 0x21;
    pub const plus = 0x22;
    pub const plus_uconst = 0x23;
    pub const shl = 0x24;
    pub const shr = 0x25;
    pub const shra = 0x26;
    pub const xor = 0x27;
    pub const bra = 0x28;
    pub const eq = 0x29;
    pub const ge = 0x2a;
    pub const gt = 0x2b;
    pub const le = 0x2c;
    pub const lt = 0x2d;
    pub const ne = 0x2e;
    pub const skip = 0x2f;
    pub const lit0 = 0x30;
    pub const lit1 = 0x31;
    pub const lit2 = 0x32;
    pub const lit3 = 0x33;
    pub const lit4 = 0x34;
    pub const lit5 = 0x35;
    pub const lit6 = 0x36;
    pub const lit7 = 0x37;
    pub const lit8 = 0x38;
    pub const lit9 = 0x39;
    pub const lit10 = 0x3a;
    pub const lit11 = 0x3b;
    pub const lit12 = 0x3c;
    pub const lit13 = 0x3d;
    pub const lit14 = 0x3e;
    pub const lit15 = 0x3f;
    pub const lit16 = 0x40;
    pub const lit17 = 0x41;
    pub const lit18 = 0x42;
    pub const lit19 = 0x43;
    pub const lit20 = 0x44;
    pub const lit21 = 0x45;
    pub const lit22 = 0x46;
    pub const lit23 = 0x47;
    pub const lit24 = 0x48;
    pub const lit25 = 0x49;
    pub const lit26 = 0x4a;
    pub const lit27 = 0x4b;
    pub const lit28 = 0x4c;
    pub const lit29 = 0x4d;
    pub const lit30 = 0x4e;
    pub const lit31 = 0x4f;
    pub const reg0 = 0x50;
    pub const reg1 = 0x51;
    pub const reg2 = 0x52;
    pub const reg3 = 0x53;
    pub const reg4 = 0x54;
    pub const reg5 = 0x55;
    pub const reg6 = 0x56;
    pub const reg7 = 0x57;
    pub const reg8 = 0x58;
    pub const reg9 = 0x59;
    pub const reg10 = 0x5a;
    pub const reg11 = 0x5b;
    pub const reg12 = 0x5c;
    pub const reg13 = 0x5d;
    pub const reg14 = 0x5e;
    pub const reg15 = 0x5f;
    pub const reg16 = 0x60;
    pub const reg17 = 0x61;
    pub const reg18 = 0x62;
    pub const reg19 = 0x63;
    pub const reg20 = 0x64;
    pub const reg21 = 0x65;
    pub const reg22 = 0x66;
    pub const reg23 = 0x67;
    pub const reg24 = 0x68;
    pub const reg25 = 0x69;
    pub const reg26 = 0x6a;
    pub const reg27 = 0x6b;
    pub const reg28 = 0x6c;
    pub const reg29 = 0x6d;
    pub const reg30 = 0x6e;
    pub const reg31 = 0x6f;
    pub const breg0 = 0x70;
    pub const breg1 = 0x71;
    pub const breg2 = 0x72;
    pub const breg3 = 0x73;
    pub const breg4 = 0x74;
    pub const breg5 = 0x75;
    pub const breg6 = 0x76;
    pub const breg7 = 0x77;
    pub const breg8 = 0x78;
    pub const breg9 = 0x79;
    pub const breg10 = 0x7a;
    pub const breg11 = 0x7b;
    pub const breg12 = 0x7c;
    pub const breg13 = 0x7d;
    pub const breg14 = 0x7e;
    pub const breg15 = 0x7f;
    pub const breg16 = 0x80;
    pub const breg17 = 0x81;
    pub const breg18 = 0x82;
    pub const breg19 = 0x83;
    pub const breg20 = 0x84;
    pub const breg21 = 0x85;
    pub const breg22 = 0x86;
    pub const breg23 = 0x87;
    pub const breg24 = 0x88;
    pub const breg25 = 0x89;
    pub const breg26 = 0x8a;
    pub const breg27 = 0x8b;
    pub const breg28 = 0x8c;
    pub const breg29 = 0x8d;
    pub const breg30 = 0x8e;
    pub const breg31 = 0x8f;
    pub const regx = 0x90;
    pub const fbreg = 0x91;
    pub const bregx = 0x92;
    pub const piece = 0x93;
    pub const deref_size = 0x94;
    pub const xderef_size = 0x95;
    pub const nop = 0x96;

    // DWARF 3 extensions.
    pub const push_object_address = 0x97;
    pub const call2 = 0x98;
    pub const call4 = 0x99;
    pub const call_ref = 0x9a;
    pub const form_tls_address = 0x9b;
    pub const call_frame_cfa = 0x9c;
    pub const bit_piece = 0x9d;

    // DWARF 4 extensions.
    pub const implicit_value = 0x9e;
    pub const stack_value = 0x9f;

    // DWARF 5 extensions.
    pub const implicit_pointer = 0xa0;
    pub const addrx = 0xa1;
    pub const constx = 0xa2;
    pub const entry_value = 0xa3;
    pub const const_type = 0xa4;
    pub const regval_type = 0xa5;
    pub const deref_type = 0xa6;
    pub const xderef_type = 0xa7;
    pub const convert = 0xa8;
    pub const reinterpret = 0xa9;

    pub const lo_user = 0xe0; // Implementation-defined range start.
    pub const hi_user = 0xff; // Implementation-defined range end.

    // GNU extensions.
    pub const GNU_push_tls_address = 0xe0;
    // The following is for marking variables that are uninitialized.
    pub const GNU_uninit = 0xf0;
    pub const GNU_encoded_addr = 0xf1;
    // The GNU implicit pointer extension.
    // See http://www.dwarfstd.org/ShowIssue.php?issue=100831.1&type=open .
    pub const GNU_implicit_pointer = 0xf2;
    // The GNU entry value extension.
    // See http://www.dwarfstd.org/ShowIssue.php?issue=100909.1&type=open .
    pub const GNU_entry_value = 0xf3;
    // The GNU typed stack extension.
    // See http://www.dwarfstd.org/doc/040408.1.html .
    pub const GNU_const_type = 0xf4;
    pub const GNU_regval_type = 0xf5;
    pub const GNU_deref_type = 0xf6;
    pub const GNU_convert = 0xf7;
    pub const GNU_reinterpret = 0xf9;
    // The GNU parameter ref extension.
    pub const GNU_parameter_ref = 0xfa;
    // Extension for Fission.  See http://gcc.gnu.org/wiki/DebugFission.
    pub const GNU_addr_index = 0xfb;
    pub const GNU_const_index = 0xfc;
    // HP extensions.
    pub const HP_unknown = 0xe0; // Ouch, the same as GNU_push_tls_address.
    pub const HP_is_value = 0xe1;
    pub const HP_fltconst4 = 0xe2;
    pub const HP_fltconst8 = 0xe3;
    pub const HP_mod_range = 0xe4;
    pub const HP_unmod_range = 0xe5;
    pub const HP_tls = 0xe6;
    // PGI (STMicroelectronics) extensions.
    pub const PGI_omp_thread_num = 0xf8;
    // Wasm extensions.
    pub const WASM_location = 0xed;
    pub const WASM_local = 0x00;
    pub const WASM_global = 0x01;
    pub const WASM_global_u32 = 0x03;
    pub const WASM_operand_stack = 0x02;
};

pub const LANG = struct {
    pub fn asStr(value: u16) []const u8 {
        return dwarfString(@This(), value);
    }

    pub fn isValid(value: u16) bool {
        return dwarfIsValid(@This(), value);
    }

    pub const C89 = 0x0001;
    pub const C = 0x0002;
    pub const Ada83 = 0x0003;
    pub const C_plus_plus = 0x0004;
    pub const Cobol74 = 0x0005;
    pub const Cobol85 = 0x0006;
    pub const Fortran77 = 0x0007;
    pub const Fortran90 = 0x0008;
    pub const Pascal83 = 0x0009;
    pub const Modula2 = 0x000a;
    pub const Java = 0x000b;
    pub const C99 = 0x000c;
    pub const Ada95 = 0x000d;
    pub const Fortran95 = 0x000e;
    pub const PLI = 0x000f;
    pub const ObjC = 0x0010;
    pub const ObjC_plus_plus = 0x0011;
    pub const UPC = 0x0012;
    pub const D = 0x0013;
    pub const Python = 0x0014;
    pub const OpenCL = 0x0015;
    pub const Go = 0x0016;
    pub const Modula3 = 0x0017;
    pub const Haskell = 0x0018;
    pub const C_plus_plus_03 = 0x0019;
    pub const C_plus_plus_11 = 0x001a;
    pub const OCaml = 0x001b;
    pub const Rust = 0x001c;
    pub const C11 = 0x001d;
    pub const Swift = 0x001e;
    pub const Julia = 0x001f;
    pub const Dylan = 0x0020;
    pub const C_plus_plus_14 = 0x0021;
    pub const Fortran03 = 0x0022;
    pub const Fortran08 = 0x0023;
    pub const RenderScript = 0x0024;
    pub const BLISS = 0x0025;

    pub const lo_user = 0x8000;
    pub const hi_user = 0xffff;

    pub const Mips_Assembler = 0x8001;
    pub const Upc = 0x8765;
    pub const HP_Bliss = 0x8003;
    pub const HP_Basic91 = 0x8004;
    pub const HP_Pascal91 = 0x8005;
    pub const HP_IMacro = 0x8006;
    pub const HP_Assembler = 0x8007;
};

pub const FORM = struct {
    pub fn asStr(value: u16) []const u8 {
        return dwarfString(@This(), value);
    }

    pub fn isValid(value: u16) bool {
        return dwarfIsValid(@This(), value);
    }

    pub const addr = 0x01;
    pub const block2 = 0x03;
    pub const block4 = 0x04;
    pub const data2 = 0x05;
    pub const data4 = 0x06;
    pub const data8 = 0x07;
    pub const string = 0x08;
    pub const block = 0x09;
    pub const block1 = 0x0a;
    pub const data1 = 0x0b;
    pub const flag = 0x0c;
    pub const sdata = 0x0d;
    pub const strp = 0x0e;
    pub const udata = 0x0f;
    pub const ref_addr = 0x10;
    pub const ref1 = 0x11;
    pub const ref2 = 0x12;
    pub const ref4 = 0x13;
    pub const ref8 = 0x14;
    pub const ref_udata = 0x15;
    pub const indirect = 0x16;
    pub const sec_offset = 0x17;
    pub const exprloc = 0x18;
    pub const flag_present = 0x19;
    pub const strx = 0x1a;
    pub const addrx = 0x1b;
    pub const ref_sup4 = 0x1c;
    pub const strp_sup = 0x1d;
    pub const data16 = 0x1e;
    pub const line_strp = 0x1f;
    pub const ref_sig8 = 0x20;
    pub const implicit_const = 0x21;
    pub const loclistx = 0x22;
    pub const rnglistx = 0x23;
    pub const ref_sup8 = 0x24;
    pub const strx1 = 0x25;
    pub const strx2 = 0x26;
    pub const strx3 = 0x27;
    pub const strx4 = 0x28;
    pub const addrx1 = 0x29;
    pub const addrx2 = 0x2a;
    pub const addrx3 = 0x2b;
    pub const addrx4 = 0x2c;

    // Extensions for Fission.  See http://gcc.gnu.org/wiki/DebugFission.
    pub const GNU_addr_index = 0x1f01;
    pub const GNU_str_index = 0x1f02;

    // Extensions for DWZ multifile.
    // See http://www.dwarfstd.org/ShowIssue.php?issue=120604.1&type=open .
    pub const GNU_ref_alt = 0x1f20;
    pub const GNU_strp_alt = 0x1f21;
};

pub const ATE = struct {
    pub fn asStr(value: u16) []const u8 {
        return dwarfString(@This(), value);
    }

    pub fn isValid(value: u16) bool {
        return dwarfIsValid(@This(), value);
    }

    pub const @"void" = 0x0;
    pub const address = 0x1;
    pub const boolean = 0x2;
    pub const complex_float = 0x3;
    pub const float = 0x4;
    pub const signed = 0x5;
    pub const signed_char = 0x6;
    pub const unsigned = 0x7;
    pub const unsigned_char = 0x8;

    // DWARF 3.
    pub const imaginary_float = 0x9;
    pub const packed_decimal = 0xa;
    pub const numeric_string = 0xb;
    pub const edited = 0xc;
    pub const signed_fixed = 0xd;
    pub const unsigned_fixed = 0xe;
    pub const decimal_float = 0xf;

    // DWARF 4.
    pub const UTF = 0x10;

    // DWARF 5.
    pub const UCS = 0x11;
    pub const ASCII = 0x12;

    pub const lo_user = 0x80;
    pub const hi_user = 0xff;

    // HP extensions.
    pub const HP_float80 = 0x80; // Floating-point (80 bit).
    pub const HP_complex_float80 = 0x81; // Complex floating-point (80 bit).
    pub const HP_float128 = 0x82; // Floating-point (128 bit).
    pub const HP_complex_float128 = 0x83; // Complex fp (128 bit).
    pub const HP_floathpintel = 0x84; // Floating-point (82 bit IA64).
    pub const HP_imaginary_float80 = 0x85;
    pub const HP_imaginary_float128 = 0x86;
    pub const HP_VAX_float = 0x88; // F or G floating.
    pub const HP_VAX_float_d = 0x89; // D floating.
    pub const HP_packed_decimal = 0x8a; // Cobol.
    pub const HP_zoned_decimal = 0x8b; // Cobol.
    pub const HP_edited = 0x8c; // Cobol.
    pub const HP_signed_fixed = 0x8d; // Cobol.
    pub const HP_unsigned_fixed = 0x8e; // Cobol.
    pub const HP_VAX_complex_float = 0x8f; // F or G floating complex.
    pub const HP_VAX_complex_float_d = 0x90; // D floating complex.
};

pub const LLE = struct {
    pub fn asStr(value: u16) []const u8 {
        return dwarfString(@This(), value);
    }

    pub fn isValid(value: u16) bool {
        return dwarfIsValid(@This(), value);
    }

    pub const end_of_list = 0x00;
    pub const base_addressx = 0x01;
    pub const startx_endx = 0x02;
    pub const startx_length = 0x03;
    pub const offset_pair = 0x04;
    pub const default_location = 0x05;
    pub const base_address = 0x06;
    pub const start_end = 0x07;
    pub const start_length = 0x08;
};

pub const CFA = struct {
    pub fn asStr(value: u16) []const u8 {
        return dwarfString(@This(), value);
    }

    pub fn isValid(value: u16) bool {
        return dwarfIsValid(@This(), value);
    }

    pub const advance_loc = 0x40;
    pub const offset = 0x80;
    pub const restore = 0xc0;
    pub const nop = 0x00;
    pub const set_loc = 0x01;
    pub const advance_loc1 = 0x02;
    pub const advance_loc2 = 0x03;
    pub const advance_loc4 = 0x04;
    pub const offset_extended = 0x05;
    pub const restore_extended = 0x06;
    pub const @"undefined" = 0x07;
    pub const same_value = 0x08;
    pub const register = 0x09;
    pub const remember_state = 0x0a;
    pub const restore_state = 0x0b;
    pub const def_cfa = 0x0c;
    pub const def_cfa_register = 0x0d;
    pub const def_cfa_offset = 0x0e;

    // DWARF 3.
    pub const def_cfa_expression = 0x0f;
    pub const expression = 0x10;
    pub const offset_extended_sf = 0x11;
    pub const def_cfa_sf = 0x12;
    pub const def_cfa_offset_sf = 0x13;
    pub const val_offset = 0x14;
    pub const val_offset_sf = 0x15;
    pub const val_expression = 0x16;

    pub const lo_user = 0x1c;
    pub const hi_user = 0x3f;

    // SGI/MIPS specific.
    pub const MIPS_advance_loc8 = 0x1d;

    // GNU extensions.
    pub const GNU_window_save = 0x2d;
    pub const GNU_args_size = 0x2e;
    pub const GNU_negative_offset_extended = 0x2f;
};

pub const CHILDREN = struct {
    pub fn asStr(value: u16) []const u8 {
        return dwarfString(@This(), value);
    }

    pub fn isValid(value: u16) bool {
        return dwarfIsValid(@This(), value);
    }

    pub const no = 0x00;
    pub const yes = 0x01;
};

pub const LNS = struct {
    pub fn asStr(value: u16) []const u8 {
        return dwarfString(@This(), value);
    }

    pub fn isValid(value: u16) bool {
        return dwarfIsValid(@This(), value);
    }

    pub const extended_op = 0x00;
    pub const copy = 0x01;
    pub const advance_pc = 0x02;
    pub const advance_line = 0x03;
    pub const set_file = 0x04;
    pub const set_column = 0x05;
    pub const negate_stmt = 0x06;
    pub const set_basic_block = 0x07;
    pub const const_add_pc = 0x08;
    pub const fixed_advance_pc = 0x09;
    pub const set_prologue_end = 0x0a;
    pub const set_epilogue_begin = 0x0b;
    pub const set_isa = 0x0c;
};

pub const LNE = struct {
    pub fn asStr(value: u16) []const u8 {
        return dwarfString(@This(), value);
    }

    pub fn isValid(value: u16) bool {
        return dwarfIsValid(@This(), value);
    }

    pub const end_sequence = 0x01;
    pub const set_address = 0x02;
    pub const define_file = 0x03;
    pub const set_discriminator = 0x04;
    pub const lo_user = 0x80;
    pub const hi_user = 0xff;
};

pub const UT = struct {
    pub fn asStr(value: u16) []const u8 {
        return dwarfString(@This(), value);
    }

    pub fn isValid(value: u16) bool {
        return dwarfIsValid(@This(), value);
    }

    pub const compile = 0x01;
    pub const @"type" = 0x02;
    pub const partial = 0x03;
    pub const skeleton = 0x04;
    pub const split_compile = 0x05;
    pub const split_type = 0x06;

    pub const lo_user = 0x80;
    pub const hi_user = 0xff;
};

pub const LNCT = struct {
    pub fn asStr(value: u16) []const u8 {
        return dwarfString(@This(), value);
    }

    pub fn isValid(value: u16) bool {
        return dwarfIsValid(@This(), value);
    }

    pub const path = 0x1;
    pub const directory_index = 0x2;
    pub const timestamp = 0x3;
    pub const size = 0x4;
    pub const MD5 = 0x5;

    pub const lo_user = 0x2000;
    pub const hi_user = 0x3fff;
};

pub const RLE = struct {
    pub fn asStr(value: u16) []const u8 {
        return dwarfString(@This(), value);
    }

    pub fn isValid(value: u16) bool {
        return dwarfIsValid(@This(), value);
    }

    pub const end_of_list = 0x00;
    pub const base_addressx = 0x01;
    pub const startx_endx = 0x02;
    pub const startx_length = 0x03;
    pub const offset_pair = 0x04;
    pub const base_address = 0x05;
    pub const start_end = 0x06;
    pub const start_length = 0x07;
};

pub const CC = struct {
    pub fn asStr(value: u16) []const u8 {
        return dwarfString(@This(), value);
    }

    pub fn isValid(value: u16) bool {
        return dwarfIsValid(@This(), value);
    }

    pub const normal = 0x1;
    pub const program = 0x2;
    pub const nocall = 0x3;

    pub const pass_by_reference = 0x4;
    pub const pass_by_value = 0x5;

    pub const lo_user = 0x40;
    pub const hi_user = 0xff;

    pub const GNU_renesas_sh = 0x40;
    pub const GNU_borland_fastcall_i386 = 0x41;
};

pub const DS = struct {
    pub fn asStr(value: u16) []const u8 {
        return dwarfString(@This(), value);
    }

    pub fn isValid(value: u16) bool {
        return dwarfIsValid(@This(), value);
    }

    pub const unsigned = 0x01;
    pub const leading_overpunch = 0x02;
    pub const trailing_overpunch = 0x03;
    pub const leading_separate = 0x04;
    pub const trailing_separate = 0x05;
};

pub const END = struct {
    pub fn asStr(value: u16) []const u8 {
        return dwarfString(@This(), value);
    }

    pub fn isValid(value: u16) bool {
        return dwarfIsValid(@This(), value);
    }

    pub const default = 0x00;
    pub const big = 0x01;
    pub const little = 0x02;
    pub const lo_user = 0x40;
    pub const hi_user = 0xff;
};

pub fn dwarfString(comptime T: type, value: u16) []const u8 {
    inline for (@typeInfo(T).Struct.decls) |decl| {
        const field = @field(T, decl.name);
        if (@TypeOf(field) != comptime_int) continue;
        if (field == value) return "DW_" ++ @typeName(T) ++ "_" ++ decl.name;
    }
    std.debug.panic("no value 0x{x} in {s}\n", .{ value, "DW_" ++ @typeName(T) });
}

pub fn dwarfIsValid(comptime T: type, value: u16) bool {
    inline for (@typeInfo(T).Struct.decls) |decl| {
        const field = @field(T, decl.name);
        if (@TypeOf(field) != comptime_int) continue;
        if (field == value) return true;
    }
    return false;
}

pub const Class = enum {
    address,
    addrptr,
    block,
    constant,
    exprloc,
    flag,
    lineptr,
    loclist,
    loclistsptr,
    macptr,
    rnglist,
    rnglistsptr,
    reference,
    string,
    stroffsetsptr,

    pub fn fromAttribute(attrib: u16) []const Class {
        if (!dwarfIsValid(AT, attrib)) std.debug.panic("0x{x} is not a valid DW_AT\n", .{attrib});
        return switch (attrib) {
            DW.AT.sibling => &[_]Class{.reference},
            DW.AT.location => &[_]Class{ .exprloc, .loclist },
            DW.AT.name => &[_]Class{.string},
            DW.AT.ordering => &[_]Class{.constant},
            DW.AT.subscr_data => &[_]Class{},
            DW.AT.byte_size => &[_]Class{ .constant, .exprloc, .reference },
            DW.AT.bit_offset => &[_]Class{ .constant, .exprloc, .reference },
            DW.AT.bit_size => &[_]Class{ .constant, .exprloc, .reference },
            DW.AT.element_list => &[_]Class{},
            DW.AT.stmt_list => &[_]Class{.lineptr},
            DW.AT.low_pc => &[_]Class{.address},
            DW.AT.high_pc => &[_]Class{ .address, .constant },
            DW.AT.language => &[_]Class{},
            DW.AT.member => &[_]Class{},
            DW.AT.discr => &[_]Class{.reference},
            DW.AT.discr_value => &[_]Class{.constant},
            DW.AT.visibility => &[_]Class{.constant},
            DW.AT.import => &[_]Class{.reference},
            DW.AT.string_length => &[_]Class{ .exprloc, .loclist, .reference },
            DW.AT.common_reference => &[_]Class{.reference},
            DW.AT.comp_dir => &[_]Class{.string},
            DW.AT.const_value => &[_]Class{ .block, .constant, .string },
            DW.AT.containing_type => &[_]Class{.reference},
            DW.AT.default_value => &[_]Class{ .constant, .reference, .flag },
            DW.AT.@"inline" => &[_]Class{.constant},
            DW.AT.is_optional => &[_]Class{.flag},
            DW.AT.lower_bound => &[_]Class{ .constant, .exprloc, .reference },
            DW.AT.producer => &[_]Class{.string},
            DW.AT.prototyped => &[_]Class{.flag},
            DW.AT.return_addr => &[_]Class{ .exprloc, .loclist },
            DW.AT.start_scope => &[_]Class{ .constant, .rnglist },
            DW.AT.bit_stride => &[_]Class{ .constant, .exprloc, .reference },
            DW.AT.upper_bound => &[_]Class{ .constant, .exprloc, .reference },
            DW.AT.abstract_origin => &[_]Class{.reference},
            DW.AT.accessibility => &[_]Class{.constant},
            DW.AT.address_class => &[_]Class{.constant},
            DW.AT.artificial => &[_]Class{.flag},
            DW.AT.base_types => &[_]Class{.reference},
            DW.AT.calling_convention => &[_]Class{.constant},
            DW.AT.count => &[_]Class{ .constant, .exprloc, .reference },
            DW.AT.data_member_location => &[_]Class{ .constant, .exprloc, .reference },
            DW.AT.decl_column => &[_]Class{.constant},
            DW.AT.decl_file => &[_]Class{.constant},
            DW.AT.decl_line => &[_]Class{.constant},
            DW.AT.declaration => &[_]Class{.flag},
            DW.AT.discr_list => &[_]Class{.block},
            DW.AT.encoding => &[_]Class{.constant},
            DW.AT.external => &[_]Class{.flag},
            DW.AT.frame_base => &[_]Class{ .exprloc, .loclist },
            DW.AT.friend => &[_]Class{.reference},
            DW.AT.identifier_case => &[_]Class{.constant},
            DW.AT.macro_info => &[_]Class{.macptr},
            DW.AT.namelist_items => &[_]Class{.reference},
            DW.AT.priority => &[_]Class{.reference},
            DW.AT.segment => &[_]Class{ .exprloc, .loclist },
            DW.AT.specification => &[_]Class{.reference},
            DW.AT.static_link => &[_]Class{ .exprloc, .loclist },
            DW.AT.@"type" => &[_]Class{.reference},
            DW.AT.use_location => &[_]Class{ .exprloc, .loclist },
            DW.AT.variable_parameter => &[_]Class{.flag},
            DW.AT.virtuality => &[_]Class{.constant},
            DW.AT.vtable_elem_location => &[_]Class{ .exprloc, .loclist },
            DW.AT.allocated => &[_]Class{ .constant, .exprloc, .reference },
            DW.AT.associated => &[_]Class{ .constant, .exprloc, .reference },
            DW.AT.data_location => &[_]Class{.exprloc},
            DW.AT.byte_stride => &[_]Class{ .constant, .exprloc, .reference },
            DW.AT.entry_pc => &[_]Class{ .address, .constant },
            DW.AT.use_UTF8 => &[_]Class{.flag},
            DW.AT.extension => &[_]Class{.reference},
            DW.AT.ranges => &[_]Class{.rnglist},
            DW.AT.trampoline => &[_]Class{ .address, .flag, .reference, .string },
            DW.AT.call_column => &[_]Class{.constant},
            DW.AT.call_file => &[_]Class{.constant},
            DW.AT.call_line => &[_]Class{.constant},
            DW.AT.description => &[_]Class{.string},
            DW.AT.binary_scale => &[_]Class{.constant},
            DW.AT.decimal_scale => &[_]Class{.constant},
            DW.AT.small => &[_]Class{.reference},
            DW.AT.decimal_sign => &[_]Class{.constant},
            DW.AT.digit_count => &[_]Class{.constant},
            DW.AT.picture_string => &[_]Class{.string},
            DW.AT.mutable => &[_]Class{.flag},
            DW.AT.threads_scaled => &[_]Class{.flag},
            DW.AT.explicit => &[_]Class{.flag},
            DW.AT.object_pointer => &[_]Class{.reference},
            DW.AT.endianity => &[_]Class{.constant},
            DW.AT.elemental => &[_]Class{.flag},
            DW.AT.pure => &[_]Class{.flag},
            DW.AT.recursive => &[_]Class{.flag},
            DW.AT.signature => &[_]Class{.reference},
            DW.AT.main_subprogram => &[_]Class{.flag},
            DW.AT.data_bit_offset => &[_]Class{.constant},
            DW.AT.const_expr => &[_]Class{.flag},
            DW.AT.enum_class => &[_]Class{.flag},
            DW.AT.linkage_name => &[_]Class{.string},
            DW.AT.string_length_bit_size => &[_]Class{.constant},
            DW.AT.string_length_byte_size => &[_]Class{.constant},
            DW.AT.rank => &[_]Class{ .constant, .exprloc },
            DW.AT.str_offsets_base => &[_]Class{.stroffsetsptr},
            DW.AT.addr_base => &[_]Class{.addrptr},
            DW.AT.rnglists_base => &[_]Class{.rnglistsptr},
            DW.AT.dwo_name => &[_]Class{.string},
            DW.AT.reference => &[_]Class{.flag},
            DW.AT.rvalue_reference => &[_]Class{.flag},
            DW.AT.macros => &[_]Class{.macptr},
            DW.AT.call_all_calls => &[_]Class{.flag},
            DW.AT.call_all_source_calls => &[_]Class{.flag},
            DW.AT.call_all_tail_calls => &[_]Class{.flag},
            DW.AT.call_return_pc => &[_]Class{.address},
            DW.AT.call_value => &[_]Class{.exprloc},
            DW.AT.call_origin => &[_]Class{.exprloc},
            DW.AT.call_parameter => &[_]Class{.reference},
            DW.AT.call_pc => &[_]Class{.address},
            DW.AT.call_tail_call => &[_]Class{.flag},
            DW.AT.call_target => &[_]Class{.exprloc},
            DW.AT.call_target_clobbered => &[_]Class{.exprloc},
            DW.AT.call_data_location => &[_]Class{.exprloc},
            DW.AT.call_data_value => &[_]Class{.exprloc},
            DW.AT.@"noreturn" => &[_]Class{.flag},
            DW.AT.alignment => &[_]Class{.constant},
            DW.AT.export_symbols => &[_]Class{.flag},
            DW.AT.deleted => &[_]Class{.flag},
            DW.AT.defaulted => &[_]Class{.constant},
            DW.AT.loclists_base => &[_]Class{.loclistsptr},

            else => std.debug.panic("unknown AT=0x{x}\n", .{attrib}),
        };
    }

    pub fn fromForm(form: u16) Class {
        if (!dwarfIsValid(FORM, form)) std.debug.panic("0x{x} is not a valid DW_FORM\n", .{form});
        return switch (form) {
            DW.FORM.addr,
            DW.FORM.addrx,
            DW.FORM.addrx1,
            DW.FORM.addrx2,
            DW.FORM.addrx3,
            DW.FORM.addrx4,
            => .address,

            DW.FORM.block,
            DW.FORM.block1,
            DW.FORM.block2,
            DW.FORM.block4,
            => .block,

            DW.FORM.data1,
            DW.FORM.data2,
            DW.FORM.data4,
            DW.FORM.data8,
            DW.FORM.data16,
            DW.FORM.sdata,
            DW.FORM.udata,
            DW.FORM.implicit_const,
            => .constant,

            DW.FORM.exprloc => .exprloc,

            DW.FORM.flag,
            DW.FORM.flag_present,
            => .flag,

            DW.FORM.loclistx => .loclist,

            DW.FORM.rnglistx => .rnglist,

            DW.FORM.ref_addr,
            DW.FORM.ref1,
            DW.FORM.ref2,
            DW.FORM.ref4,
            DW.FORM.ref8,
            DW.FORM.ref_udata,
            DW.FORM.ref_sup4,
            DW.FORM.ref_sup8,
            DW.FORM.ref_sig8,
            => .reference,

            DW.FORM.string,
            DW.FORM.strp,
            DW.FORM.strx,
            DW.FORM.strp_sup,
            DW.FORM.line_strp,
            DW.FORM.strx1,
            DW.FORM.strx2,
            DW.FORM.strx3,
            DW.FORM.strx4,
            => .string,

            DW.FORM.indirect,
            DW.FORM.sec_offset,
            => std.debug.panic("{s} doesn't have a specific class\n", .{DW.FORM.asStr(form)}),

            // these are all represented by DW.FORM.sec_offset
            //=> .addrptr,
            //=> .lineptr,
            //=> .loclistptr,
            //=> .macptr,
            //=> .rnglistptr,
            //=> .stroffsetptr,

            else => std.debug.panic("unknown FORM=0x{x}\n", .{form}),
        };
    }
};
