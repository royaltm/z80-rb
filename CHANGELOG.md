v1.1.1.pre-1
============

ZXLib:
-----
* New macro ZXLib::Gfx::Macros#ytoattr.
* Internal labels +preshift_p+, +pmask_in_e+, +px_bsh_in_ae+ added to ZXLib::Gfx::Draw::Macros for convenience and alternative entry points.

v1.1.0
======

* FIXED: Z80::Program#ns with `merge` option enabled was discarding parent label context in debug output.
* More benches and tests.
* Improvement: use Z80::Program#define_label instead of `send` to define dynamic labels in several places.
* Several docs improvements.

Z80:
---
* Z80::Program#debug_comment (Z80::Program#dc!) helper method for including user comments in debug output.
* Z80::Program#alias_label (Z80::Program#as) helper for creating namespace anchored lazy label aliases (visible in debug output).
* Z80::Program#direct_label? and Z80::Program#direct_address? helpers.
* Z80::TAP FIXED - byte block larger than the length indicated in the tap header will be truncated instead of raising an error.
* Z80::TZX recognize and skip common meta chunks.
* Z80::MathInt::Macros#twos_complement16_by_sgn macro - `sgn` argument can be accumulator and the new option `t`.
* Z80::MathInt::Macros#utobcd macro features new `input_end` option.
* Z80::Program::Macros#cp16n optimized comparing with a constant number < 256.
* new sugar macro Z80::Program::Macros#cp16rr.
* Z80::Program#select now allows conditional blocks without any code produced and else-only conditional blocks when only label validation is needed.
* Z80::Utils::Sort::Macros producing selected sorting algorithm routines.

ZXLib:
-----
* ZXLib::Sys new labels: `if1vars` (ZX Interface 1), `vars128` (128k), `io_plus` (ULAplus), `memT2k.rambot`, more ROM routines.
* new macro: ZXLib::Sys::Macros#move_basic_above_scld_screen_memory for safely accessing SCLD shadow screen from BASIC.
* ZXLib::Gfx::Sprite8 `CALCULATE_SCREEN_ADDRESS` and `CHECK_HEIGHT_SANITY` control constants; new sugar ZXLib::Gfx::Sprite8::Macros; `maskshift` label exported and slightly optimized ZXLib::Gfx::Sprite8::Macros#gfx_sprite8_calculate_coords macro.
* ZXLib::Gfx::Macros#clear_screen_region_fast macro - FIXED corner cases, now accepts odd number of columns, optimized for constant arguments, new options: `addr_mode`, `scraddr` and `subroutine`.
* new macros: ZXLib::Gfx::Macros#clear_attrs_region_fast, ZXLib::Gfx::Macros#copy_shadow_screen_region, ZXLib::Gfx::Macros#copy_shadow_attrs_region, ZXLib::Gfx::Macros#copy_shadow_attrs_region_quick and ZXLib::Gfx::Macros#copy_shadow_screen_region_quick.
* Macros ZXLib::Gfx::Macros#nextline, ZXLib::Gfx::Macros#prevline and ZXLib::Gfx::Macros#ytoscr features new `hires` option.
* Macro ZXLib::Gfx::Macros#xytoscr renamed to better reflecting argument order ZXLib::Gfx::Macros#yxtoscr and aliased as old name for backward compatibility.
* New ZXLib::Gfx::Bobs::Macros for drawing and manipulating bitmap objects.
* ZXLib::Basic recognizes SE BASIC instructions when parsing source, and ZXLib::Basic::Program#to_source can list SE BASIC tokens with `se` option enabled.

ZXUtils:
-------
* ZXUtils::Gallery displays various ZX Spectrum screen formats, including SCLD, ULAplus (also HAM256) modes.
* ZXUtils::BigFont::Macros features high resolution mode; ZXUtils::BigFontHires program.

Bins:
----
* `zxgallery`: creates ZXUtils::Gallery `.tap` file from provided `.scr` files.
* `zxconv`: v0.5 - saves hi-res and hi-color screens in a proper ULAplus extended `SCR` format.
* `zxrun`: v0.2 `ZXEMU_PATH` OS environment variable overrides emulator program; arguments passed to the emulator after `--`.
* `zxinit`: v0.3 program template now includes all zxlib modules.

v1.0.0
======

* Initial release.
