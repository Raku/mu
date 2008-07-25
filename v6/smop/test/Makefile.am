ELFX = $(top_srcdir)/../../misc/elfish/elfX/
.sm0p.c:
	perl $(top_srcdir)/sm0p.pl $(top_srcdir) $< $@
.p6.pure_sm0p:
	perl $(ELFX)/elfX -C sm0p -s $< > $@
.pure_sm0p.sm0p:
	cat $(ELFX)/sm0p_boilerplate_front $< $(ELFX)/sm0p_boilerplate_back > $@

LDADD = -lsmop 
AM_LDFLAGS = -L$(top_builddir)/src
AM_CFLAGS = -Wall -D_GNU_SOURCE -I$(top_srcdir)/include

check_PROGRAMS = 01_smop_lowlevel.t 02_stack.t 03_const_identifier.t \
	04_interpreter.t 05_capture.t 06_native_bool.t 07_native_int.t \
	08_native_uint.t 09_threads.t 10_lowlevel_methods.t 11_lowlevel_scalar.t \
	12_p6opaque.t 14_p6opaque_methods.t \
	15_hash.t 16_labels.t 17_sm0p.t 18_root_namespace.t 19_array.t \
	dispatch_benchmark

01_smop_lowlevel_t_SOURCES = 01_smop_lowlevel.c
02_stack_t_SOURCES = 02_stack.sm0p
03_const_identifier_t_SOURCES = 03_const_identifier.c
04_interpreter_t_SOURCES = 04_interpreter.c
05_capture_t_SOURCES = 05_capture.c
06_native_bool_t_SOURCES = 06_native_bool.c
07_native_int_t_SOURCES = 07_native_int.c
08_native_uint_t_SOURCES = 08_native_uint.c
09_threads_t_SOURCES = 09_threads.sm0p
10_lowlevel_methods_t_SOURCES = 10_lowlevel_methods.c
11_lowlevel_scalar_t_SOURCES = 11_lowlevel_scalar.sm0p
12_p6opaque_t_SOURCES = 12_p6opaque.sm0p
#13_io_t_SOURCES = 13_io.p6
14_p6opaque_methods_t_SOURCES = 14_p6opaque_methods.sm0p
15_hash_t_SOURCES = 15_hash.sm0p
16_labels_t_SOURCES = 16_labels.sm0p
17_sm0p_t_SOURCES = 17_sm0p.sm0p
18_root_namespace_t_SOURCES = 18_root_namespace.sm0p
19_array_t_SOURCES = 19_array.p6
dispatch_benchmark_SOURCES = dispatch_benchmark.sm0p