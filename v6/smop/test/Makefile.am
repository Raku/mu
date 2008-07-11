.sm0p.c: $(top_srcdir)/../../misc/sm0p/sm0p5.pm
	perl $(top_srcdir)/sm0p.pl $(top_srcdir) $< $@

LDADD = -lsmop 
AM_LDFLAGS = -L$(top_builddir)/src
AM_CFLAGS = -Wall -D_GNU_SOURCE -I$(top_srcdir)/include

check_PROGRAMS = 01_smop_lowlevel.t 02_stack.t 03_const_identifier.t \
	04_interpreter.t 05_capture.t 06_native_bool.t 07_native_int.t \
	08_native_uint.t 09_threads.t 10_lowlevel_methods.t 11_lowlevel_scalar.t \
	12_p6opaque.t 13_io.t 14_p6opaque_methods.t

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
13_io_t_SOURCES = 13_io.sm0p
14_p6opaque_methods_t_SOURCES = 14_p6opaque_methods.sm0p