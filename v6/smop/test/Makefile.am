.sm0p.c:
	perl $(top_srcdir)/sm0p.pl $< $@

LDADD = -lsmop 
AM_LDFLAGS = -L$(top_builddir)/src
AM_CFLAGS = -Wall -D_GNU_SOURCE -I$(top_srcdir)/include

check_PROGRAMS = 01_smop_lowlevel.t 02_stack.t 03_const_identifier.t \
	04_interpreter.t 05_capture.t 06_native_bool.t 07_int.t

01_smop_lowlevel_t_SOURCES = 01_smop_lowlevel.c
02_stack_t_SOURCES = 02_stack.sm0p
03_const_identifier_t_SOURCES = 03_const_identifier.c
04_interpreter_t_SOURCES = 04_interpreter.c
05_capture_t_SOURCES = 05_capture.c
06_native_bool_t_SOURCES = 06_native_bool.c
07_int_t_SOURCES = 07_int.c