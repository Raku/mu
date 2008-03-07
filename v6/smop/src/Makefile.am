.sm0p.c:
	perl $(top_srcdir)/sm0p.pl $(top_srcdir) $< $@

AM_CFLAGS = -Wall -D_GNU_SOURCE -DSMOP_LOWLEVEL_MEM_TRACE -I$(top_srcdir)/include
AM_LDFLAGS = --no-undefined
# use this to see alloc, refcnt_inc, refcnt_dec and destroys...   -DSMOP_LOWLEVEL_MEM_DEBUG 

lib_LTLIBRARIES = libsmop.la
libsmop_la_LIBADD = -lpthread
libsmop_la_SOURCES = smop_lowlevel.sm0p idconst.c						\
	native_bool.c native_capture.c native_int.c	native_uint.c			\
	smop.c smop_internal.h smop_interpreter.c							\
    smop_slime_capturize.c smop_slime_currentframe.c smop_slime_frame.c	\
    smop_slime_node.c p6opaque.c lowlevel_method.c smop_s1p_scalar.c

