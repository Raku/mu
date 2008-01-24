AM_CFLAGS = -Wall -D_GNU_SOURCE -DSMOP_LOWLEVEL_MEM_TRACE -I$(top_srcdir)/include
# use this to see alloc, refcnt_inc, refcnt_dec and destroys...

lib_LTLIBRARIES = libsmop.la
libsmop_la_LIBADD = -lpthread
libsmop_la_SOURCES = idconst.c idconst_decl_all.h idconst_destr_all.c \
	idconst_init_all.c native_bool.c native_capture.c native_int.c \
	smop.c smop_internal.h smop_interpreter.c smop_lowlevel.c \
    smop_slime_capturize.c smop_slime_currentframe.c smop_slime_frame.c \
    smop_slime_node.c

