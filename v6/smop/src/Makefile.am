AM_CFLAGS = -Wall -D_GNU_SOURCE -DSMOP_LOWLEVEL_MEM_TRACE -I$(top_srcdir)/include
# use this to see alloc, refcnt_inc, refcnt_dec and destroys...

lib_LTLIBRARIES = libsmop.la
libsmop_la_LIBADD = -lpthread
libsmop_la_SOURCES = smop.c idconst.c smop_lowlevel.c
