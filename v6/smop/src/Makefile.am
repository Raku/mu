LDADD = -lpthread
AM_CFLAGS = -Wall -D_GNU_SOURCE -DVROOM_MEM_TRACE
# use this to see alloc, refcnt_inc, refcnt_dec and destroys...

SOURCEFILES = const.c int_dispatcher.c ident_dispatcher.c scalar_dispatcher.c list_dispatcher.c \
	 pair_dispatcher.c bytes_dispatcher.c vroom.c vroom.h memory.c 

bin_PROGRAMS = vroom
vroom_SOURCES = $(SOURCEFILES) main.c

check_PROGRAMS = 01_apply.t 02_refcnt.t 03_refcnt_thread_trick.t 04_list.t 05_int.t 06_scalar.t 07_pair.t \
	08_bytes.t 09_int_which.t
01_apply_t_SOURCES = t_01_apply.c $(SOURCEFILES)
02_refcnt_t_SOURCES = t_02_refcnt.c $(SOURCEFILES)
03_refcnt_thread_trick_t_SOURCES = t_03_refcnt_thread_trick.c $(SOURCEFILES)
04_list_t_SOURCES = t_04_list.c $(SOURCEFILES)
05_int_t_SOURCES = t_05_int.c $(SOURCEFILES)
06_scalar_t_SOURCES = t_06_scalar.c $(SOURCEFILES)
07_pair_t_SOURCES = t_07_pair.c $(SOURCEFILES)
08_bytes_t_SOURCES = t_08_bytes.c $(SOURCEFILES)
09_int_which_t_SOURCES = t_09_int_which.c $(SOURCEFILES)
