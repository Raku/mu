bin_PROGRAMS = yap6
yap6_SOURCES = const.c ident_dispatcher.c yap6.c yap6.h memory.c
yap6_LDADD = -lpthread
AM_CFLAGS = -Wall

check_PROGRAMS = 01_apply.t 02_refcnt.t 03_refcnt_thread_trick.t
01_apply_t_SOURCES = t_01_apply.c const.c ident_dispatcher.c yap6.h memory.c
01_apply_t_LDADD = -lpthread
02_refcnt_t_SOURCES = t_02_refcnt.c const.c ident_dispatcher.c yap6.h memory.c
02_refcnt_t_LDADD = -lpthread
03_refcnt_thread_trick_t_SOURCES = t_03_refcnt_thread_trick.c const.c ident_dispatcher.c yap6.h memory.c
03_refcnt_thread_trick_t_LDADD = -lpthread