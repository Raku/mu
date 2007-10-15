bin_PROGRAMS = yap6
yap6_SOURCES = const.c int_dispatcher.c ident_dispatcher.c yap6.c yap6.h memory.c main.c
yap6_LDADD = -lpthread
AM_CFLAGS = -Wall

check_PROGRAMS = 01_apply.t 02_refcnt.t 03_refcnt_thread_trick.t 05_int.t
01_apply_t_SOURCES = t_01_apply.c const.c int_dispatcher.c ident_dispatcher.c yap6.h memory.c yap6.c
01_apply_t_LDADD = -lpthread
02_refcnt_t_SOURCES = t_02_refcnt.c const.c int_dispatcher.c ident_dispatcher.c yap6.h memory.c yap6.c
02_refcnt_t_LDADD = -lpthread
03_refcnt_thread_trick_t_SOURCES = t_03_refcnt_thread_trick.c const.c int_dispatcher.c ident_dispatcher.c yap6.h memory.c yap6.c
03_refcnt_thread_trick_t_LDADD = -lpthread
05_int_t_SOURCES = t_05_int.c const.c int_dispatcher.c ident_dispatcher.c yap6.h memory.c yap6.c
05_int_t_LDADD = -lpthread