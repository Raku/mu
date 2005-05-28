#include <perl5.h>
#include <XSUB.h>
#include "perlxsi.c"
#include "pugsembed.c"

/* Workaround for mapstart: the only op which needs a different ppaddr */
#undef Perl_pp_mapstart
#define Perl_pp_mapstart Perl_pp_grepstart
#undef OP_MAPSTART
#define OP_MAPSTART OP_GREPSTART

static PerlInterpreter *my_perl;

int __init = 0;

const char pugs_guts_code[] =
"use strict;"
"package pugs;"

"our $AUTOLOAD;"
"sub AUTOLOAD { pugs::guts::invoke($AUTOLOAD, @_) } "
"sub DESTROY {}"
"package pugs::guts;"
"our @ISA=('pugs');"
"sub code { my ($class, $val) = @_;"
"          sub { pugs::guts::invoke($val, undef, @_) } }"
"1;";

XS(_pugs_guts_invoke) {
    Val *val, *inv, **stack;
    SV *ret, *sv;
    int i;
    dXSARGS;
    if (items < 1)
        Perl_croak(aTHX_ "hate software");

    sv = ST(0);
    if (sv_isa(sv, "pugs")) {
	val = pugs_SvToVal(ST(0));
    }
    else {
	char *method, *fullname;
	fullname = SvPV_nolen(sv);
	method = strrchr(fullname, ':');
	method = method ? method+1 : fullname;
	val = pugs_PvToVal(method);
    }
    inv = SvOK(ST(1)) ? pugs_SvToVal(ST(1)) : NULL;

    stack = (Val **)malloc(sizeof(Val*)*items-1);
    for (i = 2; i < items; ++i) {
	stack[i-2] = pugs_SvToVal(ST(i));
    }
    stack[i-2] = NULL;
    
    ST(0) = pugs_Apply(val, inv, stack, GIMME_V);
    /* sv_dump (ret); */
    free (stack);
    
    XSRETURN(1);
}


#ifdef HAS_PROCSELFEXE
/* This is a function so that we don't hold on to MAXPATHLEN
   bytes of stack longer than necessary
 */
STATIC void
S_procself_val(pTHX_ SV *sv, char *arg0)
{
    char buf[MAXPATHLEN];
    int len = readlink(PROCSELFEXE_PATH, buf, sizeof(buf) - 1);

    /* On Playstation2 Linux V1.0 (kernel 2.2.1) readlink(/proc/self/exe)
       includes a spurious NUL which will cause $^X to fail in system
       or backticks (this will prevent extensions from being built and
       many tests from working). readlink is not meant to add a NUL.
       Normal readlink works fine.
     */
    if (len > 0 && buf[len-1] == '\0') {
      len--;
    }

    /* FreeBSD's implementation is acknowledged to be imperfect, sometimes
       returning the text "unknown" from the readlink rather than the path
       to the executable (or returning an error from the readlink).  Any valid
       path has a '/' in it somewhere, so use that to validate the result.
       See http://www.freebsd.org/cgi/query-pr.cgi?pr=35703
    */
    if (len > 0 && memchr(buf, '/', len)) {
        sv_setpvn(sv,buf,len);
    }
    else {
        sv_setpv(sv,arg0);
    }
}
#endif /* HAS_PROCSELFEXE */

PerlInterpreter *
perl5_init ( int argc, char **argv )
{
    int exitstatus;
    int i;

#ifdef PERL_GPROF_MONCONTROL
    PERL_GPROF_MONCONTROL(0);
#endif
#ifdef PERL_SYS_INIT3
    PERL_SYS_INIT3(&argc,&argv,&env);
#endif

#if (defined(USE_5005THREADS) || defined(USE_ITHREADS)) && defined(HAS_PTHREAD_ATFORK)
    /* XXX Ideally, this should really be happening in perl_alloc() or
     * perl_construct() to keep libperl.a transparently fork()-safe.
     * It is currently done here only because Apache/mod_perl have
     * problems due to lack of a call to cancel pthread_atfork()
     * handlers when shared objects that contain the handlers may
     * be dlclose()d.  This forces applications that embed perl to
     * call PTHREAD_ATFORK() explicitly, but if and only if it hasn't
     * been called at least once before in the current process.
     * --GSAR 2001-07-20 */
    PTHREAD_ATFORK(Perl_atfork_lock,
                   Perl_atfork_unlock,
                   Perl_atfork_unlock);
#endif

    if (!PL_do_undump) {
        my_perl = perl_alloc();
        if (!my_perl)
            exit(1);
        perl_construct( my_perl );
        PL_perl_destruct_level = 0;
    }
#ifdef PERL_EXIT_DESTRUCT_END
    PL_exit_flags |= PERL_EXIT_DESTRUCT_END;
#endif /* PERL_EXIT_DESTRUCT_END */
#ifdef PERL_EXIT_EXPECTED
    PL_exit_flags |= PERL_EXIT_EXPECTED;
#endif /* PERL_EXIT_EXPECTED */

#if (defined(CSH) && defined(PL_cshname))
    if (!PL_cshlen)
      PL_cshlen = strlen(PL_cshname);
#endif

    exitstatus = perl_parse(my_perl, xs_init, argc, argv, (char **)NULL);

    if (exitstatus == 0)
	exitstatus = perl_run( my_perl );

    __init = 1;

    newXS((char*) "pugs::guts::invoke", _pugs_guts_invoke, (char*)__FILE__);

    eval_pv(pugs_guts_code, TRUE);

    if (SvTRUE(ERRSV)) {
        STRLEN n_a;
        printf("Error init perl: %s\n", SvPV(ERRSV,n_a));
        exit(1);
    }
    return my_perl;
}

char *
perl5_SvPV ( SV *sv )
{
    char *rv;
    rv = SvPV_nolen(sv);
    return rv;
}

int
perl5_SvIV ( SV *sv )
{
    return((int)SvIV(sv));
}

double
perl5_SvNV ( SV *sv )
{
    return((double)SvNV(sv));
}

bool
perl5_SvTRUE ( SV * sv )
{
    bool rv;
    rv = SvTRUE(sv);
    return rv;
}

SV *
perl5_newSVpv ( char * pv )
{
    return(newSVpv(pv, 0));
}

SV *
perl5_newSViv ( int iv )
{
    return(newSViv(iv));
}

SV *
perl5_newSVnv ( double iv )
{
    return(newSVnv(iv));
}

SV *
perl5_apply(SV *sub, SV *inv, SV** args, void *env, int cxt)
{
    SV **arg;
    SV *rv;
    SV *sv;
    void *old_env = pugs_getenv();

    dSP;

    ENTER;
    SAVETMPS;

    pugs_setenv(env);

    PUSHMARK(SP);
    if (inv != NULL) {
        XPUSHs(inv);
    }
    for (arg = args; *arg != NULL; arg++) {
        XPUSHs(*arg);
    }
    PUTBACK;

    if (inv != NULL) {
        call_method(SvPV_nolen(sub), cxt);
    }
    else {
        call_sv(sub, cxt);
    }

    SPAGAIN;

    rv = newSVsv(POPs);

    PUTBACK;
    FREETMPS;
    LEAVE;

    pugs_setenv(old_env);
    return rv;
}

SV *
perl5_get_sv(const char *name)
{
    SV *sv = get_sv(name, 1);
    /* sv_dump(sv); */
    return sv;
}

SV *
perl5_eval(char *code, void *env, int cxt)
{
    dSP;
    SV* sv;
    void *old_env = pugs_getenv();

    ENTER;
    SAVETMPS;

    pugs_setenv(env);

    sv = newSVpv(code, 0);
    eval_sv(sv, cxt);
    SvREFCNT_dec(sv);

    SPAGAIN;
    sv = POPs;
    SvREFCNT_inc(sv);
    PUTBACK;

    if (SvTRUE(ERRSV)) {
        STRLEN n_a;
        fprintf(stderr, "Error eval perl5: \"%s\"\n*** %s\n", code, SvPV(ERRSV,n_a));
    }

    FREETMPS;
    LEAVE;

    pugs_setenv(old_env);
    return sv;
}

bool
perl5_can(SV *inv, char *subname)
{
    int rv;

    dSP;

    ENTER;
    SAVETMPS;

    PUSHMARK(SP);
    XPUSHs(inv);
    XPUSHs(newSVpv(subname, 0));
    PUTBACK;

    call_pv("UNIVERSAL::can", G_SCALAR);

    SPAGAIN;

    rv = POPi;
    /* printf("Checking: %s->can(%s), ret %d\n", SvPV_nolen(inv), subname, rv); */

    PUTBACK;
    FREETMPS;
    LEAVE;

    return rv;
}

void perl5_finalize ( SV* sv )
{
    SvREFCNT_dec(sv);
}
