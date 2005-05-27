#include <pugsembed.h>
extern int __init;

Val *
pugs_SvToVal ( SV *sv )
{
    if (!sv_isa(sv, "pugs")) {
        fprintf(stderr, "bad cast\n");
        sv_dump(sv);
        return (pugs_MkSvRef(sv));
    }
    fprintf(stderr, "good cast\n");
    IV tmp = SvIV((SV*)SvRV(sv));
    return ((Val *)tmp);
}

SV *
pugs_MkValRef ( Val *val )
{
    SV *sv = newSV(0);
    Val *isa[2];
    SV *stack[8];

    sv_setref_pv(sv, "pugs", val);

    isa[0] = pugs_PvToVal("Code");
    isa[1] = NULL;
    if (__init && pugs_ValToIv(pugs_Apply(pugs_PvToVal("&isa"), val, isa))) {
	stack[0] = sv;
	stack[1] = NULL;
	sv = perl5_apply(newSVpv("code", 0), newSVpv("pugs::guts", 0), stack, NULL, G_SCALAR);
    }
    return (sv);
}

Val *pugs_getenv ()
{
    SV** rv = hv_fetch(PL_modglobal, "PugsEnv", 7, 0);
    if (*rv == NULL)
        Perl_croak(aTHX_ "hate software so much");
    IV tmp = SvIV((SV*)SvRV(*rv));
    return ((Val *)tmp);
}

void pugs_setenv ( Val *env )
{
    if (env == NULL) { return; }

    SV *sv = newSV(0);
    sv_setref_pv(sv, "pugs", env);
    hv_store(PL_modglobal, "PugsEnv", 7, sv, 0);
}
