#include <pugsembed.h>
extern int __init;

Val *
pugs_SvToVal ( SV *sv )
{
    if (!sv_isa(sv, "pugs")) {
        return (pugs_MkSvRef(sv));
    }
    IV tmp = SvIV((SV*)SvRV(sv));
    return ((Val *)tmp);
}

SV *
pugs_MkValRef ( Val *val )
{
    SV *sv = newSV(0);
    Val *isa[2];
    SV *stack[8], *type;

    sv_setref_pv(sv, "pugs", val);

    if (!__init) {
	fprintf(stderr, "MkValRef called before perl_init.\n");
    }

    isa[0] = NULL;

    type = pugs_Apply(pugs_PvToVal("&ref"), val, isa, G_SCALAR);
    fprintf(stderr, "query the type: got %s\n", SvPV_nolen(type));
    if (SvTRUE( type )) {
	SV **rv;
	stack[0] = type;
	stack[1] = NULL;
	rv = perl5_apply(newSVpv("can", 0), newSVpv("pugs::guts", 0), stack, NULL, G_SCALAR);
	if (SvTRUE( rv[0] )) {
	    stack[0] = sv;
	    rv = perl5_apply(type, newSVpv("pugs::guts", 0), stack, NULL, G_SCALAR);
	    sv = rv[0];
	}
	else {
	    fprintf(stderr, "unknown type\n");
	}
    }

    return (sv);
}

Val *pugs_getenv ()
{
    SV** rv = hv_fetch(PL_modglobal, "PugsEnv", 7, 0);
    if (rv == NULL)
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
