#include "pugsembed.h"
extern int _P5EMBED_INIT;

IV
pugs_tied ( SV *sv )
{
    const MAGIC *mg;
    const char how = (SvTYPE(sv) == SVt_PVHV || SvTYPE(sv) == SVt_PVAV)
		? PERL_MAGIC_tied : PERL_MAGIC_tiedscalar;

    if ((mg = SvTIED_mg(sv, how))) {
	SV *osv = SvTIED_obj(sv, mg);
        if (SvROK(osv)) {
            const char *pv = sv_reftype(SvRV(osv),TRUE);
            if (strncmp(pv, "pugs::", 6) == 0) {
                SV *derefSV = newSVpv("DEREF", 0);
                SV **rv;
                SV *stack[0];
                stack[0] = NULL;
                rv = perl5_apply(derefSV, osv, stack, NULL, G_SCALAR);
                if ((rv[0] == NULL) && SvROK(rv[1])) {
                    return SvIV((SV*)SvRV(rv[1]));
                }
            }
        }
    }

    return 0;
}

Val *
pugs_SvToVal ( SV *sv )
{
    svtype ty = SvTYPE(sv);
    IV tmp = 0;

    if (sv_isa(sv, "pugs")) {
        tmp = SvIV((SV*)SvRV(sv));
        return ((Val *)tmp);
    }
    else if (SvROK(sv)) {
        if (tmp = pugs_tied(SvRV(sv))) {
            return ((Val *)tmp);
        }
        else {
            return pugs_MkSvRef(sv);
        }
    }
    else if (ty == SVt_NULL) {
        return pugs_UndefVal();
    }
    else if (SvNIOKp(sv) && (sv_len(sv) != 0)) {
        if (SvNOK(sv)) {
            return pugs_NvToVal(SvNVX(sv));
        }
        else {
            return pugs_IvToVal(SvIVX(sv));
        }
    }
    else if (SvPOKp(sv)) {
        STRLEN len = sv_len(sv);
        if (SvUTF8(sv)) {
            return pugs_PvnToValUTF8(SvPV_nolen(sv), (int)len);
        }
        else {
            return pugs_PvnToVal(SvPV_nolen(sv), (int)len);
        }
    }
    else {
        return pugs_MkSvRef(sv);
    }
}

SV *
pugs_MkValRef ( Val *val, char *typeStr )
{
    SV *sv = newSV(0);
    Val *isa[2];
    SV *stack[8];

    sv_setref_pv(sv, "pugs", val);

    if (!_P5EMBED_INIT) {
        fprintf(stderr, "MkValRef called before perl_init.\n");
    }

    isa[0] = NULL;

    /* fprintf(stderr, "query the type: got %s\n", typeStr); */

    if ((typeStr == NULL) || (*typeStr == '\0')) {
        SV *typeSV = pugs_Apply(pugs_PvnToVal("&WHAT", 5), val, isa, G_SCALAR);
        typeStr = SvPV_nolen(typeSV);
    }

    if ((typeStr != NULL) && (*typeStr != '\0')) {
        SV **rv;
        SV *typeSV = newSVpv(typeStr, 0);
        stack[0] = typeSV;
        stack[1] = NULL;
        rv = perl5_apply(newSVpv("can", 0), newSVpv("pugs::guts", 0), stack, NULL, G_SCALAR);
        if ((rv[0] == NULL) && SvTRUE( rv[1] )) {
            stack[0] = sv;
            rv = perl5_apply(typeSV, newSVpv("pugs::guts", 0), stack, NULL, G_SCALAR);
            if (rv[0] == NULL) {
                /* no error happened -- used the tied obj */
                sv = rv[1];
            }
            else {
                fprintf(stderr, "error in pugs::guts application on type: %s\n", typeStr);
                sv_dump(rv[0]);
            }
        }
        else {
            /* for scalar ref, should still turn into tied one */
#if PERL5_EMBED_DEBUG
            fprintf(stderr, "unknown type\n");
#endif
        }
    }

    return (sv);
}

Val *pugs_getenv ()
{
    SV** rv = hv_fetch(PL_modglobal, "PugsEnv", 7, 0);
    IV tmp;
    if (rv == NULL) {
        Perl_croak(aTHX_ "PugsEnv uninitialized; please call pugs_setenv() first. (hate software so much.)");
    }
    tmp = SvIV((SV*)SvRV(*rv));
    return ((Val *)tmp);
}

void pugs_setenv ( Val *env )
{
    SV *sv;
    if (env == NULL) { return; }

    sv = newSV(0);
    sv_setref_pv(sv, "pugs", env);
    hv_store(PL_modglobal, "PugsEnv", 7, sv, 0);
}
