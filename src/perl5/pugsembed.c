#include "pugsembed.h"
extern int __init;

Val *
pugs_SvToVal ( SV *sv )
{
    svtype ty = SvTYPE(sv);

    if (sv_isa(sv, "pugs")) {
        IV tmp = SvIV((SV*)SvRV(sv));
        return ((Val *)tmp);
    }
    else if (SvROK(sv)) {
        return pugs_MkSvRef(sv);
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
        /* XXX - This is wrong; byte buffers in Perl5 land will be autoupgraded via Latin1!
         *       A better way, once we have a native Buf type in Pugs, is to check SvUTF8
         *       and convert to Buf if it's off.
         */
        STRLEN len = sv_len(sv);
        return pugs_PvnToVal(SvPV_nolen(sv), (int)len);
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

    if (!__init) {
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
    if (rv == NULL) {
        Perl_croak(aTHX_ "PugsEnv uninitialized; please call pugs_setenv() first. (hate software so much.)");
    }
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
