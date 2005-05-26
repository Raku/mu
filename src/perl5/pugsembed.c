#include <pugsembed.h>

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
    sv_setref_pv(sv, "pugs", val);
    return (sv);
}

Val *pugs_Eval ( char *code ) { return NULL; }
Val *pugs_Apply ( Val *sub, Val *inv, Val **args ) { return NULL; }
