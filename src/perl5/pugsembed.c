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
    fprintf(stderr, "pugs mkvalref: %p\n", val);
    Val *isa[2];
    isa[0] = pugs_PvToVal("Code");
    isa[1] = NULL;
    if (pugs_ValToIv(pugs_Apply("&isa", val, isa))) {
	fprintf(stderr, "got a code!!\n");
    }
    sv_setref_pv(sv, "pugs", val);
    return (sv);
}
