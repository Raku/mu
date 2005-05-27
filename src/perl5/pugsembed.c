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
    SV *stack[8];

    fprintf(stderr, "pugs mkvalref: %p\n", val);
    sv_setref_pv(sv, "pugs", val);

    isa[0] = pugs_PvToVal("Code");
    isa[1] = NULL;
    if (__init && pugs_ValToIv(pugs_Apply(pugs_PvToVal("&isa"), val, isa))) {
	fprintf(stderr, "got a code!!\n");
	stack[0] = newSVpv("pugs::guts", 0);
	stack[1] = sv;
	sv = perl5_call("code", 2, stack, NULL, G_SCALAR);
    }
    return (sv);
}
