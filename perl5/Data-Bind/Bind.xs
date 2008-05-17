#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

static int
alias_mg_get(pTHX_ SV *sv, MAGIC *mg)
{
    SV *const target = mg->mg_obj;
    assert (target);

    if (SvROK(sv)) {
	sv_unref_flags(sv, 0);
    }
    SvOK_off(sv);
    if (SvTYPE(sv) >= SVt_PV && SvLEN(sv)) {
	Safefree(SvPVX(sv));
	SvLEN(sv) = 0;
	SvPVX(sv) = NULL;
    }		 
	
    SvGETMAGIC(target);
    if (SvROK(target)) {
	SvROK_on(sv);
	SvRV(sv) = SvREFCNT_inc(SvRV(target));
	/* Won't yet cope with getting blessing or overloading.
	   Worse still, there is no way to catch a bless or overload on the
	   target and send it back, or to ensure that the alias keeps reading
	   the class from the target, rather than having a stale local cache of
	   it.  */
    } else {
	if (SvPOKp(target)) {
	    SvPVX(sv) = SvPVX(target);
	    SvCUR(sv) = SvCUR(target);
	    /* SvLEN remains 0, meaning that we don't own the buffer or free
	       it. The assumption is that as we're magic our caller musn't
	       rely on our buffer existing for long.
	       Oh, and they are evil and wrong if they assume that they can
	       write to it directly, because they will come a cropper on any
	       other magic scalar.  */
	    SvPOKp_on(sv);
	}
	if (SvIOKp(target)) {
	    SvIVX(sv) = SvIVX(target);
	    SvIOKp_on(sv);
	    if (SvIsUV(target))
		SvIsUV_on(sv);
	}
	if (SvNOKp(target)) {
	    SvNVX(sv) = SvNVX(target);
	    SvNOKp_on(sv);
	}
    }
    return 0;
}

static int
alias_mg_set(pTHX_ SV *sv, MAGIC *mg)
{
    SV *const target = mg->mg_obj;
    assert (target);

    sv_force_normal(target);
    SvOK_off(target);
    if (SvROK(sv)) {
	if (SvTYPE(target) >= SVt_PV && SvLEN(target)) {
	    Safefree(SvPVX(target));
	    SvLEN(target) = 0;
	}
	SvROK_on(target);
	SvRV(target) = SvREFCNT_inc(SvRV(sv));
    } else {
	if (SvPOKp(sv)) {
	    SvGROW(target, SvCUR(sv) + 1);
	    Copy(SvPVX(sv), SvPVX(target), SvCUR(sv) + 1, char);
	    SvCUR(target) = SvCUR(sv);
	    SvPOKp_on(target);
	}
	if (SvIOKp(sv)) {
	    SvIVX(target) = SvIVX(sv);
	    SvIOKp_on(target);
	    if (!SvPOKp(sv) && !SvNOKp(sv)) {
		/* This is really getting too fragile. 5.8.0 onwards has
		   arguably-a-bug in save_magic. This line:

		   SvFLAGS(sv) |= (SvFLAGS(sv) & (SVp_NOK|SVp_POK)) >> PRIVSHIFT;

		   should probably read

		   SvFLAGS(sv) |= (SvFLAGS(sv) & (SVp_IOK|SVp_NOK|SVp_POK)) >> PRIVSHIFT;

		   The upshot is that scalars passed in with just SVp_IOK
		   aren't getting acknowledged as having any defined value.

		   The whole private/not flags business is already rather too
		   fragile, as 5.8.0 onwards assign different meaning to
		   private/not private for regular scalars and for magical
		   scalars, both meanings requireing paired bits. And now we're
		   trying to proxy values across from one sort (magic) to the
		   other sort (magic), which (I think) means we're trying to
		   squeeze two local bits of information on one side into one
		   at the other.  */
		SvIOK_on(target);
	    }
	    if (SvIsUV(sv))
		SvIsUV_on(target);
	}
	if (SvNOKp(sv)) {
	    SvNVX(target) = SvNVX(sv);
	    SvNOKp_on(target);
	}
    }
    SvSETMAGIC(target);
}

static U32
alias_mg_len(pTHX_ SV *sv, MAGIC *mg)
{
    return sv_len(mg->mg_obj);
}

/* Not sure if the last few need to become conditionally compiled based on
   perl version  */
MGVTBL alias_vtbl = {
 alias_mg_get,		/* get */
 alias_mg_set,		/* set */
 alias_mg_len,		/* len */
 0,			/* clear */
 0,			/* free */
 0,			/* copy */
 0			/* dup */
};


typedef SV *SVREF;

void
__alias_a_to_b(SVREF a, SVREF b, int read_only) {
    /* This bit of evil lifted straight from Perl_newSVrv  */
    const U32 refcnt = SvREFCNT(a);
    int is_my = SvPADMY(a);
    svtype type = SvTYPE(b);
    SvREFCNT(a) = 0;
    sv_clear(a);
    SvFLAGS(a) = 0;
    SvPADMY_on(a);
    SvREFCNT(a) = refcnt;

    SvUPGRADE(a, SVt_PVMG);
    assert(SvIVX(a) == 0);
    assert(SvNVX(a) == 0.0);
    assert(SvPVX(a) == NULL);

    /* If a magic is assigned to $a, $a has SVt_MAGIC but no MAGIC attached.
       This breaks the magicext below */
    if ( type == SVt_PVMG && !SvMAGIC(b) ) {
        type = 0;
    }

    /* other mg */
    if ( type == SVt_PVMG && SvMAGIC(b)->mg_virtual != &alias_vtbl) {
        type = 0;
    }

    /* if @array is bound to other thing, binding $array[1] to $x */
    if ( type == SVt_PVLV && SvMAGIC(b) ) {
        type = 0;
    }

    if (type >= SVt_PVMG) {
        switch (type) {
            case SVt_PVHV:
            case SVt_PVAV: {
                SV *tie = newRV_noinc((SV*)newHV());
                HV *stash = gv_stashpv(type == SVt_PVHV ?
                                       "Data::Bind::Hash" : "Data::Bind::Array",
                                       TRUE);
                hv_store((HV*)SvRV(tie), "real", 4, newRV_inc((SV *)b), 0);
                sv_bless(tie, stash);
                SvUPGRADE(a, SVt_PVAV);
                hv_magic((HV*)a, (GV *)tie, PERL_MAGIC_tied);
                break;
            }
            case SVt_PVMG: {
		//		SvMAGIC(a) = SvMAGIC(b);
		sv_magicext(a, SvMAGIC(b)->mg_obj, PERL_MAGIC_ext, &alias_vtbl, 0, 0);
		//		SvREFCNT(SvMAGIC(a)->mg_obj)++;
		mg_get(a);
                break;
	    }
            case SVt_PVLV: {
                break;
	    }
            default:
                croak("don't know what to do yet for %d", type);
        }
    }
    else if (type == SVt_RV && SvAMAGIC(b)) {
	SV *x = sv_newmortal();
	sv_setsv(a, newRV_inc(SvRV(b)));
	SvAMAGIC_on(a);
    }
    else {
        sv_magicext(a, b, PERL_MAGIC_ext, &alias_vtbl, 0, 0);
        mg_get(a);
    }

    if (read_only || SvREADONLY(b)) {
	SvREADONLY_on(a);
    }
}


OP *___bind_pad(pTHX)
{
    dMARK; dAX; dSP; dITEMS;
    int n = PL_op->op_targ;
    int i;
    for (i = 0; i < items; ++i) {
        SAVECLEARSV(PAD_SVl(i+1));
        // rw only for now
        PAD_SVl(i+1) = SvREFCNT_inc(ST(i));
    }
    return NORMAL;
}

/* format:  [ [ order, mode, defaultsub ]... ] */

OP *___bind_pad2(pTHX)
{
    dSP;
    AV *_defargs = GvAV(PL_defgv);
    AV *av = SvRV(cSVOPx_sv(PL_op));
    int i;
    for (i = 0; i <= av_len(av); ++i) {
        SV *current_arg = *av_fetch(_defargs, i, 0);
        SV *entry = *av_fetch(av, i, 0);
        IV order = SvIVX(*av_fetch(SvRV(entry), 0, 0));
        SV *mode = *av_fetch(SvRV(entry), 1, 0); // XXX: should do SvOK
        SV *default_sub = *av_fetch(SvRV(entry), 2, 0);
        SAVECLEARSV(PAD_SVl(order));
        /* XXX: check if order is over items, if so it means it's empty and we should apply default_sub->() */
        if (SvIVX(mode)) {
            PAD_SVl(order) = newSV(0);
            SvSetSV(PAD_SVl(order), SvREFCNT_inc(current_arg));
        }
        else {
            PAD_SVl(order) = SvREFCNT_inc(current_arg);
        }
    }
    RETURN;
}


MODULE = Data::Bind                PACKAGE = Data::Bind

void
OP_bind_pad(flags, n)
    I32 flags
    I32 n
    SV** sparepad = NO_INIT
    OP *o = NO_INIT
    OP *saveop = NO_INIT
    I32 typenum = NO_INIT
    CODE:
        sparepad = PL_curpad;
        saveop = PL_op;
        PL_curpad = AvARRAY(PL_comppad);
        o = newOP(OP_CUSTOM, flags);
        o->op_ppaddr = ___bind_pad;
        o->op_targ = n;
        PL_curpad = sparepad;
        PL_op = saveop;
        ST(0) = sv_newmortal();
        sv_setiv(newSVrv(ST(0), "B::OP"), PTR2IV(o));

void
OP_bind_pad2(flags, spec)
    I32 flags
    SV *spec
    SV** sparepad = NO_INIT
    OP *o = NO_INIT
    OP *saveop = NO_INIT
    I32 typenum = NO_INIT
    CODE:
        sparepad = PL_curpad;
        saveop = PL_op;
        PL_curpad = AvARRAY(PL_comppad);
        o = newSVOP(OP_CONST, flags, SvREFCNT_inc(spec));
        o->op_ppaddr = ___bind_pad2;
        PL_curpad = sparepad;
        PL_op = saveop;
        ST(0) = sv_newmortal();
        sv_setiv(newSVrv(ST(0), "B::OP"), PTR2IV(o));

void
_forget_unlocal(IV howmany)
  CODE:
{
    int lv;
    for(lv=1; lv <= howmany; ++lv) {
        PL_scopestack[PL_scopestack_ix - (lv + 1)] = PL_savestack_ix;
    }
}

void
_av_store(SV *av_ref, I32 key, SV *val)
  CODE:
{
    /* XXX many checks */
    AV *av = (AV *)SvRV(av_ref);
    /* XXX unref the old one in slot? */
    av_store(av, key, SvREFCNT_inc(SvRV(val)));
}

void
_hv_store(SV *hv_ref, const char *key, SV *val)
  CODE:
{
    /* XXX many checks */
    HV *hv = (HV *)SvRV(hv_ref);
    /* XXX unref the old one in slot? */
    hv_store(hv, key, strlen(key), SvREFCNT_inc((SvRV(val))), 0);
}

void
_alias_a_to_b(SVREF a, SVREF b, int read_only)
  CODE:
{
    __alias_a_to_b(a, b, read_only);
}
