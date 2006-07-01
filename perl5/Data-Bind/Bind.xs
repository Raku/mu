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

MODULE = Data::Bind                PACKAGE = Data::Bind

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

    if (type > SVt_PV) {
        switch (type) {
            case SVt_PVHV:
            case SVt_PVAV: {
                SV *tie = newRV_noinc((SV*)newHV());
                HV *stash = gv_stashpv(type == SVt_PVHV ?
                                       "Data::Bind::Hash" : "Data::Bind::Array",
                                       TRUE);
                hv_store(SvRV(tie), "real", 4, newRV_inc((SV *)b), 0);
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
            default:
                croak("don't know what to do yet");
        }
    }
    else {
        sv_magicext(a, b, PERL_MAGIC_ext, &alias_vtbl, 0, 0);
        mg_get(a);
    }

    if (read_only || SvREADONLY(b)) {
	SvREADONLY_on(a);
    }
}
