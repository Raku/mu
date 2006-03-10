#include "EXTERN.h"
#include "perl.h"
#include "embed.h"
#include "ppport.h"

#include <regcomp.h>
/* for the struct pointed to by substrs :( */

#define RECOGNITION_FLAG 0xfdfcd84a
#define CONTAINS_RECOGNITION_FLAG(r) \
  (r->offsets[0] >= 1 && \
   r->offsets[1] == RECOGNITION_FLAG)

static regexec_t previous_exec_hook = NULL;


I32 regexp_exechook_hook (pTHX_ regexp* r, char* stringarg, char* strend,
			  char* strbeg, I32 minend, SV* screamer,
			  void* data, U32 flags)
{
  if(!CONTAINS_RECOGNITION_FLAG(r)) {
    return previous_exec_hook(aTHX_ r,stringarg,strend,strbeg,
			      minend,screamer,data,flags);
  }
  else {
    SV* perl_callback;
    I32 ret;
    IV matched;
    I32 delta;
    dSP;

/*
    fprintf(stderr,"strarg=%lu\nstrbeg=%lu\nstrend=%lu\n",stringarg,strbeg,strend);
    fprintf(stderr,"minend=%ld pos=%ld\n",minend,PL_reg_ganch);
    fprintf(stderr,"flags=%lu\n",flags);
*/

    perl_callback = r->substrs->data[0].substr;

    ENTER;
    SAVETMPS;
    PUSHMARK(SP);
    XPUSHs(newSVpv(stringarg,strend-stringarg));
    mXPUSHu(flags);
    mXPUSHu((unsigned long)r);
    PUTBACK;

    /* fprintf(stderr,"exec hook r=%lu callback SV*=%lu\n",(unsigned long)r,(unsigned long)perl_callback); */
    ret = call_sv(perl_callback, G_ARRAY);
    /* fprintf(stderr,"exec hook survived.\n"); */
    if(ret < 1) {
	fprintf(stderr,"regexp_hook_exec failed - didnt return anything\n"); exit(0);
    }
    SPAGAIN;
    matched = POPi;

    { /* fail captures */
      int i;
      for(i=0;i<=r->nparens;i++) {
	r->startp[i] = -1;
	r->endp[i] = -1;
      }
      r->lastparen = r->lastcloseparen = 0;
    }

    if(matched) {
      SV* lp;
      SV* lcp; 
      int i;
      if(ret < 3 || ret > 3 + 2 * (r->nparens+1)) {
        fprintf(stderr,"regexp_hook_exec failed - paren info broken\n"); exit(0);
      }
      lp = POPs;
      lcp = POPs;
      delta = stringarg-strbeg;
      for(i=0;i<=r->nparens && i+3<ret;i++) {
        I32 v = POPi;
	r->startp[i] = v >= 0 ? v+delta : v;
	v = POPi;
	r->endp[i] = v >= 0 ? v+delta : v;
      }
      r->lastparen = (lp == &PL_sv_undef) ? SvIV(lp) : r->nparens;
      r->lastcloseparen = (lcp == &PL_sv_undef) ? SvIV(lcp) : r->nparens;

      Safefree(r->subbeg);
      r->sublen = strend-strbeg;
      r->subbeg = savepvn(strbeg,r->sublen);
    }

    PUTBACK;
    FREETMPS;
    LEAVE;
    /* fprintf(stderr,"done.\n"); */
    return matched ? 1 : 0;
  }
}

void regexp_exechook_insert (pTHX)
{
  if(previous_exec_hook != NULL) { croak("can't install twice"); }
  previous_exec_hook = PL_regexecp;
  PL_regexecp = &regexp_exechook_hook;
}


/*
 * regexp struct setup
 */

static void regexp_setup(pTHX_ regexp* r, SV* pat, U32 nparens, SV* callback)
{
    I32 len;
    int i;

    /* fprintf(stderr,"regexp_setup "); */
    Safefree(r->precomp);
    r->prelen = SvLEN(pat);
    r->precomp = savesvpv(pat);
    
    r->nparens = nparens;
    
    Renew(r->startp, nparens+1, I32);
    Renew(r->endp, nparens+1, I32);
    
    len = 1;
    if(r->offsets[0] < len) {
      Renew(r->offsets, 2*len+1, U32);
      r->offsets[0] = len;
    }

    r->offsets[1] = RECOGNITION_FLAG;

    Safefree(r->substrs->data[0].substr);
    r->substrs->data[0].substr = SvREFCNT_inc(callback);

    r->minlen = 0;
    r->reganch = 0;
    
    /* XXX Does regcomp.c zero any of the match fields? */
}

/*
 * regexp_compile
 */

static regcomp_t previous_comp_hook = NULL;

regexp* hook_regcompp (pTHX_ char* exp, char* xend, PMOP* pm)
{
  SV* handler = NULL;

  /* fprintf(stderr,"hook_regcompp in %lx\n",(unsigned long)exp); */

  /* Check $^H for lexical selection of semantics and implementation */
  if(handler == NULL) {
    const char hint_key[] = "regcompp";
    SV** phandler;      
    HV* hints;
    hints = get_hv("\b",0);
    if(hints != NULL) {
      phandler = hv_fetch(hints, hint_key, strlen(hint_key), 0);
      if(phandler != NULL) {
        handler = *phandler;
        /* fprintf(stderr,"hook_regcompp lexical...%lx\n",(unsigned long)handler); */
/*	call_sv(handler,G_DISCARD|G_EVAL);
        fprintf(stderr,"can survive %lx %lx\n",(unsigned long)exp,(unsigned long)handler);
*/
      }
    }
  }
  /* Check $re::override::regcompp
     for a dynamic selection of implementation */
  if(handler == NULL) {
    SV* sv;
    sv = get_sv("$re::override::regcompp",0);
    if(sv != NULL && sv != &PL_sv_undef) {
      /* fprintf(stderr,"hook_regcompp dynamic...\n"); */
      handler = sv;
    }
  }  
  /* If no handlers, then hand off */
  if(handler == NULL) {
    /* fprintf(stderr,"hook_regcompp punt.\n"); */
    return previous_comp_hook(aTHX_ exp,xend,pm);
  }
  {
    dSP;
    char *nulpat;
    regexp* r;
    I32 ret;
    I32 api;

    nulpat = savepv(""); /*XXX - can be constant? */
    r = Perl_pregcomp(aTHX_ nulpat,nulpat,pm);
    /*XXX - can free nulpat now? */

/*    fprintf(stderr,"exp =%lu\nxend=%lu\n",(unsigned long)exp,(unsigned long)xend);*/
    PUSHMARK(SP);
    XPUSHs(sv_2mortal(newSVpvn(exp,xend-exp)));
    mXPUSHu((unsigned long)r);
    PUTBACK;
  HANDLER_CALL:
/*    fprintf(stderr,"calling out...\n");*/
    ret = call_sv(handler, G_ARRAY);
/*    fprintf(stderr,"...back.\n");*/
    SPAGAIN;
    api = POPi;
    if(api == 13) {
      SV* pat;
      long nparens;
      SV* exec_callback_sub;
/*      fprintf(stderr,"api 13\n");*/
      if(ret != 4) {
        fprintf(stderr,"api 13 violation\n"); exit(0);
      }
      pat = POPs;
      nparens = POPl;
      exec_callback_sub = POPs;
      PUTBACK;
      regexp_setup(aTHX_ r,pat,nparens,exec_callback_sub);
    } else if(api == 14) {
      SV* expr_code;
      SV* expr_result;
/*      fprintf(stderr,"api 14\n");*/
      if(ret != 3) {
        fprintf(stderr,"api 14 violation\n"); exit(0);
      }
      handler = SvREFCNT_inc(POPs); /*XXX needed? */
      expr_code = POPs;
      PUTBACK;
      expr_result = eval_pv(SvPV_nolen(expr_code),0);
      SPAGAIN;
      XPUSHs(expr_result);
      PUTBACK;
      goto HANDLER_CALL;
    } else {
      fprintf(stderr,"api UNKNOWN violation\n"); exit(0);
    }
    /* fprintf(stderr,"hook_regcompp done.\n"); */
    return r;
  }
}

void regexp_hook_on(pTHX) {
  if(previous_comp_hook != NULL) { croak("Cant install twice"); }
  previous_comp_hook = PL_regcompp;
  PL_regcompp = &hook_regcompp;
}
void regexp_hook_off(pTHX) { /* aka "abandon regexps", aka "please segfault" */
  PL_regcompp = previous_comp_hook;
  previous_comp_hook = NULL;
}

