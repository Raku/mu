# todo PM_GETRE  CALLREGEXEC
# See notes after __END__
# Also note that it's been repackaged to perl5/re-override/

package Regexp::StructHook;

our $inserted = 0;
sub import {
  $inserted++ or regexp_exechook_insert();
}

sub make_qr_regexp_pair {
  my($pat,$nparens,$callback)=@_;
  die "bug - no nparens" if !defined($nparens);
  die "bug - no callback" if !defined($callback);
  my $r_address;
  $^H{regcompp} = sub {
    $r_address = $_[0];
    return reverse(13,$pat,$nparens,$callback);
  };
  my $qr = eval 'qr//'; die $@ if $@;
  return ($qr,$r_address);
}

BEGIN{$Regexp::Override::regcompp = undef;}

use Inline C => <<'END';

#include <regcomp.h>
/* for the struct pointed to by substrs :( */

#define RECOGNITION_FLAG 0xfdfcd84a
#define CONTAINS_RECOGNITION_FLAG(r) \
  (r->offsets[0] >= 1 && \
   r->offsets[1] == RECOGNITION_FLAG)

static regexec_t previous_exec_hook = NULL;

#define Safefree(x) 1

I32 regexp_exechook_hook (pTHX_ regexp* r, char* stringarg, char* strend,
			  char* strbeg, I32 minend, SV* screamer,
			  void* data, U32 flags)
{
  if(!CONTAINS_RECOGNITION_FLAG(r)) {
    return previous_exec_hook(r,stringarg,strend,strbeg,
			      minend,screamer,data,flags);
  }
  else {
    dSP;
    int pops_left;
    SV* perl_callback;
    I32 ret;
    IV matched;
    U32 start;
    int newstr;

    if(stringarg-strbeg < 0) {fprintf(stderr,"assert-082eedc532\n");exit(0);}
    start = stringarg-strbeg;

    fprintf(stderr,"regexp_exechook_hook\n");
    fprintf(stderr," strbeg=%lu\n strarg=%lu\n strend=%lu\n",strbeg,stringarg,strend);
    fprintf(stderr," flags=%lu\n",flags);
    fprintf(stderr," start=%lu\n",start);
/*
    fprintf(stderr," minend=%ld pos=%ld\n",minend,PL_reg_ganch);
*/

    perl_callback = r->substrs->data[0].substr;

    PUSHMARK(SP);
    XPUSHs(sv_2mortal(newSVpv(strbeg,strend-strbeg)));
    XPUSHs(sv_2mortal(newSVuv(flags)));
    XPUSHs(sv_2mortal(newSVuv(start)));
    XPUSHs(sv_2mortal(newSViv(minend)));
    XPUSHs(sv_2mortal(newSVuv((ulong)r)));
    PUTBACK;

    fprintf(stderr,"exec hook r=%lu callback SV*=%lu\n",(ulong)r,(ulong)perl_callback);
    ret = call_sv(perl_callback, G_ARRAY);
    SPAGAIN;
    fprintf(stderr,"exec hook survived.\n");
    if(ret < 1) {
	fprintf(stderr,"regexp_hook_exec failed - didnt return anything\n"); exit(0);
    }
    pops_left = ret;
    matched = POPi; pops_left--;

    { /* fail captures */
      int i;
      for(i=0;i<=r->nparens;i++) {
	r->startp[i] = -1;
	r->endp[i] = -1;
      }
      r->lastparen = r->lastcloseparen = 0;
    }

    newstr = (r->sublen != strend-strbeg ||
	      !strnEQ(r->subbeg,strbeg,r->sublen));
    newstr = 1; /*1-leak?,(0 only for isolated devtest)*/
    if(newstr) {
      Safefree(r->subbeg);
      r->sublen = strend-strbeg;
      r->subbeg = savepvn(strbeg,r->sublen);
    }
    RX_MATCH_COPIED_set(r,newstr);

    if(matched) {
      SV* lp;
      SV* lcp; 
      int cap_idx = 0; /* number of caps is nparens+1 */
      if(ret < 1+2+2 || ret > 1+2+2 + 2 * r->nparens) {
        fprintf(stderr,"regexp_hook_exec failed - paren info broken\n"); exit(0);
      }
      lp = POPs; pops_left--;
      lcp = POPs; pops_left--;
      for(cap_idx=0; cap_idx <= r->nparens;cap_idx++) {
        r->startp[cap_idx] = POPi; pops_left--;
	r->endp[cap_idx] = POPi; pops_left--;
      }
      for(;pops_left;) {
        POPs; pops_left--;
      }

      r->lastparen = (lp == &PL_sv_undef) ? SvIV(lp) : r->nparens;
      r->lastcloseparen = (lcp == &PL_sv_undef) ? SvIV(lcp) : r->nparens;
    }
    else {
      while(pops_left--) {
        POPs;
      }
    }
    PUTBACK;

    fprintf(stderr,"done.\n");
    return matched ? 1 : 0;
  }
}

void regexp_exechook_insert ()
{
  if(previous_exec_hook != NULL) { croak("can't install twice"); }
  previous_exec_hook = PL_regexecp;
  PL_regexecp = &regexp_exechook_hook;
}


/*
 * regexp struct setup
 */

static void regexp_setup(regexp* r, SV* pat, U32 nparens, SV* callback)
{
    I32 len;
    int i;

    fprintf(stderr,"regexp_setup ");
    Safefree(r->precomp);
    r->prelen = SvLEN(pat);
    if(r->prelen <0){ fprintf(stderr,"zoork!\n");exit(0);}
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

    /* Try to scare off the rest of perl */
    r->minlen = 0;
    r->reganch = 0;
    /* Make sure environment is set up (also helps scare off perl) */
    r->reganch |= ROPT_GPOS_SEEN | ROPT_LOOKBEHIND_SEEN | ROPT_EVAL_SEEN;
    r->reganch |= ROPT_CANY_SEEN | ROPT_SANY_SEEN;
    /* But not ROPT_CHECK_ALL */

    
    /* XXX Does regcomp.c zero any of the match fields? */
}

/*
 * regexp_compile
 */

static regcomp_t previous_comp_hook = NULL;

regexp* hook_regcompp (pTHX_ char* exp, char* xend, PMOP* pm)
{
  SV* handler = NULL;

  fprintf(stderr,"hook_regcompp in %lx\n",(ulong)exp);

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
        fprintf(stderr,"hook_regcompp lexical...%lx\n",(ulong)handler);
/*	call_sv(handler,G_DISCARD|G_EVAL);
        fprintf(stderr,"can survive %lx %lx\n",(ulong)exp,(ulong)handler);
*/
      }
    }
  }
  /* Check $Regexp::Override::regcompp
     for a dynamic selection of implementation */
  if(handler == NULL) {
    SV* sv;
    sv = get_sv("Regexp::Override::regcompp",0);
    if(sv != NULL && sv != &PL_sv_undef && SvTRUE(sv)) {
      fprintf(stderr,"hook_regcompp dynamic...\n");
      handler = sv;
    }
  }  
  /* If no handlers, then hand off */
  if(handler == NULL) {
    fprintf(stderr,"hook_regcompp punt.\n");
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

/*    fprintf(stderr,"exp =%lu\nxend=%lu\n",(ulong)exp,(ulong)xend);*/
    PUSHMARK(SP);
    XPUSHs(sv_2mortal(newSVpvn(exp,xend-exp)));
    XPUSHs(sv_2mortal(newSVuv((ulong)r)));
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
      regexp_setup(r,pat,nparens,exec_callback_sub);
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
    fprintf(stderr,"hook_regcompp done.\n");
    return r;
  }
}

void regexp_hook_on() {
  if(previous_comp_hook != NULL) { croak("Cant install twice"); }
  previous_comp_hook = PL_regcompp;
  PL_regcompp = &hook_regcompp;
}
void regexp_hook_off() { /* aka "abandon regexps", aka "please segfault" */
  PL_regcompp = previous_comp_hook;
  previous_comp_hook = NULL;
}


END

#----------------------------------------------------------------------
package main;
Regexp::StructHook::import();

1;
__END__
Regexp::StructHook::regexp_hook_on();

my $str = "axbxc";
my(@returns) = ([1,undef,undef,1,2,],[1,undef,undef,3,4],[0]);
my($qr) = Regexp::StructHook::make_qr_regexp_pair("a",0,sub{
  print STDERR "hi\n";
  my $ret = shift(@returns);
  return reverse(@$ret);
});
my(@a)=split($qr,$str);
print STDERR "(",join(", ",map{defined($_)?"'$_'":"undef"}@a),") ",0+@a,"\n";


0;
__END__

$foo = sub{
    print STDERR "hi\n";
    return reverse(1,undef,undef,1,3,2,3);
};
=pod
=cut

my($qr) = Regexp::StructHook::make_qr_regexp_pair("a",1,$foo);

eval <<'END'; print $@ if $@;
my $m = "asdf" =~ $qr;
print "match? ",$m ? "yes" : "no","\n";
print ' $` ',defined($`)?"\"$`\"":"undef","\n";
print ' $& ',defined($&)?"\"$&\"":"undef","\n";
print ' $\' ',defined($')?"\"$'\"":"undef","\n";
print ' $-[0] ',defined($-[0])?"$-[0]":"undef","\n";
print ' $+[0] ',defined($+[0])?"$+[0]":"undef","\n";
print ' $1 ',defined($1)?"\"$1\" $-[1] $+[1]":"undef","\n";
print ' $2 ',defined($2)?"\"$2\" $-[2] $+[2]":"undef","\n";
print ' $3 ',defined($3)?"\"$3\" $-[3] $+[3]":"undef","\n";
print ' $^N ',defined($^N)?"\"$^N\"":"undef","\n";
END

print "alive\n";

$^H{regcompp} = sub{
  my($re)=@_;
  print STDERR "compiling pattern '$re'.\n";
  return reverse(13,"b",1,$foo);
};

#package Foo;

eval <<'END';
my $m = "wxyzqwerty" =~ /qwert/;
print "match? ",$m ? "yes" : "no","\n";
print ' $` ',defined($`)?"\"$`\"":"undef","\n";
print ' $& ',defined($&)?"\"$&\"":"undef","\n";
print ' $\' ',defined($')?"\"$'\"":"undef","\n";
print ' $-[0] ',defined($-[0])?"$-[0]":"undef","\n";
print ' $+[0] ',defined($+[0])?"$+[0]":"undef","\n";
print ' $1 ',defined($1)?"\"$1\" $-[1] $+[1]":"undef","\n";
print ' $2 ',defined($2)?"\"$2\" $-[2] $+[2]":"undef","\n";
print ' $3 ',defined($3)?"\"$3\" $-[3] $+[3]":"undef","\n";
print ' $^N ',defined($^N)?"\"$^N\"":"undef","\n";
END




__END__

=pod
Objective:

  Permit swapping in an alternate regexp engine,
  using calls to p5 subs,
  which can be defined differently in different packages.

This would permit
  ./perl -we 'use Regexp::Perl6; print "a" =~ /<word>**{1}/;'

Status:

  Perl subs get called for regexp comp and exec.
  The hooks are not package-specific.

Design:

  The comp hook sub should return a subref, which gets written onto a
  hopefully otherwise unused section of a regexp struct.  Another
  hopefully unused part of the struct is overwritten with a flag
  value, so a regexp* thus created can be recognized, and regexp*s
  created with the normal perl engine can continue working.

  Upon exec, the subref should return match information.  Match
  information gets inserted in $1 etal.  These will be limited to
  strings, rather than Match objects.

  A set of hooks should be defined in CORE which just call back to the
  original comp and exec (not implemented).  The C hooks call for the
  sub hooks in the caller package, and thus get CORE if none are defined.

  The C hooks (except for comp, obviously) look for the flag value,
  and call the original Perl_ funcs if it is not present.

Risks:

  Trying to properly do (non-leaking) memory management, via reverse
  engineering (almost completely undocumented source), in the presence
  of copy-on-write and such complications, could be a bear.

  A source filter approach, while likely flakier (modulo memory issues),
  might suffice, be easier to create, and be more portable.
  http://www.perlmonks.org/index.pl?node_id=531465

  It may not be possible to get Match objects (only strings) into
  $1,etal, or even into the return value.  In that event, it may not
  be possible to get a plausibly scoped alternative working.

  Using $/ might be non-trivial, as only some forms of redefinition
  make its magic not stick.

  The implementation is at best an XS-based module (currently Inline::C),
  and is thus not very portable to broken (no compiler) microsoft systems.

  The implementation reaches into perlguts, which limits the range of
  perl versions it will work with, and raises maintenance concerns.
  That said, the use of internals is rather limited, and in a rather
  stable are of the code, so this may not be a real problem.

  The regexp engine and the perl compiler have an overly friendly
  relationship.  There may end up being a gotcha, where one needs
  something to happen or not happen, but the relevant line of code is
  off in perl runtime guts where it can't plausibly be reached to
  change.

  There may be no place to put the flag value?  And then prove
  implausible to realloc the regexp struct to create one?

Possible next steps:

  Get per-package hooks working.

Notes:

  http://perl.plover.com/Rx/paper/

  Some files of interest in the perl sources:
    ./regexp.h ./regcomp.c ./regexec.c
    ./ext/re/re.xs ./ext/re/re.pm
    http://cvs.perl.org/viewcvs/perl5/mirror/

  And of course man perlapi, etc.

  The perl regexp engine was long ago derived from Henry Spencer's
  regexp library.  http://arglist.com/regex/  While the code has diverged,
  at least the library has some documentation, which can help point you
  in the right direction.

  The regexp compiler hook generally gets the pattern _after_ variable
  interpolation has occured.  But not in the case of m'pattern'.
  Which might come in hand when doing p6 regex.



Notes on struct regex:

  typedef struct regexp {

The regular expression:

  I32 prelen;             /* length of precomp */
  char *precomp;          /* pre-compilation regular expression */
  U32 nparens;            /* number of parentheses */

The match result:

  I32 sublen;             /* Length of string pointed by subbeg */
  char *subbeg;           /* saved or original string 
                                   so \digit works forever. */

  I32 *startp;
  I32 *endp;

for $n (eg $1 => n=1), 1<=n<=nparens,
startp[n] is the starting offset in subbeg,
endp[n] is the offset of the last char within the capture.
n=0 is the total match?

  U32 lastparen;          /* last paren matched */
  U32 lastcloseparen;     /* last paren matched */

Other things the world messes with:

  I32 refcnt;

What invariants?  Don't know.


  I32 minlen;             /* mininum possible length of $& */

Must be zero.  Otherwise the run core thinks too much.


  U32 reganch;            /* Internal use only +
                             Tainted information used by regexec? */

Flags.  Despite the comment, _lots_ of folks look at it.  The one
field labeled "Internal use only" is one of the most widely used. ;)
Zero seems safe.  But more nuanced treatment will be needed if utf8 is
to be supported.

  regnode program[1];     /* Unwarranted chumminess with compiler. */

I don't know.

Internals:

  regnode *regstclass;

Points into ->data.
sv.c's Perl_re_dup sets it to NULL without looking.
No-one else touches it.

  struct reg_substr_data *substrs;

        struct reg_substr_data {
          struct reg_substr_datum data[3];
        };
        struct reg_substr_datum {
            I32 min_offset;
            I32 max_offset;
            SV *substr;         /* non-utf8 variant */
            SV *utf8_substr;    /* utf8 variant */
        };

min_offset and max_offset would be plausible places to hide data.
Perl_re_dup just copies them.

  struct reg_data *data;  /* Additional data. */

        struct reg_data {
            U32 count;
            U8 *what;
            void* data[1];
        };

Leave something real here and don't touch it.
Perl_re_dup knows a lot about it.


#ifdef PERL_COPY_ON_WRITE
  SV *saved_copy;         /* If non-NULL, SV which is COW from original */
#endif

I don't know.
Possible danger?

  U32 *offsets;           /* offset annotations 20001228 MJD */

offsets[0] specifies the 2*len+1 length of the array.
Other than that, we can do anything we want with it.
This is perhaps the most flexible place to allocate large data
which Perl_re_dup will copy for us.

    New(0, ret->offsets, 2*len+1, U32);
    Copy(r->offsets, ret->offsets, 2*len+1, U32);


} regexp;


Notes on functions:

regexp* regcompp (pTHX_ char* exp, char* xend, PMOP* pm)

what is pm?
XXX finish

I32      regexec (pTHX_ regexp* prog, char* stringarg, char* strend,
                  char* strbeg, I32 minend, SV* screamer,
                  void* data, U32 flags)

retval
 0 failure
 1 success
yes?

XXX finish


XXX What exactly _is_ intuit, and do we care?


=cut
