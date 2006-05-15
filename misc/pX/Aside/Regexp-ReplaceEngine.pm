# This file was an experiment in using the perl5 implementation's
# "regcomp hooks" (C functions called when regexps are compiled and
# applied) to permit alternate regexp engines to be used without
# requiring changes to user p5 code.  It was explored as a possible
# bootstrap path for perl6.

# The perl5 regexp engine is unfortunately entangled with the rest of
# the p5 implementation.  So while limited superficial success proved
# straightforward, a more robust solution would be far more difficult.
# One basically has to reverse engineer an implicit abstraction
# "barrier" for the engine, from the entire perl5 source.

# The exploratory effort was abandoned as insufficiently tractable.
# By requiring far more effort than was hoped, it became uninteresting
# as a perl6 bootstrap path, which was the objective.

# Interest was expressed in developing this into a cpan module for
# real use.  It might, imaginably, perhaps, be possible to do this
# Maybe.  And perl5 might benefit from its existence.  Normally this
# kind of reverse-engineered implicit abstraction kludge would be
# unsustainable, and unsuitable for anything but throwaway code.
# However, the perl5 core team expressed some interest in perhaps
# maintaining the capability if created, and in perhaps accepting
# patches to remove roadblocks if encountered.  The latter would of
# course not help people running existent versions of perl5.  Perhaps
# because the challenges being faced were insufficiently documented,
# audreyt forked, creating re-override, intending to provide just such
# a usable module.  Any further efforts should likely take place there.

# Some raw development notes appear after __END__.

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
  my $regexp_compile_hook = sub {
    $r_address = $_[0];
    return reverse(13,$pat,$nparens,$callback);
  };
  my $qr = eval 'qr//'; die $@ if $@;
  return ($qr,$r_address);
}

use Inline C => <<'END';

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
    newstr = 1; /*1-leak,(0 only for isolated devtest)*/
    if(newstr) {
/*      Safefree(r->subbeg);*/ /* fails split.t with newstr==1*/
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
      for(cap_idx=0; cap_idx <= r->nparens && pops_left;cap_idx++) {
        SV* v;
        IV vi;

        v = POPs; pops_left--;
        vi = SvOK(v) ? SvIV(v) : -1;
        r->startp[cap_idx] = vi;

        v = POPs; pops_left--;
        vi = SvOK(v) ? SvIV(v) : -1;
        r->endp[cap_idx] = vi;
      }
      for(;pops_left;) {
        POPs; pops_left--;
      }

      r->lastparen = SvOK(lp) ? SvIV(lp) : r->nparens;
      r->lastcloseparen = SvOK(lcp) ? SvIV(lcp) : r->nparens;
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

    /*XXX should SvREFCNT_dec current value */
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

  if(handler == NULL) {
    SV* sv;
    sv = eval_pv("$regexp_compile_hook",0);
    if(sv != NULL && SvTRUE(sv)) {
      fprintf(stderr,"regexp_compile_hook...\n");
      handler = sv;
    }
  }  
  if(handler == NULL && 0) {
    SV* sv;
    sv = eval_pv("$regexp_compile_hook_p5re",0);
    if(sv != NULL && SvTRUE(sv)) {
      fprintf(stderr,"regexp_compile_hook_p5re...\n");
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
#__END__
Regexp::StructHook::regexp_hook_on();
=pod
my $str = "axbxc";
my(@returns) = ([1,undef,undef,1,2,1,2],[1,undef,undef,3,4,3,3],[0],[0]);
my($qr) = Regexp::StructHook::make_qr_regexp_pair("a",1,sub{
  print STDERR "hi\n";
  my $ret = shift(@returns);
  return reverse(@$ret);
});
my(@a)=split($qr,$str);
print STDERR "(",join(", ",map{defined($_)?"'$_'":"undef"}@a),") ",0+@a,"\n";

0;
__END__
=cut
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

our $regexp_compile_hook = sub{
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
  [It was.  The regexp implementation is intimately entangled with
  the rest of perl.]

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


=pod

=head1 Perl5's struct regexp and its abuse

=head1 Description

The perl5 regexp engine is entangled with the rest of the perl core.
Some of the interaction is via global variables.  A lot of it uses a
regexp struct, which gets attached to the op tree.  The struct is used
variously to return the compiled regexp, to give input to a search,
and to get back match results.

Our objective here is to see if it is possible to use the regexp
struct as a new api into the guts of the perl core, to permit the use
of alternate regexp engines.  There being little documentation, we
reverse engineering perl source.

We basically use the perl regcompp and regexecp hooks (C functions
which get called on regular expression compilation and execution) to
parasitize the regexp struct - attempting to make it look sufficiently
like what the core expects to avoid problems.  While simultaneously
trying to avoid triggering core behavior which we are not prepared to
support (such as modification of the compiled re op tree the native
compiler creates, but which we obviously don't have).


=head1 struct regexp

  typedef struct regexp {

=head2 The regular expression:

  I32 prelen;             /* length of precomp */
  char *precomp;          /* pre-compilation regular expression */
  U32 nparens;            /* number of parentheses */

=head2 The match result:

  I32 sublen;             /* Length of string pointed by subbeg */
  char *subbeg;           /* saved or original string 
                                   so \digit works forever. */

  I32 *startp;
  I32 *endp;

Length is nparens+1.
startp[n] is the starting offset in subbeg,
endp[n] is the offset of the last char within the capture.
n=0 is the whole match.  $1 is n=1, etc.
A -1 in both indicates the capture didn't happen.

  U32 lastparen;          /* last paren matched */
  U32 lastcloseparen;     /* last paren matched */

=head2 Other things the world messes with:


  I32 refcnt;

What invariants?  Don'T know.


  I32 minlen;             /* mininum possible length of $& */

Must be zero.  Otherwise the run core thinks too much.


  U32 reganch;            /* Internal use only +
                             Tainted information used by regexec? */

Flags.  Despite the comment, _lots_ of folks look at it.  The one
field labeled "Internal use only" is one of the most widely used. ;)
It get's is own section, elsewhere.


  regnode program[1];     /* Unwarranted chumminess with compiler. */

regcomp.h: This is essentially a linear encoding of a nondeterministic
finite-state machine (aka syntax charts or "railroad normal form" in
parsing technology).  Each node is an opcode plus a "next" pointer,
possibly plus an operand.

We don't care.


=head2 Internals:

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

Aliases:

 #define anchored_substr  substrs->data[0].substr
 #define anchored_utf8    substrs->data[0].utf8_substr
 #define anchored_offset  substrs->data[0].min_offset
 #define float_substr     substrs->data[1].substr
 #define float_utf8       substrs->data[1].utf8_substr
 #define float_min_offset substrs->data[1].min_offset
 #define float_max_offset substrs->data[1].max_offset
 #define check_substr     substrs->data[2].substr
 #define check_utf8       substrs->data[2].utf8_substr
 #define check_offset_min substrs->data[2].min_offset
 #define check_offset_max substrs->data[2].max_offset

data[0].substr, aka anchored_substr, is used only in
regcomp.c/regexec.c.


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
which Perl_re_dup() will copy for us.

    New(0, ret->offsets, 2*len+1, U32);
    Copy(r->offsets, ret->offsets, 2*len+1, U32);


 } regexp;

=head2 Additional notes

Perl_re_dup() in sv.c is a useful source of information, as well as (I
suspect) an important constraint on what fields can contain.

=head1 reganch

 REG_SEEN_GPOS

Confined to regcomp/regexec.

 RE_INTUIT_TAIL 
 RE_USE_INTUIT 

 PMf_COMPILETIME
 PMf_EXTENDED
 PMf_MULTILINE

 ROPT_ANCH 
 ROPT_ANCH_BOL 
 ROPT_ANCH_GPOS 
 ROPT_ANCH_MBOL 
 ROPT_ANCH_SBOL 
 ROPT_ANCH_SINGLE 
 ROPT_CANY_SEEN 
 ROPT_CHECK_ALL 

 ROPT_COPY_DONE 

Aka RX_MATCH_COPIED.
See also flag REXEC_COPY_STR.


 ROPT_EVAL_SEEN 
 ROPT_GPOS_SEEN 
 ROPT_IMPLICIT 
 ROPT_LOOKBEHIND_SEEN 
 ROPT_MATCH_UTF8 
 ROPT_NAUGHTY 
 ROPT_NOSCAN 
 ROPT_SKIP 
 ROPT_TAINTED_SEEN 
 ROPT_UTF8 


=head1 Compilation

=head2 flags

=head1 Execution - Input

=head1 Execution - Output

=head1 Todo

 #define REG_SEEN_ZERO_LEN        1
 #define REG_SEEN_LOOKBEHIND      2
 #define REG_SEEN_GPOS            4
 #define REG_SEEN_EVAL            8
 #define REG_SEEN_CANY           16
 #define REG_SEEN_SANY           REG_SEEN_CANY /* src bckwrd cmpt */


  #define REXEC_COPY_STR  0x01            /* Need to copy the string. */



  #define REXEC_CHECKED   0x02            /* check_substr already checked. */

  #define REXEC_SCREAM    0x04            /* use scream table. */
  #define REXEC_IGNOREPOS 0x08            /* \G matches at start. */
  #define REXEC_NOT_FIRST 0x10            /* This is another iteration of //g. */
  #define REXEC_ML        0x20            /* $* was set. */



 regcomp.h:
 * The "internal use only" fields in regexp.h are present to pass info from
 * compile to execute that permits the execute phase to run lots faster on
 * simple cases.  They are:
 *
 * regstart     sv that must begin a match; Nullch if none obvious

Confined to regcomp/regexec.

 * reganch      is the match anchored (at beginning-of-line only)?

Very popular.  Used all over perl.  Discussed elsewhere.

 * regmust      string (pointer into program) that match must include, or NULL
 *  [regmust changed to SV* for bminstr()--law]

Confined to regcomp/regexec.

 * regmlen      length of regmust string
 *  [regmlen not used currently]

Confined to regcomp/regexec.

 *
 * Regstart and reganch permit very fast decisions on suitable starting points
 * for a match, cutting down the work a lot.  Regmust permits fast rejection
 * of lines that cannot possibly match.  The regmust tests are costly enough
 * that pregcomp() supplies a regmust only if the r.e. contains something
 * potentially expensive (at present, the only such thing detected is * or +
 * at the start of the r.e., which can involve a lot of backup).  Regmlen is
 * supplied because the test in pregexec() needs it and pregcomp() is computing
 * it anyway.
 * [regmust is now supplied always.  The tests that use regmust have a
 * heuristic that disables the test if it usually matches.]
 *
 * [In fact, we now use regmust in many cases to locate where the search
 * starts in the string, so if regback is >= 0, the regmust search is never
 * wasted effort.  The regback variable says how many characters back from
 * where regmust matched is the earliest possible start of the match.
 * For instance, /[a-z].foo/ has a regmust of 'foo' and a regback of 2.]

 [regexp.h]
 #define ROPT_ANCH		(ROPT_ANCH_BOL|ROPT_ANCH_MBOL|ROPT_ANCH_GPOS|ROPT_ANCH_SBOL)
 #define ROPT_ANCH_SINGLE	(ROPT_ANCH_SBOL|ROPT_ANCH_GPOS)
 #define ROPT_ANCH_BOL	 	0x00001
 #define ROPT_ANCH_MBOL	 	0x00002
 #define ROPT_ANCH_SBOL	 	0x00004
 #define ROPT_ANCH_GPOS	 	0x00008
 #define ROPT_SKIP		0x00010
 #define ROPT_IMPLICIT		0x00020	/* Converted .* to ^.* */
 #define ROPT_NOSCAN		0x00040	/* Check-string always at start. */
 #define ROPT_GPOS_SEEN		0x00080
 #define ROPT_CHECK_ALL		0x00100
 #define ROPT_LOOKBEHIND_SEEN	0x00200
 #define ROPT_EVAL_SEEN		0x00400
 #define ROPT_CANY_SEEN		0x00800
 #define ROPT_SANY_SEEN		ROPT_CANY_SEEN /* src bckwrd cmpt */

 /* 0xf800 of reganch is used by PMf_COMPILETIME */

 #define ROPT_UTF8		0x10000
 #define ROPT_NAUGHTY		0x20000 /* how exponential is this pattern? */
 #define ROPT_COPY_DONE		0x40000	/* subbeg is a copy of the string */
 #define ROPT_TAINTED_SEEN	0x80000
 #define ROPT_MATCH_UTF8		0x10000000 /* subbeg is utf-8 */

 #define RE_USE_INTUIT_NOML	0x0100000 /* Best to intuit before matching */
 #define RE_USE_INTUIT_ML	0x0200000
 #define REINT_AUTORITATIVE_NOML	0x0400000 /* Can trust a positive answer */
 #define REINT_AUTORITATIVE_ML	0x0800000 
 #define REINT_ONCE_NOML		0x1000000 /* Intuit can succed once only. */
 #define REINT_ONCE_ML		0x2000000
 #define RE_INTUIT_ONECHAR	0x4000000
 #define RE_INTUIT_TAIL		0x8000000

 #define RE_USE_INTUIT		(RE_USE_INTUIT_NOML|RE_USE_INTUIT_ML)
 #define REINT_AUTORITATIVE	(REINT_AUTORITATIVE_NOML|REINT_AUTORITATIVE_ML)
 #define REINT_ONCE		(REINT_ONCE_NOML|REINT_ONCE_ML)

 #define RX_MATCH_TAINTED(prog)	((prog)->reganch & ROPT_TAINTED_SEEN)
 #define RX_MATCH_TAINTED_on(prog) ((prog)->reganch |= ROPT_TAINTED_SEEN)
 #define RX_MATCH_TAINTED_off(prog) ((prog)->reganch &= ~ROPT_TAINTED_SEEN)
 #define RX_MATCH_TAINTED_set(prog, t) ((t) \
					? RX_MATCH_TAINTED_on(prog) \
					: RX_MATCH_TAINTED_off(prog))

 #define RX_MATCH_COPIED(prog)		((prog)->reganch & ROPT_COPY_DONE)
 #define RX_MATCH_COPIED_on(prog)	((prog)->reganch |= ROPT_COPY_DONE)
 #define RX_MATCH_COPIED_off(prog)	((prog)->reganch &= ~ROPT_COPY_DONE)
 #define RX_MATCH_COPIED_set(prog,t)	((t) \
					  ? RX_MATCH_COPIED_on(prog) \
					  : RX_MATCH_COPIED_off(prog))

 #ifdef PERL_COPY_ON_WRITE
 #define RX_MATCH_COPY_FREE(rx) \
	 STMT_START {if (rx->saved_copy) { \
	     SV_CHECK_THINKFIRST_COW_DROP(rx->saved_copy); \
	 } \
	 if (RX_MATCH_COPIED(rx)) { \
	     Safefree(rx->subbeg); \
	     RX_MATCH_COPIED_off(rx); \
	 }} STMT_END
 #else
 #define RX_MATCH_COPY_FREE(rx) \
	 STMT_START {if (RX_MATCH_COPIED(rx)) { \
	     Safefree(rx->subbeg); \
	     RX_MATCH_COPIED_off(rx); \
	 }} STMT_END
 #endif

 #define RX_MATCH_UTF8(prog)		((prog)->reganch & ROPT_MATCH_UTF8)
 #define RX_MATCH_UTF8_on(prog)		((prog)->reganch |= ROPT_MATCH_UTF8)
 #define RX_MATCH_UTF8_off(prog)		((prog)->reganch &= ~ROPT_MATCH_UTF8)
 #define RX_MATCH_UTF8_set(prog, t)	((t) \
			 ? (RX_MATCH_UTF8_on(prog), (PL_reg_match_utf8 = 1)) \
			 : (RX_MATCH_UTF8_off(prog), (PL_reg_match_utf8 = 0)))

 #define REXEC_COPY_STR	0x01		/* Need to copy the string. */
 #define REXEC_CHECKED	0x02		/* check_substr already checked. */
 #define REXEC_SCREAM	0x04		/* use scream table. */
 #define REXEC_IGNOREPOS	0x08		/* \G matches at start. */
 #define REXEC_NOT_FIRST	0x10		/* This is another iteration of //g. */
 #define REXEC_ML	0x20		/* $* was set. */

 #define ReREFCNT_inc(re) ((void)(re && re->refcnt++), re)
 #define ReREFCNT_dec(re) CALLREGFREE(aTHX_ re)

 #define FBMcf_TAIL_DOLLAR       1
 #define FBMcf_TAIL_DOLLARM      2
 #define FBMcf_TAIL_Z            4
 #define FBMcf_TAIL_z            8
 #define FBMcf_TAIL              (FBMcf_TAIL_DOLLAR|FBMcf_TAIL_DOLLARM|FBMcf_TAIL_Z|
 FBMcf_TAIL_z)

 #define FBMrf_MULTILINE 1

pp_ctl.c
pp_regcomp
pp_substcont
Perl_rxres_save
Perl_rxres_restore
Perl_rxres_free

=cut
