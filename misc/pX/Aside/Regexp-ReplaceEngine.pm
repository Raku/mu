=pod
PROJECT ABANDONED:

  The regexp compiler hook only gets the pattern _after_ variable
  interpolation has occured.  So this is a nice way to do a reentrant p5
  engine, even one with rules, but you can't do p6 regex with it.
=cut

=pod
Objective:

  Permit swapping in an alternate regexp engine,
  using calls to p5 subs,
  which can be defined differently in different packages.

This would permit
  ./perl -we 'use Regexp::Perl6; print "a" =~ /<word>**{1}/;'

Status:

  Perl subs get called for regexp comp and exec.
  The return value of exec is ignored. (match always succeeds)
  $1,etal are not set.
  The hooks are not package-specific.
  Regexp's compiled with the old engine stop working.

Design:

  The comp hook sub should return a subref, which gets written onto a
  hopefully otherwise unused section of a regexp struct.  Another hopefully
  unused part of the struct is overwritten with a flag value, so a regexp*
  thus created can be recognized (not implemented), and regexp*s created
  with the normal perl engine can continue working.

  Upon exec, the subref should return match information or undef (not
  implemented).  Match information gets inserted in $1 etal (not
  implemented). It seems likely these will be limited to strings,
  rather than Match objects, but we'll see.

  A set of hooks should be defined in CORE which just call back to the
  original comp and exec (not implemented).  The C hooks call for the
  sub hooks in the caller package, and thus get CORE if none are defined.

  The C hooks (except for comp, obviously) look for the flag value,
  and call the original Perl_ funcs if it is not present (not implemented).

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

  A first attempt to content, any content, into $1, didn't work.
  So perhaps back off from that, and:

  Have success actually depend on the comp hooks return value.
  Get the flag support in, so normal regexp*s continue working.
  Try again to get arbitrary string values into $1,etal,
   or into the return value.

Notes:

  http://perl.plover.com/Rx/paper/   fglock++

  Some files of interest in the perl sources:
    ./regexp.h ./regcomp.c ./regexec.c
    ./ext/re/re.xs ./ext/re/re.pm
    http://cvs.perl.org/viewcvs/perl5/mirror/

  And of course man perlapi, etc.

  The perl regexp engine was long ago derived from Henry Spencer's
  regexp library.  http://arglist.com/regex/  While the code has diverged,
  at least the library has some documentation, which can help point you
  in the right direction.

=cut

sub regexp_hook_exec {
  my($s)=@_;
  print STDERR "execing on string (".(defined($s)?"'$s'":"undef").").\n";
  return [];
}

sub regexp_hook_comp {
  my($re)=@_;
  print STDERR "compiling pattern '$re'.\n";
  return \&regexp_hook_exec;
}

hooks_in();
#my $qr = eval("qr/foobar2/");
#"abcd" =~ $qr;
print eval('"asdf" =~ /foobar3/; $1'),"\n";


use Inline C => <<'END';
void print_sv(SV* sv) {
  fprintf(stderr,"%ld\n",(unsigned long)sv);
}

regexp* hook_regcompp (pTHX_ char* exp, char* xend, PMOP* pm)
{
  regexp* prog;
  SV* f;
  int ret;
  dSP;
  fprintf(stderr,"hook_regcompp\n");
  ENTER;
  SAVETMPS;
  PUSHMARK(SP);
  XPUSHs(sv_2mortal(newSVpvn(exp,xend-exp)));
  PUTBACK;
  ret = call_pv("regexp_hook_comp", G_SCALAR);
  if(ret != 1) {
    fprintf(stderr,"regexp_hook_comp failed\n"); exit(0);
  }
  SPAGAIN;
  f = POPi;
  prog = Perl_pregcomp(aTHX_ "","",pm);
  prog->data = f;
  fprintf(stderr,"def %ld %ld\n",(ulong)prog,(ulong)f);
  PUTBACK;
  FREETMPS;
  LEAVE;
  return prog;
}
I32 hook_regexec (pTHX_ regexp* prog, char* stringarg, char* strend,
                  char* strbeg, I32 minend, SV* screamer,
                  void* data, U32 flags)
{
  SV* f;
  Inline_Stack_Vars;
  fprintf(stderr,"hook_regexec (%ld %ld %ld %ld) %d\n",(ulong)prog,(ulong)stringarg,(ulong)strend,(ulong)strbeg,strend-strbeg);
  Inline_Stack_Push(newSVpv(strbeg,strend-strbeg));
  Inline_Stack_Done;
  f = prog->data;
  fprintf(stderr,"exec hook %ld\n",(ulong)f);
  call_sv(f, 0);
  fprintf(stderr,"exec hook survived.\n");
  {
    /* not try to set up $1,etc with _anything_. */
  }
  return 1; /* 1-success, 0-failure */
}
void hook_regfree (pTHX_ struct regexp* r)
{
  fprintf(stderr,"free %ld\n",(ulong)r);
  r->data = NULL;
  if(0) {
    Perl_pregfree(aTHX_ r);
    fprintf(stderr,"free survived.\n");
  } else {
    fprintf(stderr,"free skipped.\n");
  }
  return;
}

char* hook_re_intuit_start (pTHX_ regexp *prog, SV *sv, char *strpos,
                            char *strend, U32 flags,
                            struct re_scream_pos_data_s *data)
{
  fprintf(stderr,"hook_re_intuit_start - doing nothing\n");
  return NULL;
}
SV*   hook_re_intuit_string (pTHX_ regexp *prog)
{
  fprintf(stderr,"hook_re_intuit_string - doing nothing\n");
  return &PL_sv_undef;
}

void hooks_in() {
  Inline_Stack_Vars;
  PL_regcompp = &hook_regcompp;
  PL_regexecp = &hook_regexec;
  PL_regint_start = &hook_re_intuit_start;
  PL_regint_string = &hook_re_intuit_string;
  PL_regfree = &hook_regfree;
  Inline_Stack_Void;
}
END


