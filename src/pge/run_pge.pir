.sub _main
    .local int argc, pos, size1, size2
    .local pmc args, match, add_rule, unescape
    .local string input, result, cmd, sizeStr, arg1, arg2
    .local pmc stdin, stdout
    .include "iglobals.pasm"

    load_bytecode "PGE.pbc"

# XXX - Inlined below for Parrot 0.3.0.  Re-evaluate this inlining on 0.3.1.
#   load_bytecode "PGE/Hs.pir"

    match = find_global "PGE::Hs", "match"
    add_rule = find_global "PGE::Hs", "add_rule"
    unescape = find_global "PGE::Hs", "unescape"

    getinterp args
    set args, args[.IGLOBALS_ARGV_LIST]
    argc = elements args
    if argc == 3 goto do_args

    getstdin stdin
    getstdout stdout
    stdin."setbuf"(0)
    stdout."setbuf"(0)

  loop:
    readline input, stdin
    length pos, input 
    if pos < 1 goto end
    pos = index input, " "
    if pos == 0 goto loop
    cmd = substr input, 0, pos
    inc pos
    input = substr input, pos
    pos = index input, " "
    if pos == 0 goto loop
    sizeStr = substr input, 0, pos
    size1 = sizeStr
    inc pos
    sizeStr = substr input, pos
    size2 = sizeStr

    # Now read up arg1 and arg2
    inc size1
    arg1 = read stdin, size1
    chopn arg1, 1 # skip \n
    inc size2
    arg2 = read stdin, size2
    chopn arg2, 1 # skip \n

    arg1 = unescape(arg1)
    arg2 = unescape(arg2)

    if cmd == "add_rule" goto do_add_rule
    if cmd == "match" goto do_match
    goto loop
 
  do_add_rule:
    add_rule(arg1, arg2)
    goto loop

  do_match:
    result = match(arg1, arg2)
    length pos, result 
    print "OK "
    print pos
    print "\n"
    print result
    print "\n"
    goto loop

  do_args:
    arg1 = args[1]
    arg2 = args[2]
    arg1 = unescape(arg1)
    arg2 = unescape(arg2)
    result = match(arg1, arg2)
    print result

  end:
.end

########################################################################

=head1 Title

PGE::Hs - Match and display PGE rules as Haskell expressions

=head1 SYNOPSIS

(You need to run C<make PGE-Hs.pbc> in F<compilers/pge> first.)

    .sub _main
        load_bytecode "PGE.pbc"
        load_bytecode "PGE/Hs.pir"
        $P0 = find_global "PGE::Hs", "match"
        $S0 = $P0("Hello", "(...)*$")
        print $S0   # PGE_Match 2 5 [PGE_Array [PGE_Match 2 5 [] []]] []
    .end

=head1 DESCRIPTION

The Haskell-side data structure is defined thus:

    data MatchPGE
        = PGE_Match Int Int [MatchPGE] [(String, MatchPGE)]
        | PGE_Array [MatchPGE]
        | PGE_Fail
        deriving (Show, Eq, Ord, Read)

This F<PGE-Hs.pbc> is built separately (not by default).  The reason is
because it's intended to be bundled with Pugs, so as to make Pugs usable
with vanilla Parrot from 0.2.0 on, using either an external F<parrot>
executable, or a linked F<libparrot>.

In external F<parrot> mode, Parrot's include path looks into the
F<.pbc> files inside the library tree first, then look into the current
directory, F<.>.  Hence this file includes, rather than loads, the
F<PGE.pbc> library, because if Pugs is shipped with its own copy
of F<PGE.pbc>, Parrot would ignore that file and prefer to load
the one in the Parrot tree instead.

Granted, it is possible to pass in Pugs's own library path into an
environment variable (maybe C<PARROT_LIBS>?), but as this was not in
the 0.3.0 release, I (autrijus) decided to take the easy route. :-)

=head1 CAVEATS

This is an initial sketch.  The dump format may change, and the
whole thing may be taken out or refactored away at any moment.

=cut

.namespace [ "PGE::Hs" ]

.const string PGE_FAIL = "PGE_Fail"
.const string PGE_SUB_POS = "@:capt"
.const string PGE_SUB_NAMED = "%:capt"

.sub "__onload" @LOAD
    .local pmc load
    load_bytecode "Data/Escape.imc"
.end

.sub "add_rule"
    .param string name
    .param string pattern
    .local pmc p6rule_compile, rulesub

    find_global p6rule_compile, "PGE", "p6rule"
    null rulesub
    rulesub = p6rule_compile(pattern)
    store_global name, rulesub

    .return (name)
.end

.sub "match"
    .param string x
    .param string pattern
    .local string out, tmps
    .local pmc rulesub
    .local pmc match
    .local pmc p6rule_compile
    .local pmc capt

    find_global p6rule_compile, "PGE", "p6rule"
    null rulesub

    push_eh match_error
    rulesub = p6rule_compile(pattern)
    match = rulesub(x)

  match_result:
    unless match goto match_fail
    tmps = match."dump_hs"()
    out .= tmps
    goto end_match

  match_fail:
    out = PGE_FAIL
    goto end_match

  match_error:
    out = P5

  end_match:
    out .= "\n"

    .return (out)
.end

.sub unescape
    .param string str
    .local string ret, tmp
    .local int i, j

    ret = ""
    j = length str
    if j == 0 goto END
    i = 0

LOOP:
    tmp = str[i]
    inc i
    if i >= j goto FIN

    eq tmp, "\\", ESC
    concat ret, tmp
    goto LOOP

ESC:
    tmp = str[i]
    inc i
    eq tmp, "n", LF
    concat ret, tmp
    goto UNESC
LF:
    concat ret, "\n"
UNESC:
    if i >= j goto END
    goto LOOP

FIN:
    concat ret, tmp
END:
    .return(ret)
.end

.namespace [ "PGE::Match" ]

.sub "dump_hs" :method
    .local string out
    .local int spi, spc
    .local int ari, arc
    .local int tmpi, cond
    .local string tmps, key
    .local pmc capt, iter, subelm, elm, escape, is_array

    out = ""
    escape = find_global "Data::Escape", "String"

  start:
    out .= "PGE_Match "
    tmpi = self."from"()
    tmps = tmpi
    out .= tmps
    out .= " "
    tmpi = self."to"()
    tmps = tmpi
    out .= tmps
    out .= " ["

  subpats:
    capt = getattribute self, PGE_SUB_POS
    if_null capt, subrules
    spi = 0
    spc = elements capt
    goto subpats_body
  subpats_loop:
    unless spi < spc goto subrules
    out .= ", "
  subpats_body:
    cond = defined capt[spi]
    unless cond goto subpats_fail
    elm = capt[spi]
    bsr dumper
    inc spi
    goto subpats_loop
  subpats_fail:
    out .= PGE_FAIL
    inc spi
    goto subpats_loop

  subrules:
    out .= "] ["
    capt = getattribute self, PGE_SUB_NAMED
    if_null capt, end
    iter = new Iterator, capt
    iter = 0
    goto subrules_body
  subrules_loop:
    unless iter goto end
    out .= ", "
  subrules_body:
    key = shift iter
    cond = defined capt[key]
    unless cond goto subrules_fail
    elm = capt[key]
    out .= '("'
    tmps = escape(key)
    out .= tmps
    out .= '", '
    bsr dumper
    out .= ")"
    goto subrules_loop
  subrules_fail:
    out .= PGE_FAIL
    key = shift iter
    goto subrules_loop

  dumper:
    ari = 0
    arc = elements elm
    is_array = getprop "isarray", elm
    if is_array goto dumper_array
    unless ari < arc goto dumper_fail
    subelm = elm[-1]
    tmps = subelm."dump_hs"()
    out .= tmps
    ret
  dumper_fail:
    out .= PGE_FAIL
    ret
  dumper_done:
    out .= "]"
    ret
  dumper_array:
    out .= "PGE_Array ["
    unless ari < arc goto dumper_done
    goto dumper_array_body
  dumper_array_loop:
    unless ari < arc goto dumper_done
    out .= ", "
  dumper_array_body:
    subelm = elm[ari]
    tmps = subelm."dump_hs"()
    out .= tmps
    inc ari
    goto dumper_array_loop

  end:
    out .= "]"
    .return (out)
.end
