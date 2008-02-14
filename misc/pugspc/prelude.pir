.include 'iglobals.pasm'
.include 'errors.pasm'
.sub "&return"
    get_params "(32)", $P10
    $P8 = new .ResizablePMCArray
    $P8[0] =  $P10
    throw $P8
.end
.sub "&leave"
    get_params "(32)", $P10
    .return ($P10)
.end
.sub "&statement_control:for"
    get_params "(0, 0)", $P10, $P11
    $P8 = iter $P10
  sc_for_next:
    unless $P8, sc_for_last
    $P9 = shift $P8
    set_args '(64)', $P9
    invokecc $P11
    goto sc_for_next
  sc_for_last:
    returncc
.end
.sub "&statement_control:loop"
    get_params "(0, 0, 0, 0)", $P10, $P11, $P12, $P13
  sc_loop_next:
    set_args '()'
    get_results "(0)", $P8
    invokecc $P11
    unless $P8, sc_loop_last
    set_args '()'
    invokecc $P12
    set_args '()'
    invokecc $P13
    goto sc_loop_next
  sc_loop_last:
    returncc
.end
.sub "&statement_control:while"
    get_params "(0, 0)", $P10, $P11
    print "in while: "
    print $P10
    print $P11
    print "\n"
  sc_while_next:
    set_args '()'
    get_results "(0)", $P8
    invokecc $P10
    unless $P8, sc_while_last
  sc_while_redo:
    set_args '()'
    invokecc $P11
    goto sc_while_next
  sc_while_last:
  print "leaving while"
    returncc
.end
.sub "&statement_control:until"
    get_params "(0, 0)", $P10, $P11
  sc_until_next:
    set_args '()'
    get_results "(0)", $P8
    invokecc $P10
    if $P8, sc_until_last
  sc_until_redo:
    set_args '()'
    invokecc $P11
    goto sc_until_next
  sc_until_last:
    returncc
.end
.sub "&statement_control:cond"
    get_params "(0, 0, 0)", $P10, $P11, $P12
    unless $P10, sc_cond_alt
    set_args "()"
    get_results "(0)", $P9
    invokecc $P11
    get_results "(0)", $P8
    invokecc $P9
    goto sc_cond_post
  sc_cond_alt:
    set_args "()"
    get_results "(0)", $P9
    invokecc $P12
    get_results "(0)", $P8
    invokecc $P9
  sc_cond_post:
    set_returns "(64)", $P8
    returncc
    .return ($P8)
.end
.sub "&statement_control:if"
    get_params "(0, 0, 0)", $P10, $P11, $P12
    unless $P10, sc_if_alt
    set_args "()"
    get_results "(0)", $P9
    invokecc $P11
    get_results "(0)", $P8
    invokecc $P9
    goto sc_if_post
  sc_if_alt:
    set_args "()"
    get_results "(0)", $P9
    invokecc $P12
    get_results "(0)", $P8
    invokecc $P9
  sc_if_post:
    set_returns "(64)", $P8
    returncc
    .return ($P8)
.end
.sub "&statement_control:unless"
    get_params "(0, 0, 0)", $P10, $P11, $P12
    if $P10, sc_unless_alt
    set_args "()"
    get_results "(0)", $P9
    invokecc $P11
    get_results "(0)", $P8
    invokecc $P9
    goto sc_unless_post
  sc_unless_alt:
    set_args "()"
    get_results "(0)", $P9
    invokecc $P12
    get_results "(0)", $P8
    invokecc $P9
  sc_unless_post:
    set_returns "(64)", $P8
    returncc
    .return ($P8)
.end
.sub "&infix:&&"
    get_params "(0, 0)", $P10, $P11
    if $P10, sc__38_38_alt
    set_returns "(64)", $P10
    returncc
  sc__38_38_alt:
    set_args "()"
    get_results "(0)", $P8
    invokecc $P11
    set_returns "(64)", $P8
    returncc
    .return ($P8)
.end
.sub "&infix:||"
    get_params "(0, 0)", $P10, $P11
    unless $P10, sc__124_124_alt
    set_returns "(64)", $P10
    returncc
  sc__124_124_alt:
    set_args "()"
    get_results "(0)", $P8
    invokecc $P11
    set_returns "(64)", $P8
    returncc
    .return ($P8)
.end
.sub "&infix:and"
    get_params "(0, 0)", $P10, $P11
    if $P10, sc_and_alt
    set_returns "(64)", $P10
    returncc
  sc_and_alt:
    set_args "()"
    get_results "(0)", $P8
    invokecc $P11
    set_returns "(64)", $P8
    returncc
    .return ($P8)
.end
.sub "&infix:or"
    get_params "(0, 0)", $P10, $P11
    unless $P10, sc_or_alt
    set_returns "(64)", $P10
    returncc
  sc_or_alt:
    set_args "()"
    get_results "(0)", $P8
    invokecc $P11
    set_returns "(64)", $P8
    returncc
    .return ($P8)
.end
.sub "&nothing"
.end
.sub "&print"
    get_params "(32)", $P10
    $S8 = join "", $P10
    print $S8
    .return (1)
.end
.sub "&say"
    get_params "(32)", $P10
    $S8 = join "", $P10
    print $S8
    print "\n"
    .return (1)
.end
.sub "&system"
    get_params "(0)", $P10
    $P2 = new .Undef
    $S8 = set $P10
    $I8 = spawnw $S8
    $P2 = assign $I8
    .return ($P2)
.end
.sub "&infix:,"
    get_params "(32)", $P10
    .return ($P10)
.end
.sub "&circumfix:[]"
    get_params "(32)", $P10
    $P2 = new .Undef
    $P8 = new .Array
    $P8 = assign $P10
    $P9 = new .Ref, $P8
    $P2 = assign $P9
    .return ($P2)
.end
.sub "&prefix:++"
    get_params "(0)", $P10
    inc $P10
    .return ($P10)
.end
.sub "&prefix:--"
    get_params "(0)", $P10
    dec $P10
    .return ($P10)
.end
.sub "&postfix:++"
    get_params "(0)", $P10
    $P2 = new .Undef
    $P2 = assign $P10
    inc $P10
    .return ($P2)
.end
.sub "&postfix:--"
    get_params "(0)", $P10
    $P2 = new .Undef
    $P2 = assign $P10
    dec $P10
    .return ($P2)
.end
.sub "&prefix:-"
    get_params "(0)", $P10
    $P2 = new .Undef
    $P2 = neg $P10
    .return ($P2)
.end
.sub "&infix:+"
    get_params "(0, 0)", $P10, $P11
    $P2 = new .Undef
    $P2 = add $P10, $P11
    .return ($P2)
.end
.sub '&infix:+|'
.param int a
.param int b
$I0 = bor a, b
.return ($I0)
.end
.sub '&infix:+^'
.param int a
.param int b
$I0 = bxor a, b
.return ($I0)
.end
.sub '&infix:+&' :multi(_,_)
    .param int a
    .param int b
    $I0 = band a, b
    .return ($I0)
.end
.sub '&infix:+<' :multi(_,_)
.param int a
.param int b
$I0 = shl a, b
.return ($I0)
.end
.sub '&infix:+>' :multi(_,_)
.param int a
.param int b
$I0 = shr a, b
.return ($I0)
.end
.sub "&infix:-"
    get_params "(0, 0)", $P10, $P11
    $P2 = new .Undef
    $P2 = sub $P10, $P11
    .return ($P2)
.end
.sub "&infix:*"
    get_params "(0, 0)", $P10, $P11
    $P2 = new .Undef
    $P2 = mul $P10, $P11
    .return ($P2)
.end
.sub "&infix:/"
    get_params "(0, 0)", $P10, $P11
    $P2 = new .Undef
    $P2 = div $P10, $P11
    .return ($P2)
.end
.sub "&infix:%"
    get_params "(0, 0)", $P10, $P11
    $P2 = new .Undef
    $P2 = mod $P10, $P11
    .return ($P2)
.end
.sub '&infix:~~'
.param pmc topic
.param pmc x
.return x.ACCEPTS(topic)
.end
.sub '&infix:!~~'
.param pmc topic
.param pmc x
.return x.REJECTS(topic)
.end
.sub "&infix:~"
    get_params "(0, 0)", $P10, $P11
    $P2 = new .Undef
    $P2 = concat $P10, $P11
    .return ($P2)
.end
.sub "&prefix:!"
    get_params "(0)", $P10
    $P2 = new .Undef
    $P2 = not $P10
    .return ($P2)
.end
.sub "&not"
    get_params "(0)", $P10
    $P2 = new .Undef
    $P2 = not $P10
    .return ($P2)
.end
.sub "&infix:<"
    get_params "(0, 0)", $P10, $P11
    $P2 = new .Undef
    $I8 = set $P10
    $I9 = set $P11
    $I8 = islt $I8, $I9
    $P2 = assign $I8
    .return ($P2)
.end
.sub "&infix:<="
    get_params "(0, 0)", $P10, $P11
    $P2 = new .Undef
    $I8 = set $P10
    $I9 = set $P11
    $I8 = isle $I8, $I9
    $P2 = assign $I8
    .return ($P2)
.end
.sub "&infix:>"
    get_params "(0, 0)", $P10, $P11
    $P2 = new .Undef
    $I8 = set $P10
    $I9 = set $P11
    $I8 = isgt $I8, $I9
    $P2 = assign $I8
    .return ($P2)
.end
.sub "&infix:>="
    get_params "(0, 0)", $P10, $P11
    $P2 = new .Undef
    $I8 = set $P10
    $I9 = set $P11
    $I8 = isge $I8, $I9
    $P2 = assign $I8
    .return ($P2)
.end
.sub "&infix:=="
    get_params "(0, 0)", $P10, $P11
    $P2 = new .Undef
    $I8 = set $P10
    $I9 = set $P11
    $I8 = iseq $I8, $I9
    $P2 = assign $I8
    .return ($P2)
.end
.sub "&infix:!="
    get_params "(0, 0)", $P10, $P11
    $P2 = new .Undef
    $I8 = set $P10
    $I9 = set $P11
    $I8 = isne $I8, $I9
    $P2 = assign $I8
    .return ($P2)
.end
.sub "&infix:lt"
    get_params "(0, 0)", $P10, $P11
    $P2 = new .Undef
    $S8 = set $P10
    $S9 = set $P11
    $I8 = islt $S8, $S9
    $P2 = assign $I8
    .return ($P2)
.end
.sub "&infix:le"
    get_params "(0, 0)", $P10, $P11
    $P2 = new .Undef
    $S8 = set $P10
    $S9 = set $P11
    $I8 = isle $S8, $S9
    $P2 = assign $I8
    .return ($P2)
.end
.sub "&infix:gt"
    get_params "(0, 0)", $P10, $P11
    $P2 = new .Undef
    $S8 = set $P10
    $S9 = set $P11
    $I8 = isgt $S8, $S9
    $P2 = assign $I8
    .return ($P2)
.end
.sub "&infix:gt"
    get_params "(0, 0)", $P10, $P11
    $P2 = new .Undef
    $S8 = set $P10
    $S9 = set $P11
    $I8 = isge $S8, $S9
    $P2 = assign $I8
    .return ($P2)
.end
.sub "&infix:eq"
    get_params "(0, 0)", $P10, $P11
    $P2 = new .Undef
    $S8 = set $P10
    $S9 = set $P11
    $I8 = iseq $S8, $S9
    $P2 = assign $I8
    .return ($P2)
.end
.sub "&infix:ne"
    get_params "(0, 0)", $P10, $P11
    $P2 = new .Undef
    $S8 = set $P10
    $S9 = set $P11
    $I8 = isne $S8, $S9
    $P2 = assign $I8
    .return ($P2)
.end
.sub "&prefix:?^"
    get_params "(0)", $P10
    $P2 = new .Undef
    $P2 = bnot $P10
    .return ($P2)
.end
.sub "&postcircumfix:{}"
    get_params "(0, 0)", $P10, $P11
    $S8 = set $P11
    $P2 = set $P10[$S8]
    .return ($P2)
.end
.sub "&postcircumfix:[]"
    get_params "(0, 0)", $P10, $P11
    $I8 = set $P11
    $P2 = set $P10[$I8]
    .return ($P2)
.end
.sub "&prefix:+"
    get_params "(0)", $P10
    $P2 = new .Undef
    $N8 = set $P10
    $P2 = set $N8
    .return ($P2)
.end
.sub "&prefix:~"
    get_params "(0)", $P10
    $P2 = new .Undef
    $S8 = set $P10
    $P2 = set $S8
    .return ($P2)
.end
.sub "&int"
    get_params "(0)", $P10
    $P2 = new .Undef
    $I8 = set $P10
    $P2 = set $I8
    .return ($P2)
.end
.sub "&true"
    get_params "(0)", $P10
    $P2 = new .Undef
    $P2 = set 1
    if $P10, true_pmc_is_true
    $P2 = set 0
  true_pmc_is_true:
    .return ($P2)
.end
.sub "&chars"
    get_params "(0)", $P10
    $P2 = new .Undef
    $S8 = set $P10
    $I8 = length $S8
    $P2 = assign $I8
    .return ($P2)
.end
.sub "&bytes"
    get_params "(0)", $P10
    $P2 = new .Undef
    $S8 = set $P10
    $I8 = bytelength $S8
    $P2 = assign $I8
    .return ($P2)
.end
.sub "&prefix:\\"
    get_params "(0)", $P10
    $P8 = new .Ref, $P10
    .return ($P2)
.end
.sub "&infix:=>"
    get_params "(0, 0)", $P10, $P11
    $P2 = new .Pair
    $P2[$P10] =  $P11
    .return ($P2)
.end
.sub "&infix:.."
    get_params "(0, 0)", $P10, $P11
    $I8 = set $P10
    $P2 = new .ResizablePMCArray
  range_next:
    lt_num $P11, $I8, range_end
    push $P2, $I8
    inc $I8
    goto range_next
  range_end:
    .return ($P2)
.end
.sub "&substr"
    get_params "(0, 0, 0)", $P10, $P11, $P12
    $S8 = set $P10
    $I8 = set $P11
    $I9 = set $P12
    print "substr: "
    print $S8
    print "\n"
    $S9 = substr $S8, $I8, $I9
    print "substr: "
    print $S9
    print "\n"
    $P2 = new .Undef
    $P2 = set $S9
    print "substr: "
    print $P2
    print "\n"
    .return ($P2)
.end
.sub "&chr"
    get_params "(0)", $P10
    $P2 = new .Undef
    $I8 = set $P10
    $S8 = chr $I8
    $P2 = assign $S8
    .return ($P2)
.end
.sub "&ord"
    get_params "(0)", $P10
    $P2 = new .Undef
    $S8 = set $P10
    $I8 = ord $S8
    $P2 = assign $I8
    .return ($P2)
.end
.sub "&infix:x"
    get_params "(0, 0)", $P10, $P11
    $P2 = new .Undef
    $S8 = set $P10
    $I8 = set $P11
    $S8 = repeat $S8, $I8
    $P2 = assign $S8
    .return ($P2)
.end
.sub "&lc"
    get_params "(0)", $P10
    $P2 = new .Undef
    $S8 = set $P10
    $S8 = downcase $S8
    $P2 = assign $S8
    .return ($P2)
.end
.sub "&uc"
    get_params "(0)", $P10
    $P2 = new .Undef
    $S8 = set $P10
    $S8 = upcase $S8
    $P2 = assign $S8
    .return ($P2)
.end
.sub "&undef"
    $P2 = new .Undef
    .return ($P2)
.end
.sub "&undefine"
    get_params "(0)", $P10
    $P8 = new .Undef
    $P10 = assign $P8
    .return ($P10)
.end
.sub "&defined"
    get_params "(0)", $P10
    $P2 = new .Undef
    $P8 = set $P10
    $I8 = defined $P8
    $P2 = assign $I8
    .return ($P2)
.end
.sub "&clone"
    get_params "(0)", $P10
    $P2 = new .Undef
    $P2 = clone $P10
    .return ($P2)
.end
.sub "&pop"
    get_params "(0)", $P10
    $P2 = pop $P10
    .return ($P2)
.end
.sub "&push"
    get_params "(0, 0)", $P10, $P11
    push $P10, $P11
    .return (1)
.end
.sub "&delete"
    get_params "(0, 0)", $P10, $P11
    $P2 = set $P10[$P11]
    delete $P10[$P11]
    .return ($P2)
.end
.sub "&exists"
    get_params "(0, 0)", $P10, $P11
    $I8 = exists $P10[$P11]
    $P2 = new .Undef
    $P2 = set $I8
    .return ($P2)
.end
.sub "&join"
    get_params "(0, 0)", $P10, $P11
    $P2 = new .Undef
    $S8 = set $P10
    $S9 = join $S8, $P11
    $P2 = assign $S9
    .return ($P2)
.end
.namespace ['Perl6::Internals']
.sub "&symbolic_deref"
    get_params "(0, 32)", $P10, $P11
    $S8 = join "::", $P11
    $S9 = set $P10
    $S8 = concat $S9, $S8
    $P2 = find_name $S8
    .return ($P2)
.end
.sub "&exit"
    get_params "(0)", $P10
    $P8 = find_global "main", "&*END"
    set_args "()"
    invokecc $P8
    $I8 = set $P10
    exit $I8
.end
.sub "&sleep"
    get_params "(0)", $P10
    $N8 = set $P10
    sleep $N8
.end
.sub "&compile_pir"
    get_params "(0)", $P10
    $S8 = set $P10
    $P8 = compreg "PIR"
    .return ($P9)
.end
.sub "&eval_pir"
    get_params "(0)", $P10
    $P8 = open "temp.pl", ">"
    print $P8, $P10
    close $P8
    $P8 = open "pugs -CPIR temp.pl", "-|"
    $P2 = new .Undef
    $P2 = set ""
  eval_pir_read_pre_next:
    $S8 = readline $P8
    ne $S8, ".sub \"init\" :main :anon\n", eval_pir_read_pre_next
  eval_pir_read_next:
    $S8 = readline $P8
    eq $S8, ".end\n", eval_pir_done
    $P2 = concat $S8
    if $P8, eval_pir_read_next
  eval_pir_done:
    close $P8
    .return ($P2)
.end
.namespace ['main']
.sub "&abs"
    get_params "(0)", $P10
    $P2 = new .Undef
    $P2 = assign $P10
    abs $P10
    .return ($P2)
.end
.sub "&exp"
    get_params "(0)", $P10
    $P2 = new .Undef
    $N8 = set $P10
    $N8 = exp $N8
    $P2 = assign $N8
    .return ($P2)
.end
.sub "&ln"
    get_params "(0)", $P10
    $P2 = new .Undef
    $N8 = set $P10
    $N8 = ln $N8
    $P2 = assign $N8
    .return ($P2)
.end
.sub "&log2"
    get_params "(0)", $P10
    $P2 = new .Undef
    $N8 = set $P10
    $N8 = log2 $N8
    $P2 = assign $N8
    .return ($P2)
.end
.sub "&log10"
    get_params "(0)", $P10
    $P2 = new .Undef
    $N8 = set $P10
    $N8 = log10 $N8
    $P2 = assign $N8
    .return ($P2)
.end
.sub "&sqrt"
    get_params "(0)", $P10
    $P2 = new .Undef
    $N8 = set $P10
    $N8 = sqrt $N8
    $P2 = assign $N8
    .return ($P2)
.end
.sub "&cos"
    get_params "(0)", $P10
    $P2 = new .Undef
    $P2 = cos $P10
    .return ($P2)
.end
.sub "&tan"
    get_params "(0)", $P10
    $P2 = new .Undef
    $P2 = tan $P10
    .return ($P2)
.end
.sub "&sec"
    get_params "(0)", $P10
    $P2 = new .Undef
    $P2 = sec $P10
    .return ($P2)
.end
.sub "&asin"
    get_params "(0)", $P10
    $P2 = new .Undef
    $P2 = asin $P10
    .return ($P2)
.end
.sub "&acos"
    get_params "(0)", $P10
    $P2 = new .Undef
    $P2 = acos $P10
    .return ($P2)
.end
.sub "&atan"
    get_params "(0)", $P10
    $P2 = new .Undef
    $P2 = atan $P10
    .return ($P2)
.end
.sub "&asec"
    get_params "(0)", $P10
    $P2 = new .Undef
    $P2 = asec $P10
    .return ($P2)
.end
.sub "&sinh"
    get_params "(0)", $P10
    $P2 = new .Undef
    $P2 = sinh $P10
    .return ($P2)
.end
.sub "&ceil"
    get_params "(0)", $P10
    $P2 = new .Undef
    $N8 = set $P10
    $N8 = ceil $N8
    $P2 = assign $N8
    .return ($P2)
.end
.sub "&floor"
    get_params "(0)", $P10
    $P2 = new .Undef
    $N8 = set $P10
    $N8 = floor $N8
    $P2 = assign $N8
    .return ($P2)
.end
.sub "&fact"
    get_params "(0)", $P10
    $P2 = new .Undef
    $I8 = set $P10
    $I8 = fact $I8
    $P2 = assign $I8
    .return ($P2)
.end
.sub "&gcd"
    get_params "(0, 0)", $P10, $P11
    $P2 = new .Undef
    $I8 = set $P10
    $I9 = set $P11
    $I8 = gcd $I8, $I9
    $P2 = assign $I8
    .return ($P2)
.end
.sub "&lcm"
    get_params "(0, 0)", $P10, $P11
    $P2 = new .Undef
    $I8 = set $P10
    $I9 = set $P11
    $I8 = lcm $I8, $I9
    $P2 = assign $I8
    .return ($P2)
.end
.sub "&pow"
    get_params "(0, 0)", $P10, $P11
    $P2 = new .Undef
    $N8 = set $P10
    $N9 = set $P11
    $N8 = pow $N8, $N9
    $P2 = assign $N8
    .return ($P2)
.end
.sub "&time"
    $P2 = new .Undef
    $N8 = time
    $P2 = set $N8
    sub $P2, 9.466848e8
    .return ($P2)
.end
.sub "&split"
    get_params "(0, 0)", $P10, $P11
    $P2 = new .Undef
    $S8 = set $P10
    $S9 = set $P11
    ne $S8, "\n", split_normally
    $P2 = new .Array
    $I8 = set 0
    $I11 = length $S8
  split_loop:
    $I9 = index $S9, $S8, $I8
    lt $I9, 0, split_last
    $I10 = sub $I9, $I8
    $S10 = substr $S9, $I8, $I10
    $I8 = add $I9, $I11
    push $P2, $S10
    goto split_loop
  split_last:
    $S10 = substr $S9, $I8
    push $P2, $S10
    goto split_done
  split_normally:
    $P8 = split $S8, $S9
    $P2 = assign $P8
  split_done:
    .return ($P2)
.end
.sub "&True"
    .return (1)
.end
.sub "&False"
    .return (0)
.end
.sub "&Bool::True"
    .return (1)
.end
.sub "&Bool::False"
    .return (0)
.end
