
.sub sub1 :lex
    .local pmc x
    .lex "$x", x
    $P1 = new 'Integer'
    $P1 = 42
    x = $P1
    say "# in sub1"
.end

.sub sub2 :lex :outer(sub1)
    .local pmc x
    find_lex x, "$x"
    say "# in sub2"
    $P1 = x
    say $P1
.end

.sub sub3 :lex
    .local pmc x
    .lex "$x", x
    $P1 = new 'Integer'
    $P1 = 123
    x = $P1
    say "# in sub3"
.end

.sub sub4 :lex :outer(sub3)
    .local pmc x
    find_lex x, "$x"
    say "# in sub4"
    $P1 = x
    say $P1
.end

.sub main :main
    sub1()
    sub2()

    .const .Sub sub2 = "sub2"
    .const .Sub sub3 = "sub3"
    .const .Sub sub4 = "sub4"
    sub2.set_same_outer(sub4)
    sub3()
    sub4()
    say "new closure"
    $P1 = newclosure sub2
    say "call it"
    $P1()
.end
