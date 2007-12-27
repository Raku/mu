
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

.sub main :main
    sub1()
    sub2()

    sub3()
    .const .Sub sub2 = "sub2"
    .const .Sub sub3 = "sub3"
    sub2.set_outer(sub3)
    $P1 = newclosure sub2
    $P1()
.end
