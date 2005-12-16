module Parse::Rule::Match;

class Match {
    has $.backtrack;
    has $.input;
    has $.pos;
    has $.marks;

    method with(:$backtrack, :$input, :$pos, :$marks) {
        Match.new(
            backtrack => $backtrack // $.backtrack,
            input     => $input // $.input,
            pos       => $pos // $.pos,
            marks     => $marks // $.marks,
        );
    }

    submethod BUILD() {
        $.marks = [];
    }
}

sub fmatch($r, $m, $c) {
    match($r: $m, $c);
}

multi match(Literal $rule: $match, $continue) {
    if $match.input.substr($match.pos, $rule.string.chars) eq $rule.string {
        $continue($match.with(
            pos       => $match.pos + $rule.string.chars,
        ));
    }
    else {
        $match.backtrack()();
    }
}

multi match(Union $rule: $match, $continue) {
    fmatch($rule.left, $match.with(
        backtrack => { fmatch($rule.right, $match, $continue) },
    ), $continue);
}

multi match(Concat $rule: $match, $continue) {
    fmatch($rule.left, $match, -> $m {
        fmatch($rule.right, $m, $continue);
    });
}

multi match(Mark $rule: $match, $continue) {
    fmatch($rule.exp, $match.with(
        marks => [ [ $rule, $match.backtrack ], *$match.marks ],
    ), $continue);
}

multi match(Cut $rule: $match, $continue) {
    my $back;
    for *$match.marks -> $mark {
        if $rule.mark === $mark[0] {
            $back = $mark[1];
            last;
        }
    }
    die "No mark to cut to" unless $back.defined;
    
    $continue($match.with(
        backtrack => $back,
    ));
}

# vim: ft=perl6 :
