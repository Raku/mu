module Parse::Rule::Match;

class Match {
    has $.backtrack;
    has $.input;
    has $.pos;
    has $.marks;

    submethod BUILD() {
        $.marks = [];
    }
}

# multis are too flaky in pugs to handle
sub match($rule, $match, $continue) {
  given $rule {
    when Literal {
        if $match.input.substr($match.pos, $rule.string.chars) eq $rule.string {
            $continue.goto($match.clone(
                pos => $match.pos + $rule.string.chars,
            ));
        }
        else {
            $match.backtrack.goto();
        }
    }

    when Union {
        &match.goto($rule.either, $match.clone(
            backtrack => { &match.goto($rule.or, $match, $continue) },
        ), $continue);
    }

    when Concat {
        &match.goto($rule.first, $match, -> $m {
            &match.goto($rule.then, $m, $continue);
        });
    }

    when Mark {
        my $nmatch = $match.clone(marks => [ [ $rule, $match.backtrack ] ]);
        my $marks = $nmatch.marks;
        &match.goto($rule.exp, $match.clone(
            marks => [ [ $rule, $match.backtrack ] ],
        ), $continue);
    }

    when Cut {
        my $back;
        my $marks = $match.marks;
        for $match.marks -> $mark {
            if $rule.mark === $mark[0] {
                $back = $mark[1];
                last;
            }
        }
        die "No mark to cut to" unless $back.defined;
        
        $continue.goto($match.clone(
            backtrack => $back,
        ));
    }
  }
}

# vim: ft=perl6 :
