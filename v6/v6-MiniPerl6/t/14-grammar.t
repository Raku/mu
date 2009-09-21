use v6-alpha;

class Main {
    say '1..4';
    my $m := MiniPerl6::Grammar.var_ident( '$abc', 0);
    say '# Ast is:        ', $m.perl;
    say '# code is:  ', ($$m).perl;
    if (($$m).emit) eq 'v_abc' {
        say 'ok 1';
    }
    else {
        say 'not ok 1';
    }

    $m := MiniPerl6::Grammar.val_buf( '"abc"', 0);
    say '# Ast is:        ', $m.perl;
    say '# code is:  ', ($$m).perl;
    if (($$m).emit) eq '"abc"' {
        say 'ok 2';
    }
    else {
        say 'not ok 2';
    }

}

