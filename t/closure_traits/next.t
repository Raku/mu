use v6-alpha;

use Test;

plan 12;

# L<S04/Closure traits/NEXT executes "only if" 
#   "end of the loop block" or "explicit next">
{
    my $str;
    for 1..5 {
        NEXT { $str ~= ':' }
        next if $_ % 2 == 1;
        $str ~= $_;
    }
    is $str, ':2::4::', 'NEXT called by both next and normal falling out';
}

# NEXT is positioned at the bottom:
{
    my $str;
    for 1..5 {
        next if $_ % 2 == 1;
        $str ~= $_;
        NEXT { $str ~= ':' }
    }
    is $str, ':2::4::', 'NEXT called by both next and normal falling out';
}

# NEXT is positioned in the middle:
{
    my $str;
    for 1..5 {
        next if $_ % 2 == 1;
        NEXT { $str ~= ':' }
        $str ~= $_;
    }
    is $str, ':2::4::', 'NEXT called by both next and normal falling out';
}

# NEXT is evaluated even at the last iteration
{
    my $str;
    for 1..2 {
        NEXT { $str ~= 'n'; }
        LAST { $str ~= 'l'; }
    }
    is $str, 'nnl', 'NEXT are LAST blocks may not be exclusive';
}

# L<S04/Closure traits/NEXT "not executed" if exited
#   "via any exception other than" next>

{
    my $str;
    eval q{
        for 1..5 {
            NEXT { $str ~= $_ }
            die if $_ > 3;
        }
    };
    is $str, '123', "die didn't trigger NEXT \{}";
}

{
    eval_is q{
        my $str;
        for 1..5 {
            NEXT { $str ~= $_ }
            leave if $_ > 3;
        }
        $str;
    }, '123', "leave didn't trigger NEXT \{}";
}

{
    my $str;
    my sub foo {
        for 1..5 {
            NEXT { $str ~= $_ }
            return if $_ > 3;
        }
    }
    foo();
    is $str, '123', "return didn't trigger NEXT \{}";
}

# L<S04/Closure traits/last bypasses "NEXT blocks">
{
    my $str;
    for 1..5 {
        NEXT { $str ~= $_; }
        last if $_ > 3;
    }
    is $str, '123', "last bypass NEXT \{}";
}

# L<S04/Closure traits/NEXT "before any LEAVE">

{
    my $str;
    for 1..2 {
        NEXT { $str ~= 'n' }
        LEAVE { $str ~= 'l' }
    }
    is $str, 'nlnl', 'NEXT {} ran before LEAVE {} (1)';
}

# reversed order
{
    my $str;
    for 1..2 {
        LEAVE { $str ~= 'l' }
        NEXT { $str ~= 'n' }
    }
    is $str, 'nlnl', 'NEXT {} ran before LEAVE {} (2)';
}

die "Infinite loop - look at this later";
# L<S04/Closure traits/NEXT "at loop continuation time">

# L<http://groups.google.com/group/perl.perl6.language/msg/07370316d32890dd>

{
    my $str;
    my $n = 0;
    while $n < 5 {
        NEXT { ++$n }
        NEXT { $str ~= $n }
    }
    is $str, '01234', 'NEXT {} ran in reversed order';
}

{
    my $str;
    loop (my $n = 0; $n < 5; ++$n) {
       NEXT { $str ~= $n }
    }
    is $str, '01234';
}
