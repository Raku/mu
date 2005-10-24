#!/usr/bin/perl6

use v6;

=head1 In one array and out the other.

You want to find elements in one array but not in another.

=cut

# XXX Q::Sp has eigenstates(any(@a) == none(@b)), but I recall having read that
# junctions in Perl 6 have nothing to do with set calculations.

my @a = (1,2,3,4,5);
my @b = (3,5);

# for numbers
my @aonly;
for @a -> $elem {
    @aonly.push($elem) if $elem == none(@b);
}
say @aonly;

@a = (1,2,3,4,5);
@b = (3,5);

# for numbers
@aonly = grep { $_ == none(@b) } @a;
say @aonly;
# for strings
@a = ('foo', 'bar', 'baz');
@b = ('bar');
for @a -> $elem {
    say $elem if $elem eq none(@b);
}

# if you're not sure which type of data you have
@a = ('foo', 1, 2, 3, 4, 5, 'bar', 'baz');
@b = ('bar', 3, 5);
for @a -> $elem {
    say $elem if $elem ~~ none(@b);
}

@aonly = ();
@a = (1,2,3,4,5);
@b = (3,5);

# from Juerd

@aonly = gather {
    for @a -> $elem {
        take $elem if $elem == none(@b);
    }
};
print @aonly;
say @aonly;

@aonly = ();
@aonly = gather {
    $_ == none(@b) and take for @a;
};
say @aonly;
