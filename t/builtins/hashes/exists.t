use v6-alpha;
use Test;
plan 8;

# L<S29/"Hash"/"=item exists">

sub gen_hash {
    my %h{'a'..'z'} = (1..26);
    return %h;
};

{
    my %h1 = gen_hash;
    my %h2 = gen_hash;

    my $b = %h1<b>;
    is (exists %h1, 'a'), 1, "Test existance for singe key. (Indirect notation)";
    is (%h1.exists('a')), 1, "Test existance for singe key. (method call)";
};

{
    my %h;
    %h<none> = 0;
    %h<one> = 1;
    %h<nothing> = undef;
    is %h.exists('none'),     1,  "Existance of single key with 0 as value: none";
    is %h.exists('one'),      1,  "Existance of single key: one";
    is %h.exists('nothing'),  1,  "Existance of single key with undef as value: nothing";
    is defined(%h<none>),     1,  "Defined 0 value for key: none";
    is defined(%h<one>),      1,  "Defined 1 value for key: one";
    is defined(%h<nothing>),  '', "NOT Defined value for key: nothing";
}
