#!perl6

use v6;

=head1 Accessing subroutine arguments

You have written a function that takes arguments supplied by its caller and
you need to access those arguments

=cut

sub perl5 {
    my ($x) = @_;
    say $x;
}

perl5('old-fashioned');

sub parameters ($foo) { say $foo }
parameters('some parameter');

sub whole (@names, %flags) {
    for @names -> $name{
        say $name;
    }
    # arg!  Shouldn't need parens!
    for (%flags.kv) -> $key, $value {
        say "$key => $value";
    }
}
my @stuff = ('array', 'elements');
my %flags = ('hash' => 'elements', 'are' => 'pairs');
whole(@stuff, %flags);

sub optional ($required, ?$optional) {
    my $second_arg = $optional // 'Told you it was optional!';
    say $required;
    say $second_arg;
}

optional('this');
optional('this', 'that');

sub named_params ($first, +$second, +$third) {
    say $first, $second, $third;
}

named_params(1, second => 2, third => 3);

sub transport ($planet, *@names) {
    say "Transporting to $planet:";
    for @names -> $name {
        say "\t$name";
    }
}
transport('Magrathea', 'Arthur', 'Ford', 'Ovid');

sub typed (Int $val) {
    say $val++;
}
typed(3);
