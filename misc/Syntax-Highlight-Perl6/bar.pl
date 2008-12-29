
use strict;
use warnings;
use feature qw(say);
use Data::Dumper;

use lib 'lib';
use Foo;

my @texts = (
    'my $foo;', 
    'my $fo;');

for my $text (@texts) {
    my $p = Foo->new(text => $text,);
    $p->parse;
    say "_-_" x 30;
}
