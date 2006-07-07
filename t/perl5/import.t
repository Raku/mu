use v6-alpha;

use Test;
plan 1;

=pod

P5 module import test

=cut

unless try({ eval("1", :lang<perl5>) }) {
    skip_rest;
    exit;
}

eval q[
use perl5:Text::Wrap 'wrap';
is(wrap('foo', 'bar', 'baz'), 'foobaz', "import p5 module");
] or die $!.perl;
