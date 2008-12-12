
use strict;
use warnings;
use English;
use feature qw(say);
use Data::Dumper;

use lib 'lib';
use STD;
use Syntax::Highlight::Perl6;

my $h = Syntax::Highlight::Perl6->new(
    text => 'my $foo;'
);

say $h->snippet_html;

say $h->simple_html;

say $h->full_html;

say $h->ansi_text;

say Dumper($h->parse_trees);
