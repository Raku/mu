
use strict;
use warnings;
use English;
use feature qw(say);

use lib 'lib';
use STD;
use Syntax::Highlight::Perl6;

my $foo = Syntax::Highlight::Perl6->new(text => 'my $foo;');
say $foo->snippet_html;
say $foo->simple_html;
say $foo->full_html;
say $foo->ansi;
say $foo->yaml;
