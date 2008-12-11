
use strict;
use warnings;
use English;
use feature qw(say);

use lib 'lib';
use STD;
use Perl6::Highlight;

my $foo = Perl6::Highlight->new(text => 'my $foo;');
#say $foo->snippet_html;
#say $foo->simple_html;
say $foo->full_html;
