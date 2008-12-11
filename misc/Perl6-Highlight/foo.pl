
use strict;
use warnings;
use English;
use feature qw(say);

use lib 'lib';
use Perl6::Highlight;

my $foo = Perl6::Highlight->new(text => 'my');
say $foo->snippet_html;
