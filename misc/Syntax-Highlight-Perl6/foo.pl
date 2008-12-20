
use strict;
use warnings;
use English;
use feature qw(say);
use Data::Dumper;

use lib 'lib';
use Syntax::Highlight::Perl6;

my $h = Syntax::Highlight::Perl6->new(
    text => 'my $foo;'
);

say $h->snippet_html;
say "_-_" x 30;
say $h->simple_html;
say "_-_" x 30;
say $h->full_html;
say "_-_" x 30;
say $h->ansi_text;
say "_-_" x 30;
say Dumper($h->tokens);
say "_-_" x 30;
say $h->vim_html;
