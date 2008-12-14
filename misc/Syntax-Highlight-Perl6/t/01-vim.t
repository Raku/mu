use English qw( -no_match_vars ) ;  # Avoids regex performance penalty
use Test::More;

#This is an optional package so we're going to test for it
eval { require Text::VimColor; };
if($EVAL_ERROR) {
    plan skip_all => 'Text::VimColor is not installed';
} else {
    plan tests => 2;
}

#tests for vim_html()
my $p = Syntax::Highlight::Perl6->new(
    text => 'my $foo;'
);

my $vim = $p->vim_html;
ok( defined $vim, 'vim_html returned something');
like( $vim, '/foo/i', 'vim_html contain the word foo');
