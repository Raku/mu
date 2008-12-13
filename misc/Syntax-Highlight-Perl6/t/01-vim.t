use Test::More tests => 3;
use Test::Exception;

#it can be used...
BEGIN { 
    use_ok('Syntax::Highlight::Perl6'); 
}

#tests for vim_html()
my $p = Syntax::Highlight::Perl6->new(
    text => 'my $foo;'
);

my $vim = $p->vim_html;
ok( defined $vim, 'vim_html returned something');
like( $vim, '/foo/i', 'vim_html contain the word foo');
