#########################

use Test::More tests => 7;
use Test::Exception;

#this is needed for now before using my module
#STD is should be used first otherwise you'll see this error after new()
#Can't call method "bless" on an undefined value at
use STD;

#it can be used...
BEGIN { 
    use_ok('Syntax::Highlight::Perl6'); 
}

#text option is a required option in new()
dies_ok { Syntax::Highlight::Perl6->new(); } 'text option is required in new()';

#check if new(...) works
my $p = Syntax::Highlight::Perl6->new(
    text => 'my $foo;'
);
ok(defined $p, "new() returned something");
ok( $p->isa('Syntax::Highlight::Perl6'), " And it is the right classd");

my $str = $p->snippet_html;
ok( defined $str, 'snippet_html returned something');
like( $str, '/<pre>/i', 'snippet_html contains <pre> tag'); 
unlike( $str, '/<html>/i', 'snippet_html should not contain <html>');
