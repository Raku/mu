#########################

use Test::More tests => 23;
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
ok(defined $p, 'new() returned something');
isa_ok( $p, 'Syntax::Highlight::Perl6', ' And it is the right class');

#tests for snippet_html()
my $snippet = $p->snippet_html;
ok( defined $snippet, 'snippet_html returned something');
like( $snippet, '/<pre>/i', 'snippet_html contains <pre> tag'); 
unlike( $snippet, '/<html>/i', 'snippet_html should not contain any <html>');
like( $snippet, '/foo/i', 'snippet_html should contain the word foo');

#tests for simple_html()
my $simple = $p->simple_html;
ok( defined $simple, 'simple_html returned something');
like( $simple, '/<pre>/i', 'simple_html should have <pre> tags'); 
like( $simple, '/<html>/i', 'simple_html should contain <html>');
unlike( $simple, '/text\/javascript/i', 'simple_html should not contain any JS');
like( $simple, '/foo/i', 'simple_html should contain the word foo');

#tests for full_html()
my $full = $p->full_html;
ok( defined $full, 'full_html returned something');
like( $full, '/<pre>/i', 'full_html should have <pre> tags'); 
like( $full, '/<html>/i', 'full_html should contain <html>');
like( $full, '/text\/javascript/i', 'full_html should contain JS');
like( $full, '/foo/i', 'full_html should contain the word foo');

#tests for ansi_text()
my $ansi = $p->ansi_text;
ok( defined $ansi, 'ansi_text returned something');
like( $ansi, '/\033\[.+?m/i', 'ansi_text should contain ansi color escape sequences');
like( $ansi, '/foo/i', 'ansi_text should contain the word foo');

#tests for parse_trees()
my $ptree = $p->parse_trees;
ok( defined $ptree, 'parse_trees returned something');
isa_ok( $ptree, 'ARRAY', 'parse_trees returned an array');
