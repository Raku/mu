#########################

use Test::More tests => 36;
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
ok(defined Syntax::Highlight::Perl6->new(text => q{}), 'text option can be empty');

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

#tests for tokens()
my @tokens = $p->tokens;
ok( @tokens, 'tokens returned an array');
ok( $#tokens > 0, 'and the tokens has some elements in it');  
isa_ok( $tokens[0], 'HASH', '$tokens[0] returned a hash');
my %rec = %{$tokens[0]};
ok(defined keys %rec, '%rec has one or keys');
ok(defined $rec{buffer}, '%rec has a buffer');
ok(defined $rec{last_pos}, '%rec has a last_pos');
ok(defined $rec{rule}, '%rec has a rule');
ok(defined $rec{tree}, '%rec has a tree');

#tests for static behavior between different instances
my $q = Syntax::Highlight::Perl6->new(
    text => q{my $bar = "&<>";}
);
like( $q->snippet_html, '/bar/i', 'second instance worked perfectly');
like( $p->snippet_html, '/foo/i', 'and first instance is not affected');

#tests for correct _escape_html behavior
like( $q->snippet_html, '/&lt;&gt;/', 
    'snippet_html & _escape_html works');
like( $q->snippet_html, '/&amp;/', 
    'snippet_html & _escape_html works');
like( $q->simple_html, '/&quot;/', 
    'simple_html html escaping works');
like( $q->full_html, '/&lt;&gt;/',
    'full_html html escaping works');
