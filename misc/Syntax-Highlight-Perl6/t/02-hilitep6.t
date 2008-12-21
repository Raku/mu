use strict;
use warnings;
use English qw(-no_match_vars); # to avoid regexp performance penalty
use feature qw(say);
use Test::More tests => 21;
use IPC::Open2;
use File::Spec;
use Carp;

my $hilitep6 = File::Spec->catfile( qw(blib script hilitep6) );
if (not -e $hilitep6) {
    $hilitep6 = File::Spec->catfile( qw(bin hilitep6) )
}

sub run_script {
    my $args = shift;
    open2(*README, *WRITEME, "perl -Ilib $hilitep6 $args");
    print WRITEME q{my $foo="&<>";};
    close WRITEME;
    local $INPUT_RECORD_SEPARATOR = undef;   #enable localized slurp mode
    my $output = <README>;
    close(README);
    return $output;
}

#tests for --help
#XXX-implement

#tests for --snippet-html
my $snippet = run_script '--snippet-html=-';
ok( defined $snippet, 'snippet_html returned something');
like( $snippet, '/<pre>/i', 'snippet_html contains <pre> tag'); 
unlike( $snippet, '/<html>/i', 'snippet_html should not contain any <html>');
like( $snippet, '/foo/i', 'snippet_html should contain the word foo');

#tests for --simple-html
my $simple = run_script '--simple-html=-';
ok( defined $simple, 'simple_html returned something');
like( $simple, '/<pre>/i', 'simple_html should have <pre> tags'); 
like( $simple, '/<html>/i', 'simple_html should contain <html>');
unlike( $simple, '/text\/javascript/i', 'simple_html should not contain any JS');
like( $simple, '/foo/i', 'simple_html should contain the word foo');

#tests for --full_html
my $full = run_script '--full-html=-';
ok( defined $full, 'full_html returned something');
like( $full, '/<pre>/i', 'full_html should have <pre> tags'); 
like( $full, '/<html>/i', 'full_html should contain <html>');
like( $full, '/text\/javascript/i', 'full_html should contain JS');
like( $full, '/foo/i', 'full_html should contain the word foo');

#tests for --ansi-text
my $ansi = run_script '--ansi-text=-';
ok( defined $ansi, 'ansi_text returned something');
like( $ansi, '/\033\[.+?m/i', 'ansi_text should contain ansi color escape sequences');
like( $ansi, '/foo/i', 'ansi_text should contain the word foo');

#tests for correct html escaping behavior
like( $snippet, '/&lt;&gt;/', 
    'snippet_html & html escaping works');
like( $snippet, '/&amp;/', 
    'snippet_html & html escaping works');
like( $simple, '/&quot;/', 
    'simple_html html escaping works');
like( $full, '/&lt;&gt;/',
    'full_html html escaping works');
=cut
