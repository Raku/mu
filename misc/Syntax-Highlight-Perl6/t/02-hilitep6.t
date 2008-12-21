use strict;
use warnings;
use English qw(-no_match_vars); # to avoid regexp performance penalty
use feature qw(say);
use Test::More tests => 30;
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
my $help = run_script '--help';
ok(defined $help, '--help returned something');
like( $help, '/USAGE/', '--help contains USAGE');
like( $help, '/--simple-html/', '--help contains --simple-html');
like( $help, '/--full-html/', '--help contains --full-html');
like( $help, '/--snippet-html/', '--help contains --snippet-html');
like( $help, '/--ansi-text/', '--help contains --ansi-text');

#no arguments should default to --ansi-text=-
my $default = run_script q{};
ok( defined $default, 'no arguments returned something');
like( $default, '/\033\[.+?m/i', 
    'no arguments should contain ansi color escape sequences');
like( $default, '/foo/i', 'no arguments should contain the word foo');

#tests for --snippet-html
my $snippet = run_script '--snippet-html=-';
ok( defined $snippet, '--snippet-html=- returned something');
like( $snippet, '/<pre>/i', '--snippet-html=- contains <pre> tag'); 
unlike( $snippet, '/<html>/i', '--snippet-html=- should not contain any <html>');
like( $snippet, '/foo/i', '--snippet-html=- should contain the word foo');

#tests for --simple-html
my $simple = run_script '--simple-html=-';
ok( defined $simple, '--simple-html=- returned something');
like( $simple, '/<pre>/i', '--simple-html=- should have <pre> tags'); 
like( $simple, '/<html>/i', '--simple-html=- should contain <html>');
unlike( $simple, '/text\/javascript/i', '--simple-html=- should not contain any JS');
like( $simple, '/foo/i', '--simple-html=- should contain the word foo');

#tests for ----full-html=-
my $full = run_script '--full-html=-';
ok( defined $full, '--full-html=- returned something');
like( $full, '/<pre>/i', '--full-html=- should have <pre> tags'); 
like( $full, '/<html>/i', '--full-html=- should contain <html>');
like( $full, '/text\/javascript/i', '--full-html=- should contain JS');
like( $full, '/foo/i', '--full-html=- should contain the word foo');

#tests for --ansi-text
my $ansi = run_script '--ansi-text=-';
ok( defined $ansi, '--ansi-text=- returned something');
like( $ansi, '/\033\[.+?m/i', 
    '--ansi-text=- should contain ansi color escape sequences');
like( $ansi, '/foo/i', '--ansi-text=- should contain the word foo');

#tests for correct html escaping behavior
like( $snippet, '/&lt;&gt;/', 
    '--snippet-html=- & html escaping works');
like( $snippet, '/&amp;/', 
    '--snippet-html=- & html escaping works');
like( $simple, '/&quot;/', 
    '--simple-html=- html escaping works');
like( $full, '/&lt;&gt;/',
    '--full-html=- html escaping works');
=cut
