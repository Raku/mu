#!/usr/bin/pugs

use v6;
use Test;
use File::Spec;

plan 3;

use_ok('Pod::Event::Parser');
use_ok('Pod::Event::Handler::POD');
chdir "ext/Pod-Event-Parser"; # Hack if we're run from make smoke

my $file_path = catfile('lib', 'Pod', 'Event', 'Parser.pm');
my $test_output = "";

# parse the file
parse($file_path, pod2pod($test_output));

# now slurp the file so we can compare it to something
my $expected_output;
my $start = 0;
my $fh = open($file_path);
for =$fh -> $line {
    $start = 1 if $line ~~ rx:perl5/^=pod/;
    if $start { $expected_output ~= $line }    
    $start = 0 if $line ~~ rx:perl5/^=cut/;    
}
$fh.close();
chomp($expected_output);

# now compare
is($test_output, $expected_output, '... Pod::Event::Parser POD round tripped successfully');

