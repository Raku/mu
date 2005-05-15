#!/usr/bin/pugs

use v6;
use Test;

plan 4;

use_ok('Test::Builder::Output');

my $output = open(">output");
my $error_output = open(">error_output");

my $output = ::Test::Builder::Output.new(
    output       => $output,
    error_output => $error_output,    
    );
is($output.ref, 'Test::Builder::Output', '... this is a Test::Builder::Output instance');

# NOTE:
# the # needs to be removed, but right now
# it messed up Test::Harness too much
$output.write('#ok 1');
$output.write('#ok 2');
$output.write("#ok 3\ntesting");

my $output_output = slurp('output');
is($output_output, 
"ok 1
ok 2
ok 3
#testing
", '... got the right output', :todo<feature>);

$output.diag('this is error output');
$output.diag("this is error output\nover two lines");

my $error_output_output = slurp('error_output');
is($error_output_output, 
"#this is error output
#this is error output
#over two lines
", '... got the right error output', :todo<feature>);

END {
    unlink("output");
    unlink("error_output");    
}