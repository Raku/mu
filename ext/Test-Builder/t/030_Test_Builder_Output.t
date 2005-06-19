#!/usr/bin/pugs

use v6;
use Test;

plan 3;

use Test::Builder::Output;

my $output       = open('output',       :w);
my $error_output = open('error_output', :w);

my $tbo = Test::Builder::Output.new(
    output       => $output,
    error_output => $error_output,    
);

is( $tbo.ref, 'Test::Builder::Output',
    'new() should return a Test::Builder::Output instance' );

$tbo.write('ok 1');
$tbo.write('ok 2');
$tbo.write("ok 3\ntesting");

$output.close();

my $output_output = slurp('output');
is( $output_output, "ok 1\nok 2\nok 3\n#testing\n",
	'write() should write to normal output, escaping newlines' );

$tbo.diag('this is error output');
$tbo.diag("this is error output\nover two lines");

$error_output.close();

my $error_output_output = slurp('error_output');
is($error_output_output, 
"#this is error output
#this is error output
#over two lines\n",
	'diag() should write to error output, escaping all output' );

END
{
    unlink( 'output' );
    unlink( 'error_output' );
}
