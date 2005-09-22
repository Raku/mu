#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 3;

require "lib/TestBuilder.pm";

my ($output, $error_output);

open $output, ">", "output";
open $error_output, ">", "error_output";

my $tbo = $::TestBuilder->FETCH('::Output')->new(
    '$.output'       => $output,
    '$.error_output' => $error_output,    
);

isa_ok($tbo, 'TestBuilder::Output');

$tbo->write('ok 1');
$tbo->write('ok 2');
$tbo->write("ok 3\ntesting");

close($output);

open OUTPUT, '<', 'output';
my $output_output = join "" => <OUTPUT>;
is($output_output, 
    "ok 1\nok 2\nok 3\n# testing\n", 
    'write() should write to normal output, escaping newlines' );

$tbo->diag('this is error output');
$tbo->diag("this is error output\nover two lines");

close($error_output);

open ERROR_OUTPUT, '<', 'error_output';
my $error_output_output = join "" => <ERROR_OUTPUT>;
is($error_output_output, 
qq|# this is error output
# this is error output
# over two lines\n|,
    'diag() should write to error output, escaping all output' );

END {
    unlink( 'output' );
    unlink( 'error_output' );
}

{
    # avoid those annoying "used only once" warnings
    my @blah = ($::TestBuilder);
}
