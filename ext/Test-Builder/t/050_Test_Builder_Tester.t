#!/usr/bin/pugs

use v6;

use Test::Builder::Tester;
plan 8;

my $Test = Test::Builder.new();

test_out( 'ok 1 - Hello' );
$Test.ok( 1, 'Hello' );
test_test( 'passing test' );

test_out( 'not ok 2 - Goodbye' );
$Test.ok( 0, 'Goodbye' );
test_test( 'failing test' );

test_out( 'ok 3 - A message' );
test_diag( "some\nlines" );
$Test.ok( 1, 'A message' );
$Test.diag( 'some' );
$Test.diag( 'lines' );
test_test( 'passing test with diagnostics' );

test_out( 'not ok 4 - another message' );
$Test.ok( 0, 'another message' );
$Test.diag( "many\nmany\nlines" );
test_diag( 'many' );
test_diag( 'many' );
test_diag( 'lines' );
test_test( 'failing test with diagnostics' );

test_pass();
$Test.ok( 1 );
test_test( 'passing test with no diagnostics' );

test_pass( 'my diagnostics' );
$Test.ok( 1, 'my diagnostics' );
test_test( 'passing test with diagnostics' );

test_fail();
$Test.ok( 0 );
test_test( 'failing test with no diagnostics' );

test_fail( 'more diagnostics' );
$Test.ok( 0, 'more diagnostics' );
test_test( 'failing test with diagnostics' );
