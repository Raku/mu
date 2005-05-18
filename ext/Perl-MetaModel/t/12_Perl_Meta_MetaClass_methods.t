#!/usr/bin/pugs

use v6;
use Test;

plan 25;

use Perl::Meta::Class;
use Perl::Meta::Method;

=pod

This class tests property assignment and removal

=cut

my $mmc = Perl::Meta::Class::new('Class');

{
    my @labels = $mmc.methodLabels();
    is(+@labels, 0, '... we have no method labels yet'); 
}

my $method1 = Perl::Meta::Method.new();
my $method2 = Perl::Meta::Method.new(code => sub { 'Hello World' });

$mmc.addMethod('method1', $method1);
$mmc.addMethod('method2', $method2);

ok($mmc.findMethod('method1') =:= $method1, '... found the right method');
ok($mmc.findMethod('method2') =:= $method2, '... found the right method');

ok(!$mmc.isMethodSupported('method3'), '... did not find the method (as expected)');

{
    my @labels = sort $mmc.methodLabels();
    is(+@labels, 2, '... we have 2 method labels'); 
    is(@labels[0], 'method1', '... the first is method1'); 
    is(@labels[1], 'method2', '... the second is method2');     
}

{
    my %methods = sort $mmc.methods();
    is(+%methods, 2, '... we have 2 methods'); 
    ok(%methods{'method1'} =:= $method1, '... the first is $method1'); 
    ok(%methods{'method2'} =:= $method2, '... the second is $method2');     
}

my $removed_method = $mmc.removeMethod('method2');
ok($removed_method =:= $method2, '... removed $method2');

{
    my %methods = sort $mmc.methods();
    is(+%methods, 1, '... we have 1 method'); 
    ok(%methods{'method1'} =:= $method1, '... the first is $method1');     
}

my $sub_mmc = Perl::Meta::Class::new('SubClass');
$sub_mmc.superclass($mmc);

$sub_mmc.addMethod('method2', $method2);

ok($sub_mmc.isMethodSupported('method1'), '... did find the method in parent class');
ok($sub_mmc.findMethod('method1') =:= $method1, '... found the right method (in parent class)');

ok(!$mmc.isMethodSupported('method2'), '... did not find the method (as expected) in parent class');
ok($sub_mmc.isMethodSupported('method2'), '... did find the method (as expected) in class');

my $sub_sub_mmc = Perl::Meta::Class::new('SubSubClass');
$sub_sub_mmc.superclass($sub_mmc);

ok($sub_sub_mmc.isMethodSupported('method1'), '... did find the method in parents parent class');
ok($sub_sub_mmc.findMethod('method1') =:= $method1, '... found the right method (in parents parent class)');
ok($sub_sub_mmc.findMethod('method2'), '... did find the method (as expected) in parents class');

# check some errors

$!= undef; 
dies_ok {
    $mmc.invokeMethod('method2');
}, '... this dies as expected';
like($!, rx:perl5/^Method not found/, '... got the right error');

$!= undef; 
dies_ok {
    $sub_sub_mmc.invokeMethod('method1');
}, '... this dies as expected';
like($!, rx:perl5/^Method has no code/, '... got the right error');

is($sub_mmc.invokeMethod('method2'), 'Hello World', '... the method returned what we expected');
