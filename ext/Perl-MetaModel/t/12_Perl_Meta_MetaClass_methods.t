#!/usr/bin/pugs

use v6;
use Test;

plan 10;

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
my $method2 = Perl::Meta::Method.new();

$mmc.addMethod('method1', $method1);
$mmc.addMethod('method2', $method2);

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