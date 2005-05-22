#!/usr/bin/pugs

use v6;
use Test;

plan 28;

use Perl::Meta::MetaClass;
use Perl::Meta::Method;

=pod

This class tests property assignment and removal

=cut

my $mmc = Perl::Meta::MetaClass.new('Role');

{
    my @labels = $mmc.methodLabels();
    is(+@labels, 0, '... we have no method labels yet'); 
}

# what's a nice real-world example of a method that can be called on
# the Model? Well, .new(), .isa(), .meta() of course!

my $new = Perl::Meta::Method.new();
my $isa = Perl::Meta::Method.new(code => 
    sub ($self, $other) {
        $self.allSuperclasses().grep:{ $other =:= $self }
    }
);
my $meta = Perl::Meta::Method.new(code => sub { Perl::Meta::Class });

$mmc.addMethod('new', $new);
$mmc.addMethod('isa', $isa);

ok($mmc.findMethod('new') =:= $new, '... found the right method');
ok($mmc.findMethod('isa') =:= $isa, '... found the right method');

ok(!$mmc.isMethodSupported('meta'), '... did not find the method (as expected)');

$mmc.addMethod('meta', $meta);

{
    my @labels = sort $mmc.methodLabels();
    is(+@labels, 3, '... we have 3 method labels'); 
    is(@labels[0], 'isa', '... the first is isa'); 
    is(@labels[1], 'meta', '... the second is meta');     
    is(@labels[2], 'new', '... the second is new');     
}

{
    my %methods = sort $mmc.methods();
    is(+%methods, 3, '... we have 2 methods'); 
    ok(%methods{'isa'} =:= $isa, '... the first is $isa'); 
    ok(%methods{'meta'} =:= $meta, '... the second is $meta');     
    ok(%methods{'new'} =:= $new, '... the second is $new');     
}

my $removed_method = $mmc.removeMethod('meta');
ok($removed_method =:= $meta, '... removed $meta');

{
    my %methods = sort $mmc.methods();
    is(+%methods, 2, '... we have 2 methods left'); 
    ok(%methods{'isa'} =:= $isa, '... the first is $isa');     
    ok(%methods{'new'} =:= $new, '... the first is $new');     
}

my $sub_mmc = Perl::Meta::MetaClass.new('Class');
$sub_mmc.superclass($mmc);

# for some reason, we're deciding that only Class objects have
# meta-objects.
$sub_mmc.addMethod('meta', $meta);

ok($sub_mmc.isMethodSupported('new'), '... did find the method in parent class');
ok($sub_mmc.findMethod('new') =:= $new, '... found the right method (in parent class)');

ok(!$mmc.isMethodSupported('meta'), '... did not find the method (as expected) in parent class');
ok($sub_mmc.isMethodSupported('meta'), '... did find the method (as expected) in class');

my $sub_sub_mmc = Perl::Meta::MetaClass.new('ThreadSafeClass');
$sub_sub_mmc.superclass($sub_mmc);

ok($sub_sub_mmc.isMethodSupported('new'), '... did find the method in parents parent class');
ok($sub_sub_mmc.findMethod('new') =:= $new, '... found the right method (in parents parent class)');
ok($sub_sub_mmc.findMethod('meta'), '... did find the method (as expected) in parents class');

# check some errors

my $FakeInstance;

$!= undef; 
dies_ok {
    $mmc.invokeMethod('meta', $FakeInstance);
}, '... this dies as expected';
like($!, rx:perl5/^Method not found/, '... got the right error');

$!= undef; 
dies_ok {
    $sub_sub_mmc.invokeMethod('new', $FakeInstance);
}, '... this dies as expected';
like($!, rx:perl5/^No method impl defined/, '... got the right error');

is($sub_mmc.invokeMethod('meta', $FakeInstance), Perl::Meta::Class, '... the method returned what we expected');
