#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 52;
use Test::Exception;

require Perl6::MetaModel::Chaos;

## test the opaque instance containers

my $i;
$i = ::create_opaque_instance(\$i, (foo => 1));
ok(ref($i), 'Dispatchable');

is(::opaque_instance_id($i), 1, '... got the right ID');
is(::opaque_instance_class($i), $i, '... got the right class');
is(::opaque_instance_attr($i, 'foo'), 1, '... got the right attrs');
    
my $j = ::create_opaque_instance(\$i, (bar => 1));
ok(ref($j), 'Dispatchable');

is(::opaque_instance_id($j), 2, '... got the right ID');    
is(::opaque_instance_class($j), $i, '... got the right class');
is(::opaque_instance_attr($j, 'bar'), 1, '... got the right attrs');
    
## test out some of the global functions ...

ok(!defined($::SELF), '... cannot get $?SELF outside of a valid context');
ok(!defined($::CLASS), '... cannot get $?CLASS outside of a valid context');
    
## test the method constructors

{
    # basic methods can be created ...
    
    my $m = ::make_method(sub { return 'Foo' });
    ok(ref($m), 'Perl6::Method');

    # and even called ...
    is($m->(), 'Foo', '... got the right return value');

    # but they do not have $?SELF or $?CLASS bound 
    
    my $m2 = ::make_method(sub { return $::SELF });
    ok(ref($m2), 'Perl6::Method');

    is($m2->($i), undef, '... got the right return value');

    my $m3 = ::make_method(sub { return $::CLASS });
    ok(ref($m3), 'Perl6::Method');

    is($m3->($i), undef, '... got the right return value');    
}

{
    my $m = ::make_method(sub { return 'Foo' });
    ok(ref($m), 'Perl6::Method');

    ::bind_method_to_class($m, $i);

    is($m->($i), 'Foo', '... got the right return value');

    my $m2 = ::make_method(sub { return $::SELF });
    ok(ref($m2), 'Perl6::Method');
    
    ::bind_method_to_class($m2, $i);

    is($m2->('Bar'), 'Bar', '... got the right return value');
    
    my $m3 = ::make_method(sub { return $::CLASS });
    ok(ref($m3), 'Perl6::Method');

    ::bind_method_to_class($m3, $i);

    is($m3->($i), $i, '... got the right return value');    
}

{
    my $m = ::make_class_method(sub { return 'Foo::Bar' });
    ok(ref($m), 'Perl6::ClassMethod');

    ::bind_method_to_class($m, $i);

    is($m->($i), 'Foo::Bar', '... got the right return value');
    
    my $m2 = ::make_class_method(sub { return $::SELF });
    ok(ref($m2), 'Perl6::ClassMethod');
    
    ::bind_method_to_class($m2, $i);

    is($m2->('Bar::Baz'), 'Bar::Baz', '... got the right return value');    
    
    my $m3 = ::make_class_method(sub { return $::CLASS });
    ok(ref($m3), 'Perl6::Method');

    ::bind_method_to_class($m3, $i);

    is($m3->($i), $i, '... got the right return value');       
}

{
    my $m = ::make_submethod(sub { return 'Baz' });
    ok(ref($m), 'Perl6::Submethod');
    
    ::bind_method_to_class($m, $i);
    
    is($m->($i), 'Baz', '... got the right return value');
    
    my $m2 = ::make_submethod(sub { return 'Baz' });
    ok(ref($m2), 'Perl6::Submethod');
    
    ::bind_method_to_class($m2, $i);    
    
    is($m2->($j), 'Baz', '... got the right return value');    

    my $m3 = ::make_submethod(sub { return $::SELF });
    ok(ref($m3), 'Perl6::Submethod');
    
    ::bind_method_to_class($m3, $i);    
    
    is($m3->($j), $j, '... got the right return value');    
    
    my $m4 = ::make_submethod(sub { return $::CLASS });
    ok(ref($m4), 'Perl6::Submethod');

    ::bind_method_to_class($m4, $i);    
    
    is($m4->($j), $i, '... got the right return value');        
    
    {
        no strict 'refs';
        no warnings 'redefine';
        
        *{'::next_METHOD'} = sub () { "fake next_METHOD" };
    
        my $m = ::make_submethod(sub { return 'Baz' });
        ok(ref($m), 'Perl6::Submethod');
        
        ::bind_method_to_class($m, $j);
        
        is($m->($i), 'fake next_METHOD', '... got the right return value');    
    }
    
    {
        my $m = ::make_submethod(sub { return 'Baz' });
        ok(ref($m), 'Perl6::Submethod');
        ok($Perl6::Submethod::FORCE, '... $Perl6::Submethod::FORCE is defined');
    
        ::bind_method_to_class($m, $j);
        
        is($m->($Perl6::Submethod::FORCE, $i), 'Baz', '... got the right return value (with force call)');          
    }
}

{
    
    my $pm = ::make_private_method(sub { return 'Foo' });
    ok(ref($pm), 'Perl6::PrivateMethod');    

    my $m = ::make_method(sub { $pm->($i) });
    ok(ref($m), 'Perl6::Method');
    
    ::bind_method_to_class($pm, $i);    
    ::bind_method_to_class($m,  $i);    
    
    is($m->($i), 'Foo', '... called private method successfully');
    
    my $m2 = ::make_method(sub { $pm->($i) });
    ok(ref($m2), 'Perl6::Method');  
    
    ::bind_method_to_class($m2, $j);      

    dies_ok {
        $m2->($i);
    } '... cannot call a private method from a different class';
}

## check some attribute stuff

{
   my $scalar = ::make_attribute('$.scalar');
   isa_ok($scalar, 'Perl6::Attribute');
   
   is(::instantiate_attribute_container($scalar), undef, '... got the right attribute container');
   
   my $array = ::make_attribute('@.array');
   isa_ok($array, 'Perl6::Attribute');

   is_deeply(::instantiate_attribute_container($array), [], '... got the right attribute container');
   
   my $hash = ::make_attribute('%.hash');      
   isa_ok($hash, 'Perl6::Attribute');    

   is_deeply(::instantiate_attribute_container($hash), {}, '... got the right attribute container');    
}
