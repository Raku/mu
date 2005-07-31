#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 21;
use Test::Exception;

use Perl6::MetaModel;
use Perl6::Object;

=pod

From L<A12/Single Dispatch>

=cut

class Foo => {
    is => [ 'Perl6::Object' ],    
    instance => {
        methods => {
            foo => sub { 'Foo::foo' },
            bar => sub { 'Foo::bar' }            
        }
    }
};

class Bar => {
    is => [ 'Foo' ],
    instance => {
        methods => {
            foo => sub { 'Bar::foo' },
            bar => sub { 'Bar::bar' }
        }
    }
};

class Baz => {
    is => [ 'Bar' ],
    instance => {
        methods => {
            foo => sub { 'Baz::foo' }
        }
    }
};

my $baz = Baz->new();
isa_ok($baz, 'Baz');
isa_ok($baz, 'Bar');
isa_ok($baz, 'Foo');

=pod

this is like call this in Perl 6:

  $baz.?baz()

=cut
{
    my $rval;
    lives_ok {
        $rval = CALLONE($baz, 'baz', 1);
    } '... calling a unknown method with the $maybe bit set didnt die';
    ok(!defined($rval), '... we got an undef return value');
}

{
    my $rval;
    lives_ok {
        $rval = CALLONE($baz, 'foo', 1);
    } '... calling a known method with the $maybe bit set didnt die';
    is($rval, 'Baz::foo', '... we got the right return value');
}

{ # works well with virtual methods
    my $rval;
    lives_ok {
        $rval = CALLONE($baz, 'bar', 1);
    } '... calling a known method with the $maybe bit set didnt die';
    is($rval, 'Bar::bar', '... we got the right return value');
}

=pod

this is like call this in Perl 6:

  $baz.baz()

=cut
{
    dies_ok {
        CALLONE($baz, 'baz');
    } '... calling a unknown method without the $maybe bit set did die';
}

=pod

this is like call this in Perl 6:

  $baz.+foo()
  $baz.+bar()  

=cut
{
    my @rval;
    lives_ok {
        @rval = CALLALL($baz, 'foo');
    } '... calling a known method on all didnt die';
    is_deeply(
        \@rval, 
        [[ 'Baz::foo' ], [ 'Bar::foo' ], [ 'Foo::foo' ]], 
        '... we got the right return value');
}

{
    my @rval;
    lives_ok {
        @rval = CALLALL($baz, 'bar');
    } '... calling a known method on all didnt die';
    is_deeply(
        \@rval, 
        [ [ 'Bar::bar' ], [ 'Foo::bar' ]], 
        '... we got the right return value');
}


=pod

this is like call this in Perl 6:

  $baz.*foo()
  $baz.*bar()  
  $baz.*baz()

=cut
{
    my @rval;
    lives_ok {
        @rval = CALLALL($baz, 'foo', 1);
    } '... calling a known method on all didnt die with the $maybe bit set';
    is_deeply(
        \@rval, 
        [[ 'Baz::foo' ], [ 'Bar::foo' ], [ 'Foo::foo' ]], 
        '... we got the right return value');
}

{
    my @rval;
    lives_ok {
        @rval = CALLALL($baz, 'bar', 1);
    } '... calling a known method on all didnt die with the $maybe bit set';
    is_deeply(
        \@rval, 
        [ [ 'Bar::bar' ], [ 'Foo::bar' ]], 
        '... we got the right return value');
}

{
    my $rval;
    lives_ok {
        $rval = CALLALL($baz, 'baz', 1);
    } '... calling an unknown method on all didnt die with the $maybe bit set';
    ok(!defined($rval), '... we got an undef return value');
}

{
    dies_ok {
        CALLALL($baz, 'baz');
    } '... calling a unknown method on all without the $maybe bit set did die';
}
