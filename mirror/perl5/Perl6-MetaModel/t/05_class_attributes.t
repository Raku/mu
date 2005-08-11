#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 25;
use Test::Exception;

use Perl6::MetaModel;
use Perl6::Object;

=pod

This test file checks the details of the Class attribute accessor
generation, in particular it checks the following:

=over 4

=item private attributes do not get accessors

=item private attributes can still be reached inside the local class

=item public attributes do get accessors

=item public attributes do get mutators

=item public attributes mutators will change the attribute value

=back

=cut

class Basic => {
    is => [ 'Perl6::Object' ],
    instance => {
        attrs => [ 
            [ '$.scalar' => { access => 'rw' } ], 
            [ '@.array'  => { access => 'rw' } ], 
            [ '%.hash'   => { access => 'rw' } ] 
        ]
    }
};

my $basic = Basic->new();
isa_ok($basic, 'Basic');

ok(!defined($basic->scalar()), '... scalar initializes to undef');
is_deeply($basic->array(), [], '... array initializes to an empty array ref');
is_deeply($basic->hash(), {}, '... hash initializes to an empty hash ref');

lives_ok { 
    $basic->scalar('Foo') 
} '... scalar() was assigned to correctly';
is($basic->scalar(), 'Foo', '... and the value of scalar() is correct');

dies_ok { 
    $basic->array('Foo') 
} '... assigning a non ARRAY ref to array() is an error';

dies_ok { 
    $basic->array({ Fail => 1 }) 
} '... assigning a non ARRAY ref to array() is an error';

lives_ok { 
    $basic->array([ 1, 2, 3 ]) 
} '... array() was assigned to correctly';
is_deeply($basic->array(), [ 1, 2, 3 ], '... array() was assigned to correctly');

dies_ok { 
    $basic->hash('Foo') 
} '... assigning a non HASH ref to hash() is an error';

dies_ok { 
    $basic->hash([]) 
} '... assigning a non HASH ref to hash() is an error';

lives_ok { 
    $basic->hash({ one => 1, two => 2 }) 
} '... hash() was assigned to correctly';
is_deeply($basic->hash(), { one => 1, two => 2 }, '... hash() was assigned to correctly');

class Base => {
    is => [ 'Perl6::Object' ],    
    instance => {
        attrs => [ '$:foo' ],
        BUILD => sub { _('$:foo' => 'Base::Foo') },
        methods => {
            get_base_foo => sub { _('$:foo') },
            set_base_foo => sub { _('$:foo' => 'Base::Foo -> new') }            
        }
    }
};

class Derived1 => {
    is => [ 'Base' ],
    instance => {
        attrs => [ [ '$.foo' => { access => 'rw' } ], '$:bar' ],
        BUILD => sub { _('$.foo' => 'Foo::Foo') },
    }
};

my $d = Derived1->new();
isa_ok($d, 'Derived1');

ok(!$d->can('bar'), '... we cannot bar() because that is private');

is($d->foo(), 'Foo::Foo', '... the foo attribute was collected in the right order');
is($d->get_base_foo(), 'Base::Foo', '... the Base::foo attribute can still be accessed');

lives_ok { 
    $d->foo('New::Foo') 
} '... setting a public attribute did not fail';

is($d->foo(), 'New::Foo', '... the foo attribute can be changed with the accessor');

lives_ok { 
    $d->set_base_foo() 
} '... calling a method which sets a private attribute worked correctly';

is($d->get_base_foo(), 'Base::Foo -> new', '... the Base::foo attribute can still be accessed');

dies_ok {
    _('$.foo2' => 'nothing')
} '... setting a incorrect parameter failed correctly';

# check for accessor conflicts

class ConflictChecker => {
    is => [ 'Perl6::Object' ],    
    instance => {
        attrs => [ '$.foo' ],
        BUILD => sub { _('$.foo' => 'just $.foo') },
        methods => {
            foo => sub {
                'ConflictChecker->foo returns "' . _('$.foo') . '"'
            }
        }
    }    
};

my $cc = ConflictChecker->new();
isa_ok($cc, 'ConflictChecker');

is($cc->foo(), 'ConflictChecker->foo returns "just $.foo"', '... got the right value from the accessor');
