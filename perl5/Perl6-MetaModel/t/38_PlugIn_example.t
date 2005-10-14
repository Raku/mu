#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 8;

use Perl6::MetaModel;

=pod

from http://www.nntp.perl.org/group/perl.perl6.language/23474

class Host {
    my $.plugInClass;
}

role PlugIn {
    method initWithHost (Host $h:) { ... }
}

role FeatureA {}
role FeatureB {}
role FeatureC {}

class AB {
    does PlugIn;
    does FeatureA;
    does FeatureB;
}

class ABC {
    does AB;
    does FeatureC;
}

if ($host.plugInClass.does('FeatureB')) {
    # ... do something with FeatureB
} 

=cut

my $Host = class 'Host' => {
    is => [ $::Object ],
    attributes => [ '$.plugInClass' ],
    methods => {
        'plugInClass' => sub {
            my $self = shift;
            _('$.plugInClass' => shift) if @_;
            _('$.plugInClass');
        }
    }
};

my $PlugIn = role 'PlugIn' => { 
    methods => {
        'initWithHost' => sub { 'initing with Host' }
    }
};

my $FeatureA = role 'FeatureA' => {};
my $FeatureB = role 'FeatureB' => {};
my $FeatureC = role 'FeatureC' => {};

my $AB = class 'AB' => {
    does => [ $PlugIn, $FeatureA, $FeatureB ]
};

my $ABC = class 'ABC' => {
    does => [ $AB, $FeatureC ]
};

# now test it all 
my $host = $Host->new();
$host->plugInClass($AB);

ok($host->plugInClass->does('PlugIn'), '... AB does PlugIn');
ok($host->plugInClass->does('FeatureA'), '... AB does FeatureA');
ok($host->plugInClass->does('FeatureB'), '... AB does FeatureB');

my $host2 = $Host->new();
$host2->plugInClass($ABC);

ok($host2->plugInClass->does('PlugIn'), '... ABC does PlugIn');
ok($host2->plugInClass->does('FeatureA'), '... ABC does FeatureA');
ok($host2->plugInClass->does('FeatureB'), '... ABC does FeatureB');
ok($host2->plugInClass->does('FeatureC'), '... ABC does FeatureC');

# we can even check that one plugin is capable of "doing" another plugin

ok($host2->plugInClass->does('AB'), '... ABC does AB');

