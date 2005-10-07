#!/usr/bin/perl

use strict;
use warnings;

use Perl6::MetaModel;
use Carp 'confess';

require "lib/TestBuilder.pm";

$::TestBuilder->STORE('::TestPlan' => class 'TestBuilder::TestPlan' => {
    is => [ $::Object ],
    attributes => [ '$:expect' ],
    submethods => {
        'BUILD' => sub {
            my ($self, %params) = @_;
            if (!exists $params{'$:expect'}) {
                _('$:expect' => 0);
            }
            elsif (!defined $params{'$:expect'}) {
                confess "Invalid or missing plan";            
            }
        }
    },
    methods => {
        'header' => sub { '1..' . _('$:expect') },
        'footer' => sub {
            my ($self, $run) = @_;
            return '' if $run == _('$:expect');
            return "Expected " . _('$:expect') . " but ran $run";            
        }
    }
});

$::TestBuilder->STORE('::NullPlan' => class 'TestBuilder::NullPlan' => {
    is => [ $::Object ],    
    methods => {
        'header' => sub { '' },
        'footer' => sub {
            my ($self, $run) = @_;
            return "1..$run";            
        }
    }    
});

1;

__END__

=pod

=head1 NAME

TestBuilder::TestPlan

=head1 SYNOPSIS

  use TestBuilder::TestPlan;

=head1 DESCRIPTION

B<NOTE:>
This is a Perl6-MetaModel 2.0 port of the Perl 6 version of 
Test::Builder::TestPlan.

This file contains both the TestBuilder::TestPlan object and 
a subclass, TestBuilder::NullPlan. 

TestBuilder::NullPlan is roughly equivalent to C<plan('no_plan')>
in the Perl 5 Test::Builder.

=head1 PUBLIC ATTRIBUTES

=over 4

=item B<Int $.expect>

The number of tests expected to run.

=back

=head1 METHODS

=over 4

=item B<header returns Str>

Returns a string containing the TAP header for this plan.

=item B<footer returns Str (Int $run)>

Returns a string containing the TAP footer for this plan.

=back

=head1 SEE ALSO

Perl 5 Test::Builder and Test::Harness::TAP.
Perl 6 Test::Builder;

=head1 AUTHORS

Perl6::MetaModel 2.0 code by Stevan Little E<lt>stevan@iinteractive.comE<gt>

Perl 6 code by chromatic E<lt>chromatic@wgz.orgE<gt>

documentation by Stevan Little E<lt>stevan@iinteractive.comE<gt> and chromatic.

=head1 Perl 6 Code

  class Test::Builder::TestPlan-0.2.1
  {
      has Int $:expect;
  
      submethod BUILD ( ?$:expect = 0 )
      {
          fail "Invalid or missing plan" unless defined $:expect;
      }
  
      method header returns Str
      {
          return "1..$:expect";
      }
  
      method footer returns Str (Int $run)
      {
          return '' if $run == $:expect;
          return "Expected $.expect but ran $run";
      }
  }
  
  # XXX: extract the useful similarities into a role
  # cannot inherit from TestPlan because of the lack of $:expect
  class Test::Builder::NullPlan
  {
      method header returns Str
      {
          return '';
      }
  
      method footer returns Str (Int $run)
      {
          return "1..$run";
      }
  }


=cut
