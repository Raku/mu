#!/usr/bin/perl

use strict;
use warnings;

use Perl6::MetaModel;

require "lib/TestBuilder.pm";

$::TestBuilder->STORE('::Test' => class 'TestBuilder::Test' => {
    is => [ $::Object ],
    class_methods => { 
        'new' => sub {
            my ($self, %params) = @_;
            $params{'$.passed'}      = 1  unless exists $params{'$.passed'};
            $params{'$.skip'}        = 0  unless exists $params{'$.skip'};
            $params{'$.todo'}        = 0  unless exists $params{'$.todo'};
            $params{'$.reason'}      = '' unless exists $params{'$.reason'};
            $params{'$.description'} = '' unless exists $params{'$.description'};

            return $::TestBuilder->FETCH('::Test')->FETCH('::TODO')->new(
                '$.description' => $params{'$.description'},
                '$.passed'      =>      $params{'$.passed'},
                '$.reason'      =>      $params{'$.reason'},
                '$.number'      =>      $params{'$.number'},
            ) if $params{'$.todo'};

            return $::TestBuilder->FETCH('::Test')->FETCH('::Skip')->new(
                '$.description' => $params{'$.description'},
                '$.passed'      =>                        1,
                '$.reason'      =>      $params{'$.reason'},
                '$.number'      =>      $params{'$.number'},
            ) if $params{'$.skip'};

            return $::TestBuilder->FETCH('::Test')->FETCH('::Pass')->new(
                '$.description' => $params{'$.description'},
                '$.passed'      =>                        1,
                '$.number'      =>      $params{'$.number'},
            ) if $params{'$.passed'};

            return $::TestBuilder->FETCH('::Test')->FETCH('::Fail')->new(
                '$.description' => $params{'$.description'},
                '$.passed'      =>                        0,
                '$.number'      =>      $params{'$.number'},
            );
        }
    }
});

$::TestBuilder->FETCH('::Test')->STORE('::Base' => role 'TestBuilder::Test::Base' => {
    attributes => [
        '$.passed',
        '$.number',
        '$.diagnostic',
        '$.description',
    ],
    submethods => { 
        'BUILD' => sub {
            my ($self, %params) = @_;
            _('$.number'     => 0)     unless exists $params{'$.number'};
            _('$.diagnostic' => '???') unless exists $params{'$.diagnostic'};
        }
    },
    methods => {
        'number'      => sub { _('$.number') },
        'passed'      => sub { _('$.passed') },        
        'diagnostic'  => sub { _('$.diagnostic') },        
        'description' => sub { _('$.description') },        
        'status' => sub {
            return {
                passed      => _('$.passed'),
                description => _('$.description'),
            };
        },
        'report' => sub {
            my $ok  = _('$.passed') ? 'ok ' : 'not ok ';
            $ok    .= _('$.number');
            $ok    .= " - " . _('$.description') if _('$.description');
            return $ok;
        }
    }
});

$::TestBuilder->FETCH('::Test')->STORE('::Pass' => class 'TestBuilder::Test::Pass' => {
    is   => [ $::Object ],
    does => [ $::TestBuilder->FETCH('::Test')->FETCH('::Base') ],
});

$::TestBuilder->FETCH('::Test')->STORE('::Fail' => class 'TestBuilder::Test::Fail' => {
    is   => [ $::Object ],
    does => [ $::TestBuilder->FETCH('::Test')->FETCH('::Base') ],
});

$::TestBuilder->FETCH('::Test')->STORE('::WithReason' => role 'TestBuilder::Test::WithReason' => {
    does => [ $::TestBuilder->FETCH('::Test')->FETCH('::Base') ],
    attributes => [ '$.reason' ],
    methods => {
        'reason' => sub { _('$.reason') },
        'status' => sub {
            my $status_sub      = $::TestBuilder->FETCH('::Test')->FETCH('::Base')->FETCH('&status');
            my $status          = $::SELF->$status_sub();
            $status->{"reason"} = _('$.reason');
            return $status;
        }
    }
});

$::TestBuilder->FETCH('::Test')->STORE('::Skip' => class 'TestBuilder::Test::Skip' => {
    is   => [ $::Object ],
    does => [ $::TestBuilder->FETCH('::Test')->FETCH('::WithReason') ],
    methods => {
        'report' => sub { "not ok " . _('$.number') . " #skip " . _('$.reason') },
        'status' => sub {
            my $status_sub    = $::TestBuilder->FETCH('::Test')->FETCH('::WithReason')->FETCH('&status');            
            my $status        = $::SELF->$status_sub();
            $status->{"skip"} = 1;
            return $status;            
        }
    }
});

$::TestBuilder->FETCH('::Test')->STORE('::TODO' => class 'TestBuilder::Test::TODO' => {
    is   => [ $::Object ],
    does => [ $::TestBuilder->FETCH('::Test')->FETCH('::WithReason') ],
    methods => {
        'report' => sub {
            my $ok          = _('$.passed') ? 'ok' : 'not ok';
            my $description = "# TODO " . _('$.description');
            return join(' ', $ok, _('$.number'), $description);            
        },
        'status' => sub {
            my $status_sub  = $::TestBuilder->FETCH('::Test')->FETCH('::WithReason')->FETCH('&status');            
            my $status                 = $::SELF->$status_sub();
            $status->{"TODO"}          = 1;
            $status->{"passed"}        = 1;
            $status->{"really_passed"} = _('$.passed');
            return $status;          
        }
    }
});

1;

__END__

=pod

=head1 NAME

Test::Builder::Test

=head1 SYNOPSIS

  use Test::Builder::Test;

=head1 DESCRIPTION

The Test::Builder::Test class represents a single test within the
Test::Builder framework. The class itself is actually a factory for
classes which C<does> the Test::Builder::Test::Base role.

=head1 METHODS

=over 4

=item B<new>

This is a Factory method used to create a specific class instances which
perform the Test::Builder::Test::Base role.

=over 4

=item C<$number>   

This is the test number. 

=item C<$passed = 1>

This parameter defaults to true, and so the factory returns a 
Test::Builder::Test::Pass instance, passing on the C<$description> 
parameter, and setting the passed parameter to a true value. 

If this parameter is false, the factory returns a Test::Builder::Test::Fail
instance, passing on the C<$description> parameter and setting the passed
parameter to a false value. 

=item C<?$skip = 0>

This parameter defaults to false. If it is true, the factory returns a
Test::Builder::Test::Skip instance, passing on the C<$description> and
C<$reason> parameters and a true value for the passed parameter.

=item C<?$todo = 0>

This parameter defaults to false.  If it is true, the factory returns
a Test::Builder::Test::TODO instance, passing on the C<$description>, 
C<$reason>, and C<$passed> parameters. 

=item C<?$reason = ''> 

This parameter is for the skip and TODO tests and should contain
a string describing why the test is a skip or TODO test.

=item C<?$description = ''>

This parameter and contains the test description.

=back

=back

=head1 ROLES

=over 4

=item B<Test::Builder::Test::Base>

This role contains several public attributes.

=over 4

=item B<Bool $.passed>

Whether the test passed.

=item B<Int $.number>

The number of the test in the current test file.

=item B<Str $.description>

The test's description (optional).

=item B<Str $.diagnostic>

The test's diagnostic message (optional).

=back 

This role contains two methods:

=over 4

=item B<status returns Hash>

This returns a hash containing two keys, C<passed> and C<description>, from the
test's attributes.

=item B<report returns Str>

This returns a string which follows the TAP protocol:

  ok 1 - test description
  not ok 2 - test description
  # test diagnoistic

=back

=item B<Test::Builder::Test::WithReason>

This role extends the Test::Builder::Test::Base role to add a simple public
attribute.

=over 4

=item B<Str $.reason>

=back

It also overrides a single method:

=over 4

=item B<status returns Hash>

This method calls the superclass C<status> method and adds the values of the
test's C<skip> and C<reason> attributes to the hash before returning it.

=back

=back

=head1 OTHER CLASSES

=over 4

These first two classes perform the Test::Builder::Test::Base role.

=item B<Test::Builder::Test::Pass>

=item B<Test::Builder::Test::Fail>

These next two classes both perform the Test::Builder::Test::WithReason role
and override some methods to provide additional features.

=item B<Test::Builder::Test::TODO>

This overrides two methods.

=over 4

=item B<report returns Str>

This handles the specific details of TAP output for TODO tests.

=item B<status returns Hash>

This adds a C<TODO> key, sets the C<passed> key to true, and adds a
C<really_passed> key which contains the boolean representing if the test truly
did pass.

=back

=item B<Test::Builder::Test::Skip>

This also overrides two methods.

=over 4

=item B<report returns Str>

This handles the specific details of TAP output for Skip tests.

=item B<status returns Hash>

This sets the C<skip> key to true in the hash.

=back

=back

=head1 SEE ALSO

Perl 5 Test::Builder and Test::Harness::TAP.

=head1 AUTHORS

Perl6::MetaModel 2.0 code by Stevan Little E<lt>stevan@iinteractive.comE<gt>

Perl 6 code by chromatic E<lt>chromatic@wgz.orgE<gt>

documentation by Stevan Little E<lt>stevan@iinteractive.comE<gt> and chromatic.

=head1 Perl 6 Code
  
  class Test::Builder::Test-0.2.1
  {
      method new (
          $number,     
          ?$passed      = 1,
          ?$skip        = 0,
          ?$todo        = 0,
          ?$reason      = '', 
          ?$description = '',
      )
      {
          return ::Test::Builder::Test::TODO.new(
              description => $description,
              passed      =>      $passed,
              reason      =>      $reason,
              number      =>      $number,
          ) if $todo;
  
          return ::Test::Builder::Test::Skip.new(
              description => $description,
              passed      =>            1,
              reason      =>      $reason,
              number      =>      $number,
          ) if $skip;
  
          return ::Test::Builder::Test::Pass.new(
              description => $description,
              passed      =>            1,
              number      =>      $number,
          ) if $passed;
  
          return ::Test::Builder::Test::Fail.new(
              description => $description,
              passed      =>            0,
              number      =>      $number,
          );
      }
  }
  
  role Test::Builder::Test::Base
  {
      has Bool $.passed;
      has Int  $.number;
      has Str  $.diagnostic;
      has Str  $.description;
  
      submethod BUILD (
          $.description,
          $.passed,
          ?$.number     =     0,
          ?$.diagnostic = '???',
      ) {}
  
      method status returns Hash
      {
          return
          {
              passed      => $.passed,
              description => $.description,
          };
      }
  
      method report returns Str
      {
          my $ok  = $.passed ?? 'ok ' !! 'not ok ';
          $ok    ~= $.number;
          $ok    ~= " - $.description" if $.description;
  
          return $ok;
      }
  }
  
  class Test::Builder::Test::Pass does Test::Builder::Test::Base {}
  class Test::Builder::Test::Fail does Test::Builder::Test::Base {}
  
  role Test::Builder::Test::WithReason does Test::Builder::Test::Base
  {
      has Str $.reason;
  
      submethod BUILD ( $.reason ) {}
  
      method status returns Hash ( $self: )
      {
          # XXX - This wants to be $self.SUPER::status();
          my $status        = $self.Test::Builder::Test::Base::status();
          $status{"reason"} = $.reason;
          return $status;
      }
  }
  
  class Test::Builder::Test::Skip does Test::Builder::Test::WithReason
  {
      method report returns Str
      {
          return "not ok $.number #skip $.reason";
      }
  
      method status returns Hash ( $self: ) 
      {
          # XXX - This wants to be $self.SUPER::status();
          my $status      = $self.Test::Builder::Test::WithReason::status();
          $status{"skip"} = 1;
          return $status;
      }
  
  }
  
  class Test::Builder::Test::TODO does Test::Builder::Test::WithReason
  {
      method report returns Str
      {
          my $ok          = $.passed ?? 'ok' !! 'not ok';
          my $description = "# TODO $.description";
          return join( ' ', $ok, $.number, $description );
      }
  
      method status returns Hash ( $self: ) 
      {
          # XXX - This wants to be $self.SUPER::status();
          my $status               = $self.Test::Builder::Test::WithReason::status();
          $status{"TODO"}          = 1;
          $status{"passed"}        = 1;
          $status{"really_passed"} = $.passed;
          return $status;
      }
  }

=cut
