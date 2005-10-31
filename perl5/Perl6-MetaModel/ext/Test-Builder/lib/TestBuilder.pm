#!/usr/bin/perl

use strict;
use warnings;

use lib '../../lib', '../../';

use Perl6::MetaModel;
use Carp 'confess';
use Scalar::Util 'blessed';

$::TestBuilder = undef;

$::TestBuilder = class 'TestBuilder' => {
    is => [ $::Object ],
    class_attributes => [
        '$:singleton',    
    ],
    attributes => [
        '$.output',
        '$.testplan',
        '@:results',
    ],
    class_methods => {
        'new'    => sub { 
            my ($class, $plan, $output) = @_;
            __('$:singleton' => $::CLASS->new('$.testplan' => $plan, '$.output'   => $output)) 
                unless defined __('$:singleton');
            __('$:singleton');
        },
        'create' => sub { shift; $::CLASS->new(@_) }        
    },
    submethods => {
        'BUILD' => sub {
            _('$.output' => $::TestBuilder->FETCH('::Output')->new());
        },
        'DESTROY' => sub {
            my $footer = _('$.testplan')->footer( scalar @{_('@:results')} );
            _('$.output')->write($footer) if $footer;            
        }
    },
    methods => {
        'get_test_number' => sub { scalar(@{_('@:results')}) + 1 },
        'testplan' => sub { _('$.testplan') },
        'plan' => sub {
            my ($self, $explanation, $tests) = @_;
            confess "Plan already set!" if _('$.testplan');
            if ($tests) {
                _('$.testplan' => $::TestBuilder->FETCH('::TestPlan')->new( '$.expect' => $tests ));
            }
            elsif ($explanation eq 'no_plan') {
                _('$.testplan' => $::TestBuilder->FETCH('::NullPlan')->new());
            }
            else {
                confess "Unknown plan";
            }
            _('$.output')->write(_('$.testplan')->header());            
        },
        'ok' => sub { 
            my ($self, $passed, $description) = @_;
            $description ||= '';
            $self->report_test(
                $::TestBuilder->FETCH('::Test')->new(
                    '$.number'      => $self->get_test_number(),
                    '$.passed'      =>  $passed,
                    '$.description' =>  $description,
                )
            );
            return $passed;
        },
        'diag' => sub {
            my ($self, $diagnostic) = @_;
            $diagnostic ||= '';            
            _('$.output')->diag($diagnostic);
        },
        'todo' => sub { 
            my ($self, $passed, $description, $reason) = @_;
            $self->report_test(
                $::TestBuilder->FETCH('::Test')->new(
                    '$.todo'        => 1,
                    '$.number'      => $self->get_test_number(),
                    '$.reason'      =>  $reason,
                    '$.description' =>  $description,
                )
            );
            return $passed;
        },        
        'skip' => sub { 
            my ($self, $num, $reason) = @_;
            $num ||= 1;
            $reason ||= 'skipped';
            for (1 .. $num) {
                $self->report_test(
                    $::TestBuilder->FETCH('::Test')->new(
                        '$.skip'   => 1,
                        '$.number' => $self->get_test_number(),
                        '$.reason' =>  $reason,
                    )
                );
            }
        },        
        'skip_all' => sub {
            confess 'Cannot skill_all with a plan' if defined _('$.testplan');
            _('$.output')->write('1..0');
            exit 0;
        },
        'BAILOUT' => sub {
            my ($self, $reason) = @_;
            $reason ||= '';
            _('$.output')->write("Bail out!  $reason");
            exit 255;
        },
        'report_test' => sub {
            my ($self, $test) = @_;
            (blessed($test) && $test->isa('TestBuilder::Test'))
                || confess "test argument must be a TestBuilder::Test instance";
            confess 'No plan set!' unless _('$.testplan');
            push @{_('@:results')} => $test;
            _('$.output')->write( $test->report() );
        }
    }
};

require "lib/TestBuilder/TestPlan.pm";
require "lib/TestBuilder/Output.pm";
require "lib/TestBuilder/Test.pm";

1;

__END__

=pod

=head1 NAME

Test::Builder - Backend for building test libraries

=head1 SYNOPSIS

  module My::Test::Module;

  use Test::Builder;
  use Test::Builder::Output;

  my Test::Builder $Test .= new(
        output => Test::Builder::Output.new(
           error_output => open('my_error_log_file')
        )
    );

  sub plan (Str $explanation?, Int $tests?) is export {
     $Test.testplan($explanation, $tests);
  }

  sub ok ($passed, $description?, $todo?) is export {
     if $todo {
        $Test.todo($passed, $description, $todo) 
            || $Test.diag("FAILED : $description");
     }
     else {
        $Test.ok($passed, $description)
            || $Test.diag("FAILED : $description");
     }
  }

  sub is ($got, $expected, $description?, $todo?) is export {
     if $todo {
        $Test.todo($got eq $expected, $description, $todo)
            || $Test.diag("FAILED : $description");
     }
     else {
        $Test.ok($got eq $expected, $description)
            || $Test.diag("FAILED : $description");        
     }
  }

  # then using our test module the test file themselves ...

  use My::Test::Module;

  plan('no_plan');
  # or
  plan :tests<20>;

  ok(2 == 2, '... 2 is equal to 2');
  is(2 + 2, 5, '... 2 plus 2 should be 5', :todo<bug>);

=head1 DESCRIPTION

This is a Perl 6 port of the Perl 5 module Test::Builder.

=head1 PUBLIC ATTRIBUTES

=over 4

=item B<Test::Builder::Output $.output>

=item B<Test::Builder::TestPlan $.testplan>

=back

=head1 METHODS

=over 4

=item B<new( *@args )>

This method actually returns a Test::Builder singleton, creating it if
necessary.  The optional named arguments are:

=over 4

=item C<output>

A Test::Builder::Output object.

=item C<plan>

A Test::Builder::TestPlan object.

=back

=item B<create( *@args )>

This method actually creates and returns a new Test::Builder instance.  It
takes the same optional named arguments as C<new>.

=item B<plan( Str $explanation?, Int $tests? )>

Sets the current test plan or throws an exception if there's already a plan in
place.  You have two options for the plan.  If you pass a pair such as C<tests
=Egt 10>, the plan is to run ten tests.  If you pass the string C<no_plan>,
there is no set number of tests to run.

Those are the only valid arguments.

You must have a plan set before you can record any tests.

=item B<ok returns Bit ( Bit $passed, Str $description = '' )>

Records that a test has passed or failed, depending on the value of C<$passed>,
recording C<$description> as an optional explanation.

=item B<todo returns Bit ( Bit $passed, Str $description?, Str $reason? )>

Records that a test has passed or failed, depending on C<$passed> with an
optional C<$description>, but marks it as a TODO test with an optional
C<$reason>.

=item B<skip( Int $num = 1, Str $reason = 'skipped' )>

Records the skipping of C<$num> tests (one by default), giving an optional
C<$reason> for skipping them.

=item B<skip_all()>

Skips all of the tests before running them.

Fails if there is a test plan set.

=item B<BAILOUT( Str $reason = '' )>

Aborts the entire test run.

=item B<get_test_number()>

Returns the number of the I<next> test to record.

=item B<report_test( Test::Builder::Test $test )>

Records a test.  Internal use only, probably.

=back

=head1 SEE ALSO

Perl 5 Test::Builder.

=head1 AUTHORS

Perl6::MetaModel 2.0 code by Stevan Little E<lt>stevan@iinteractive.comE<gt>

Perl 6 code by chromatic E<lt>chromatic@wgz.orgE<gt>

documentation by Stevan Little E<lt>stevan@iinteractive.comE<gt> and chromatic.

=head1 Perl 6 Code

  class Test::Builder-0.2.1;
  
  use Test::Builder::Test;
  use Test::Builder::Output;
  use Test::Builder::TestPlan;
  
  my  Test::Builder           $:singleton;
  has Test::Builder::Output   $.output handles 'diag';
  has Test::Builder::TestPlan $.testplan;
  has                         @:results;
  
  method new ( Test::Builder $Class: $plan?, $output? )
  {
      return $:singleton //= $Class.SUPER::new(
          testplan => $plan, output => $output
      );
  }
  
  method create ( Test::Builder $Class: $plan?, $output? )
  {
      return $Class.new( testplan => $plan, output => $output );
  }
  
  submethod BUILD
  (
      Test::Builder::TestPlan $.testplan?,
      Test::Builder::Output   $.output = Test::Builder::Output.new()
  )
  {}
  
  submethod DESTROY
  {
      my $footer = $.testplan.footer( +@:results );
      $.output.write( $footer ) if $footer;
  }
  
  method get_test_number
  {
      return +@:results + 1;
  }
  
  method plan ( Str $explanation?, Int $tests? )
  {
      fail "Plan already set!" if $.testplan;
  
      if $tests
      {
          $.testplan = Test::Builder::TestPlan.new( expect => $tests );
      }
      elsif $explanation eq 'no_plan'
      {
          $.testplan = Test::Builder::NullPlan.new();
      }
      else
      {
          fail "Unknown plan";
      }
  
      $.output.write( $.testplan.header() );
  }
  
  method ok returns Bit ( $self: Bit $passed, Str $description = '' )
  {
      $self.report_test(
          Test::Builder::Test.new(
              number      => $self.get_test_number(),
              passed      =>  $passed,
              description =>  $description,
          )
      );
  
      return $passed;
  }
  
  method diag ( Str $diagnostic = '' )
  {
      $.output.diag( $diagnostic );
  }
  
  method todo returns Bit ( $self: Bit $passed, Str $description?, Str $reason? )
  {
      $self.report_test(
          Test::Builder::Test.new(
              todo        => 1,
              number      => $self.get_test_number(),
              reason      =>  $reason,
              description =>  $description,
          )
      );
  
      return $passed;
  }
  
  method skip ( $self: Int $num = 1, Str $reason = 'skipped' )
  {
      for 1 .. $num
      {
          $self.report_test(
              Test::Builder::Test.new(
                  skip   => 1,
                  number => $self.get_test_number(),
                  reason =>  $reason,
              )
          );
      }
  }
  
  method skip_all
  {
      fail "Cannot skip_all with a plan" if $.testplan;
  
      $.output.write( "1..0" );
      exit 0;
  }
  
  method BAILOUT ( Str $reason = '' )
  {
      $.output.write( "Bail out!  $reason" );
      exit 255;
  }
  
  method report_test ( Test::Builder::Test $test )
  {
      fail 'No plan set!' unless $.testplan;
  
      push $.results, $test;
      $.output.write( $test.report() );
  }

=cut
