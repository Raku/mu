#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;
use Test::Exception;

use Perl6::MetaModel;

=pod

this test was converted from t/oo/submethods.t

=cut

{
  my $was_in_foo_build = 0;
  my $was_in_bar_build = 0;

  lives_ok {   
    class 'Foo' => { 
        'is' => [ 'Perl6::Object' ],
        'instance' => {
            'BUILD' => sub { $was_in_foo_build++ } 
        }
    };

    class 'Bar' => {
        'is' => [ 'Foo' ],
        'instance' => {
            'BUILD' => sub { $was_in_bar_build++ }
        }
    };
  } '... classes were built ok';

  my $a;
  ok($a = Foo->new(),    "Foo.new() worked (1)");
  is($was_in_foo_build, 1,      "Foo's BUILD was called");
  # is instead of todo_is to avoid unexpected succeedings
  is($was_in_bar_build, 0,      "Bar's BUILD was not called");

  my $b;
  ok($b = Bar->new(),    "Bar.new() worked");
  is($was_in_foo_build, 2,      "Foo's BUILD was called again");
  is($was_in_bar_build, 1,      "Bar's BUILD was called, too");

  # The next three tests are basically exactly the same as the first three tests
  # (not counting the initial class definition). This is to verify our call to
  # Bar.new didn't removed/changed some internal structures which'd prevent
  # Foo.BUILD of getting called.
  my $c;
  ok($c = Foo->new(), "Foo.new() worked (2)");
  is($was_in_foo_build, 3,      "Foo's BUILD was called again");
  is($was_in_bar_build, 1,      "Bar's BUILD was not called again");
}

# See thread "BUILD and other submethods" on p6l
# http://groups-beta.google.com/group/perl.perl6.language/msg/e9174e5538ded4a3
{
  my $was_in_baz_submethod  = 0;
  my $was_in_grtz_submethod = 0;

  class 'Baz' => { 
      'is' => [ 'Perl6::Object' ],
      'instance' => {
          'submethods' => {
              'blarb' => sub { $was_in_baz_submethod++ }
           }
      }
  };
  class 'Grtz' => { 
      'is' => [ 'Baz' ],
      'instance' => {
          'submethods' => {
              'blarb' => sub { $was_in_grtz_submethod++ }
           }
      }
  };
  
  my ($baz, $grtz);
  ok($baz  = Baz->new,  "Baz.new() worked");
  ok($grtz = Grtz->new, "Grtz.new() worked");

  eval { $baz->blarb };
  is($was_in_baz_submethod,  1, "Baz's submethod blarb was called");
  # No :todo to avoid unexpected suceedings
  is($was_in_grtz_submethod, 0, "Grtz's submethod blarb was not called");

  eval { $grtz->blarb };
  is($was_in_baz_submethod,  1, "Baz's submethod blarb was not called again");
  # No :todo to avoid unexpected suceedings
  is($was_in_grtz_submethod, 1, "Grtz's submethod blarb was called now");

=pod

  NOTE: 
  This test will not work because it cannot dispatch this correctly.

  eval { 
      $grtz->Baz::blarb() 
  };
  is($was_in_baz_submethod,  2, "Baz's submethod blarb was called again now");
  # No :todo to avoid unexpected suceedings
  is($was_in_grtz_submethod, 1, "Grtz's submethod blarb was not called again");
=cut
}

=pod

The next set of tests were regarding Roles and BUILD, 
which is not currently supported either.

=cut

# BUILD with signatures that don't map directly to attributes
{
  class 'ClassC' => {
    'is' => [ 'Perl6::Object' ],
    'instance' => {
        'attrs' => [ '$.double_value' ],
        'BUILD' => sub {
            my ($self, %params) = @_;
            $params{'value'} ||= 1;
            _('$.double_value' => $params{'value'} * 2);
        }
     }
  };

  my $C = ClassC->new();
  is($C->double_value, 2,
    'BUILD() should allow default values of optional params in signature');

  my $C2 = ClassC->new(value => 100);
  is($C2->double_value, 200, '... or value passed in');
}




