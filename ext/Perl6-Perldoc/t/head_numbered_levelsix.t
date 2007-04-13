# Testing this Pod specification...
my $perldoc_data = <<'END_PERLDOC';
=head1 Head 1
=head2 Head 2
=head3 Head 3
=head4 Head 4
=head5 Head 5
=head6 # The Rescue of the Kobayashi Maru

END_PERLDOC

# Expect it to parse to this ADT...
my $expected_structure = eval <<'END_EXPECTED';
$VAR1 = bless( {
  'warnings' => [],
  'errors' => [],
  'tree' => bless( {
    'typename' => '(document)',
    'content' => [
      bless( {
        'typename' => 'pod',
        'content' => [
          bless( {
            'typename' => 'head1',
            'content' => [
              'Head 1
'
            ],
            'style' => 'abbreviated'
          }, 'Perl6::Perldoc::Block::head1' ),
          bless( {
            'typename' => 'head2',
            'content' => [
              'Head 2
'
            ],
            'style' => 'abbreviated'
          }, 'Perl6::Perldoc::Block::head2' ),
          bless( {
            'typename' => 'head3',
            'content' => [
              'Head 3
'
            ],
            'style' => 'abbreviated'
          }, 'Perl6::Perldoc::Block::head3' ),
          bless( {
            'typename' => 'head4',
            'content' => [
              'Head 4
'
            ],
            'style' => 'abbreviated'
          }, 'Perl6::Perldoc::Block::head4' ),
          bless( {
            'typename' => 'head5',
            'content' => [
              'Head 5
'
            ],
            'style' => 'abbreviated'
          }, 'Perl6::Perldoc::Block::head5' ),
          bless( {
            'typename' => 'head6',
            'number' => '1.1.1.1.1.1',
            'content' => [
              'The Rescue of the Kobayashi Maru
'
            ],
            'style' => 'abbreviated'
          }, 'Perl6::Perldoc::Block::head6' )
        ],
        'style' => 'implicit'
      }, 'Perl6::Perldoc::Block::pod' )
    ]
  }, 'Perl6::Perldoc::Document' )
}, 'Perl6::Perldoc::Parser::ReturnVal' );

END_EXPECTED

# Remove filenames from error messages (since two sources differ)...
for my $msg ( @{ $expected_structure->{warnings} },
              @{ $expected_structure->{errors} }
) {
    $msg =~ s{at \S+ line}{at line};
}

use Perl6::Perldoc::Parser;
use Test::More 'no_plan';

# Open input filehandle on Pod daa and parse it...
open my $fh, '<', \$perldoc_data
    or die "Could not open file on test data";
my $representation = Perl6::Perldoc::Parser->parse($fh ,{all_pod=>1});

# Walk resulting representation and expectation tree in parallel, comparing...
compare(
    '  ',                     # Indent
    'return value',           # Description
    {%{$representation}},     # What we got
    {%{$expected_structure}}  # What we expected
);


use Scalar::Util qw< reftype blessed >;

# Only consider valid accessor methods...
my %is_valid_scalar_method;
my %is_valid_list_method;
BEGIN {
   @is_valid_scalar_method{ qw< typename style number target > } = ();
   @is_valid_list_method{   qw< content rows cells >           } = ();
}

# Walk two trees, comparing nodes as we go...
sub compare {
    my ($indent, $desc, $rep, $expected) = @_;

    # Verify data at current node is of correct class...
    my ($rep_class, $expected_class)
        = map {ref($_) || q{STRING}} $rep, $expected;

    is $rep_class, $expected_class => "$indent$desc is $expected_class";

    # Recurse down trees according to type of node expected...
    $indent .= q{  };
    my $expected_type = reftype($expected) || q{STRING};

    # If current node an object -> match keys as method calls...
    if (blessed $expected) {
        for my $attr ( keys %{ $expected } ) {
            # Expected subnode must be retrieved via known accessor...
            my $is_scalar = exists $is_valid_scalar_method{$attr};
            my $is_list   = exists $is_valid_list_method{$attr};
            if (!$is_scalar && !$is_list) {
                fail "Internal error: unknown method $attr() "
                   . "expected for $rep_class node";
            }

            # Known accessor must be available...
            elsif (! $rep->can($attr) ) {
                fail "Can't call $attr() on $rep_class node";
            }

            # If accessor returns a list, recursively compare the lists...
            elsif ($is_list) {
                compare($indent,$attr, [$rep->$attr], $expected->{$attr});
            }

            # If accessor returns a scalar, string-compare the values...
            else {
                compare($indent,$attr, scalar($rep->$attr), $expected->{$attr});
            }
        }
    }
    
    # If current node a hash -> match keys as hash entries...
    elsif ($expected_type eq 'HASH') {
        for my $attr ( keys %{ $expected } ) {
            compare($indent, $attr, $rep->{$attr}, $expected->{$attr});
        }
    }

    # If current node an array -> match each element in sequence...
    elsif ($expected_type eq 'ARRAY') {
        for my $idx ( 0..$#{$expected} ) {
            compare($indent,"[$idx]", $rep->[$idx], $expected->[$idx]);
        }
    }

    # Otherwise current node is raw text -> simple string comparison...
    else {
        is $rep, $expected  =>  "$indent$desc content was correct";
    }
} 
