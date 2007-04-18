# Testing this Pod specification...
my $perldoc_data = <<'END_PERLDOC';
B<basis>
C<code>
E<entity>
I<important>
K<keyboard input>
N<note>
P<placement link>
R<replaceable>
S<space preserving>
T<terminal output>
U<unusual>
V<verbatim>
Z<zero width comment>

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
            'typename' => 'para',
            'content' => [
              bless( {
                'typename' => 'B',
                'content' => [
                  'basis'
                ],
                'style' => 'formatting'
              }, 'Perl6::Perldoc::FormattingCode::B' ),
              '
',
              bless( {
                'typename' => 'C',
                'content' => [
                  'code'
                ],
                'style' => 'formatting'
              }, 'Perl6::Perldoc::FormattingCode::C' ),
              '
',
              bless( {
                'typename' => 'E',
                'content' => [
                  'entity'
                ],
                'style' => 'formatting'
              }, 'Perl6::Perldoc::FormattingCode::E' ),
              '
',
              bless( {
                'typename' => 'I',
                'content' => [
                  'important'
                ],
                'style' => 'formatting'
              }, 'Perl6::Perldoc::FormattingCode::I' ),
              '
',
              bless( {
                'typename' => 'K',
                'content' => [
                  'keyboard input'
                ],
                'style' => 'formatting'
              }, 'Perl6::Perldoc::FormattingCode::K' ),
              '
',
              bless( {
                'typename' => 'N',
                'content' => [
                  'note'
                ],
                'style' => 'formatting'
              }, 'Perl6::Perldoc::FormattingCode::N' ),
              '
',
              bless( {
                'typename' => 'P',
                'content' => [
                  'placement link'
                ],
                'style' => 'formatting',
                'target' => 'placement link'
              }, 'Perl6::Perldoc::FormattingCode::P' ),
              '
',
              bless( {
                'typename' => 'R',
                'content' => [
                  'replaceable'
                ],
                'style' => 'formatting'
              }, 'Perl6::Perldoc::FormattingCode::R' ),
              '
',
              bless( {
                'typename' => 'S',
                'content' => [
                  'space preserving'
                ],
                'style' => 'formatting'
              }, 'Perl6::Perldoc::FormattingCode::S' ),
              '
',
              bless( {
                'typename' => 'T',
                'content' => [
                  'terminal output'
                ],
                'style' => 'formatting'
              }, 'Perl6::Perldoc::FormattingCode::T' ),
              '
',
              bless( {
                'typename' => 'U',
                'content' => [
                  'unusual'
                ],
                'style' => 'formatting'
              }, 'Perl6::Perldoc::FormattingCode::U' ),
              '
',
              bless( {
                'typename' => 'V',
                'content' => [
                  'verbatim'
                ],
                'style' => 'formatting'
              }, 'Perl6::Perldoc::FormattingCode::V' ),
              '
',
              bless( {
                'typename' => 'Z',
                'content' => [
                  'zero width comment'
                ],
                'style' => 'formatting'
              }, 'Perl6::Perldoc::FormattingCode::Z' ),
              '
'
            ],
            'style' => 'implicit'
          }, 'Perl6::Perldoc::Block::para' )
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
