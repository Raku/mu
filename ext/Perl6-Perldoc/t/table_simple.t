# Testing this Pod specification...
my $perldoc_data = <<'END_PERLDOC';
=table
    The Shoveller   Eddie Stevens     King Arthur's singing shovel   
    Blue Raja       Geoffrey Smith    Master of cutlery              
    Mr Furious      Roy Orson         Ticking time bomb of fury      
    The Bowler      Carol Pinnsler    Haunted bowling ball

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
            'typename' => 'table',
            'content' => [
              '    The Shoveller   Eddie Stevens     King Arthur\'s singing shovel   
    Blue Raja       Geoffrey Smith    Master of cutlery              
    Mr Furious      Roy Orson         Ticking time bomb of fury      
    The Bowler      Carol Pinnsler    Haunted bowling ball
'
            ],
            'style' => 'abbreviated',
            'rows' => [
              bless( {
                'cells' => [
                  bless( {
                    'content' => [
                      bless( {
                        'typename' => 'pod',
                        'content' => [
                          bless( {
                            'typename' => 'para',
                            'content' => [
                              'The Shoveller'
                            ],
                            'style' => 'implicit'
                          }, 'Perl6::Perldoc::Block::para' )
                        ],
                        'style' => 'implicit'
                      }, 'Perl6::Perldoc::Block::pod' )
                    ]
                  }, 'Perl6::Perldoc::Block::table::Cell' ),
                  bless( {
                    'content' => [
                      bless( {
                        'typename' => 'pod',
                        'content' => [
                          bless( {
                            'typename' => 'para',
                            'content' => [
                              'Eddie Stevens'
                            ],
                            'style' => 'implicit'
                          }, 'Perl6::Perldoc::Block::para' )
                        ],
                        'style' => 'implicit'
                      }, 'Perl6::Perldoc::Block::pod' )
                    ]
                  }, 'Perl6::Perldoc::Block::table::Cell' ),
                  bless( {
                    'content' => [
                      bless( {
                        'typename' => 'pod',
                        'content' => [
                          bless( {
                            'typename' => 'para',
                            'content' => [
                              'King Arthur\'s singing shovel'
                            ],
                            'style' => 'implicit'
                          }, 'Perl6::Perldoc::Block::para' )
                        ],
                        'style' => 'implicit'
                      }, 'Perl6::Perldoc::Block::pod' )
                    ]
                  }, 'Perl6::Perldoc::Block::table::Cell' )
                ]
              }, 'Perl6::Perldoc::Block::table::Row' ),
              bless( {
                'cells' => [
                  bless( {
                    'content' => [
                      bless( {
                        'typename' => 'pod',
                        'content' => [
                          bless( {
                            'typename' => 'para',
                            'content' => [
                              'Blue Raja'
                            ],
                            'style' => 'implicit'
                          }, 'Perl6::Perldoc::Block::para' )
                        ],
                        'style' => 'implicit'
                      }, 'Perl6::Perldoc::Block::pod' )
                    ]
                  }, 'Perl6::Perldoc::Block::table::Cell' ),
                  bless( {
                    'content' => [
                      bless( {
                        'typename' => 'pod',
                        'content' => [
                          bless( {
                            'typename' => 'para',
                            'content' => [
                              'Geoffrey Smith'
                            ],
                            'style' => 'implicit'
                          }, 'Perl6::Perldoc::Block::para' )
                        ],
                        'style' => 'implicit'
                      }, 'Perl6::Perldoc::Block::pod' )
                    ]
                  }, 'Perl6::Perldoc::Block::table::Cell' ),
                  bless( {
                    'content' => [
                      bless( {
                        'typename' => 'pod',
                        'content' => [
                          bless( {
                            'typename' => 'para',
                            'content' => [
                              'Master of cutlery'
                            ],
                            'style' => 'implicit'
                          }, 'Perl6::Perldoc::Block::para' )
                        ],
                        'style' => 'implicit'
                      }, 'Perl6::Perldoc::Block::pod' )
                    ]
                  }, 'Perl6::Perldoc::Block::table::Cell' )
                ]
              }, 'Perl6::Perldoc::Block::table::Row' ),
              bless( {
                'cells' => [
                  bless( {
                    'content' => [
                      bless( {
                        'typename' => 'pod',
                        'content' => [
                          bless( {
                            'typename' => 'para',
                            'content' => [
                              'Mr Furious'
                            ],
                            'style' => 'implicit'
                          }, 'Perl6::Perldoc::Block::para' )
                        ],
                        'style' => 'implicit'
                      }, 'Perl6::Perldoc::Block::pod' )
                    ]
                  }, 'Perl6::Perldoc::Block::table::Cell' ),
                  bless( {
                    'content' => [
                      bless( {
                        'typename' => 'pod',
                        'content' => [
                          bless( {
                            'typename' => 'para',
                            'content' => [
                              'Roy Orson'
                            ],
                            'style' => 'implicit'
                          }, 'Perl6::Perldoc::Block::para' )
                        ],
                        'style' => 'implicit'
                      }, 'Perl6::Perldoc::Block::pod' )
                    ]
                  }, 'Perl6::Perldoc::Block::table::Cell' ),
                  bless( {
                    'content' => [
                      bless( {
                        'typename' => 'pod',
                        'content' => [
                          bless( {
                            'typename' => 'para',
                            'content' => [
                              'Ticking time bomb of fury'
                            ],
                            'style' => 'implicit'
                          }, 'Perl6::Perldoc::Block::para' )
                        ],
                        'style' => 'implicit'
                      }, 'Perl6::Perldoc::Block::pod' )
                    ]
                  }, 'Perl6::Perldoc::Block::table::Cell' )
                ]
              }, 'Perl6::Perldoc::Block::table::Row' ),
              bless( {
                'cells' => [
                  bless( {
                    'content' => [
                      bless( {
                        'typename' => 'pod',
                        'content' => [
                          bless( {
                            'typename' => 'para',
                            'content' => [
                              'The Bowler'
                            ],
                            'style' => 'implicit'
                          }, 'Perl6::Perldoc::Block::para' )
                        ],
                        'style' => 'implicit'
                      }, 'Perl6::Perldoc::Block::pod' )
                    ]
                  }, 'Perl6::Perldoc::Block::table::Cell' ),
                  bless( {
                    'content' => [
                      bless( {
                        'typename' => 'pod',
                        'content' => [
                          bless( {
                            'typename' => 'para',
                            'content' => [
                              'Carol Pinnsler'
                            ],
                            'style' => 'implicit'
                          }, 'Perl6::Perldoc::Block::para' )
                        ],
                        'style' => 'implicit'
                      }, 'Perl6::Perldoc::Block::pod' )
                    ]
                  }, 'Perl6::Perldoc::Block::table::Cell' ),
                  bless( {
                    'content' => [
                      bless( {
                        'typename' => 'pod',
                        'content' => [
                          bless( {
                            'typename' => 'para',
                            'content' => [
                              'Haunted bowling ball'
                            ],
                            'style' => 'implicit'
                          }, 'Perl6::Perldoc::Block::para' )
                        ],
                        'style' => 'implicit'
                      }, 'Perl6::Perldoc::Block::pod' )
                    ]
                  }, 'Perl6::Perldoc::Block::table::Cell' )
                ]
              }, 'Perl6::Perldoc::Block::table::Row' )
            ]
          }, 'Perl6::Perldoc::Block::table' )
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
