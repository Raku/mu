use t::lib::AST;

plan tests => 1 * blocks();

run_tests();

__DATA__

=== TEST 1: concat and constant
--- regex: ab
--- ast
$VAR1 = {
          '_pos' => [
                    0,
                    2
                  ],
          'concat' => [
                      {
                        '_pos' => [
                                  0,
                                  1
                                ],
                        'constant' => 'a'
                      },
                      {
                        '_pos' => [
                                  1,
                                  2
                                ],
                        'constant' => 'b'
                      }
                    ]
        };




=== TEST 2: metasyntax '...'
--- regex: "'a'"
--- ast
$VAR1 = {
          'metasyntax' => {
                          '_pos' => [
                                    0,
                                    3
                                  ],
                          'metasyntax' => '\'a\''
                        }
        };




=== TEST 3: metasyntax <$...>
--- regex: ' <$a> '
--- ast
$VAR1 = {
          'quant' => {
                     '_pos' => [
                               0,
                               6
                             ],
                     'greedy' => '',
                     'quant' => '',
                     'term' => {
                               'metasyntax' => {
                                               '_pos' => [
                                                         1,
                                                         5
                                                       ],
                                               'metasyntax' => '$a',
                                               'modifier' => ''
                                             }
                             },
                     'ws1' => ' ',
                     'ws2' => ' ',
                     'ws3' => ''
                   }
        };




=== TEST 4: metasyntax <@...>
--- regex: ' <@foo> '
--- ast
$VAR1 = {
          'quant' => {
                     '_pos' => [
                               0,
                               8
                             ],
                     'greedy' => '',
                     'quant' => '',
                     'term' => {
                               'metasyntax' => {
                                               '_pos' => [
                                                         1,
                                                         7
                                                       ],
                                               'metasyntax' => '@foo',
                                               'modifier' => ''
                                             }
                             },
                     'ws1' => ' ',
                     'ws2' => ' ',
                     'ws3' => ''
                   }
        };




=== TEST 5: metasyntax <%...>
--- regex: ' <%hi> '
--- ast
$VAR1 = {
          'quant' => {
                     '_pos' => [
                               0,
                               7
                             ],
                     'greedy' => '',
                     'quant' => '',
                     'term' => {
                               'metasyntax' => {
                                               '_pos' => [
                                                         1,
                                                         6
                                                       ],
                                               'metasyntax' => '%hi',
                                               'modifier' => ''
                                             }
                             },
                     'ws1' => ' ',
                     'ws2' => ' ',
                     'ws3' => ''
                   }
        };




=== TEST 6: metasyntax
--- regex: ' <{ return $0.sqrt }> '
--- ast
$VAR1 = {
          'quant' => {
                     '_pos' => [
                               0,
                               22
                             ],
                     'greedy' => '',
                     'quant' => '',
                     'term' => {
                               'closure' => {
                                            '_pos' => [
                                                      1,
                                                      21
                                                    ],
                                            'closure' => '{ return $0.sqrt }',
                                            'modifier' => ''
                                          }
                             },
                     'ws1' => ' ',
                     'ws2' => ' ',
                     'ws3' => ''
                   }
        };




=== TEST 7: metasyntax
--- regex: ' <&foo()> '
--- ast
$VAR1 = {
          'quant' => {
                     '_pos' => [
                               0,
                               10
                             ],
                     'greedy' => '',
                     'quant' => '',
                     'term' => {
                               'metasyntax' => {
                                               '_pos' => [
                                                         1,
                                                         9
                                                       ],
                                               'metasyntax' => '&foo()',
                                               'modifier' => ''
                                             }
                             },
                     'ws1' => ' ',
                     'ws2' => ' ',
                     'ws3' => ''
                   }
        };




=== TEST 8: alt
--- regex: 'a|b'
--- ast
$VAR1 = {
          '_pos' => [
                    0,
                    3
                  ],
          'alt1' => [
                    {
                      '_pos' => [
                                0,
                                1
                              ],
                      'constant' => 'a'
                    },
                    {
                      '_pos' => [
                                2,
                                3
                              ],
                      'constant' => 'b'
                    }
                  ]
        };




=== TEST 9: special chars
--- regex: "\\d \\s"
--- ast
$VAR1 = {
          '_pos' => [
                    0,
                    5
                  ],
          'concat' => [
                      {
                        'quant' => {
                                   '_pos' => [
                                             0,
                                             3
                                           ],
                                   'greedy' => '',
                                   'quant' => '',
                                   'term' => {
                                             '_pos' => [
                                                       0,
                                                       2
                                                     ],
                                             'special_char' => '\\d'
                                           },
                                   'ws1' => '',
                                   'ws2' => ' ',
                                   'ws3' => ''
                                 }
                      },
                      {
                        '_pos' => [
                                  3,
                                  5
                                ],
                        'special_char' => '\\s'
                      }
                    ]
        };




=== TEST 10: closures
--- regex: " a { say 'hi' } "
--- ast
$VAR1 = {
          '_pos' => [
                    0,
                    16
                  ],
          'concat' => [
                      {
                        'quant' => {
                                   '_pos' => [
                                             0,
                                             3
                                           ],
                                   'greedy' => '',
                                   'quant' => '',
                                   'term' => {
                                             '_pos' => [
                                                       1,
                                                       2
                                                     ],
                                             'constant' => 'a'
                                           },
                                   'ws1' => ' ',
                                   'ws2' => ' ',
                                   'ws3' => ''
                                 }
                      },
                      {
                        'quant' => {
                                   '_pos' => [
                                             3,
                                             16
                                           ],
                                   'greedy' => '',
                                   'quant' => '',
                                   'term' => {
                                             'closure' => {
                                                          '_pos' => [
                                                                    3,
                                                                    15
                                                                  ],
                                                          'closure' => '{ say \'hi\' }',
                                                          'modifier' => 'plain'
                                                        }
                                           },
                                   'ws1' => '',
                                   'ws2' => ' ',
                                   'ws3' => ''
                                 }
                      }
                    ]
        };




=== TEST 11: closure quantifier
--- regex: " a**{1..2} "
--- ast
$VAR1 = {
          'quant' => {
                     '_pos' => [
                               0,
                               11
                             ],
                     'greedy' => '',
                     'quant' => {
                                'closure' => '{1..2}'
                              },
                     'term' => {
                               '_pos' => [
                                         1,
                                         2
                                       ],
                               'constant' => 'a'
                             },
                     'ws1' => ' ',
                     'ws2' => '',
                     'ws3' => ' '
                   }
        };




=== TEST 12: quantifiers
--- regex: " a+ b? "
--- ast
$VAR1 = {
          '_pos' => [
                    0,
                    7
                  ],
          'concat' => [
                      {
                        'quant' => {
                                   '_pos' => [
                                             0,
                                             4
                                           ],
                                   'greedy' => '',
                                   'quant' => '+',
                                   'term' => {
                                             '_pos' => [
                                                       1,
                                                       2
                                                     ],
                                             'constant' => 'a'
                                           },
                                   'ws1' => ' ',
                                   'ws2' => '',
                                   'ws3' => ' '
                                 }
                      },
                      {
                        'quant' => {
                                   '_pos' => [
                                             4,
                                             7
                                           ],
                                   'greedy' => '',
                                   'quant' => '?',
                                   'term' => {
                                             '_pos' => [
                                                       4,
                                                       5
                                                     ],
                                             'constant' => 'b'
                                           },
                                   'ws1' => '',
                                   'ws2' => '',
                                   'ws3' => ' '
                                 }
                      }
                    ]
        };




=== TEST 13: subrule (<foo>)
--- regex: " <foo> "
--- ast
$VAR1 = {
          'quant' => {
                     '_pos' => [
                               0,
                               7
                             ],
                     'greedy' => '',
                     'quant' => '',
                     'term' => {
                               'metasyntax' => {
                                               '_pos' => [
                                                         1,
                                                         6
                                                       ],
                                               'metasyntax' => 'foo',
                                               'modifier' => ''
                                             }
                             },
                     'ws1' => ' ',
                     'ws2' => ' ',
                     'ws3' => ''
                   }
        };




=== TEST 14: subrule (<?foo>)
--- regex: " <?foo> "
--- ast
$VAR1 = {
          'quant' => {
                     '_pos' => [
                               0,
                               8
                             ],
                     'greedy' => '',
                     'quant' => '',
                     'term' => {
                               'metasyntax' => {
                                               '_pos' => [
                                                         1,
                                                         7
                                                       ],
                                               'metasyntax' => 'foo',
                                               'modifier' => '?'
                                             }
                             },
                     'ws1' => ' ',
                     'ws2' => ' ',
                     'ws3' => ''
                   }
        };




=== TEST 15: subrule (quanlified)
--- regex: " <Bar.foo> "
--- ast
$VAR1 = {
          'quant' => {
                     '_pos' => [
                               0,
                               11
                             ],
                     'greedy' => '',
                     'quant' => '',
                     'term' => {
                               'metasyntax' => {
                                               '_pos' => [
                                                         1,
                                                         10
                                                       ],
                                               'metasyntax' => 'Bar.foo',
                                               'modifier' => ''
                                             }
                             },
                     'ws1' => ' ',
                     'ws2' => ' ',
                     'ws3' => ''
                   }
        };




=== TEST 16: capure (...)
--- regex: " (a) "
--- ast
$VAR1 = {
          'quant' => {
                     '_pos' => [
                               0,
                               5
                             ],
                     'greedy' => '',
                     'quant' => '',
                     'term' => {
                               '_pos' => [
                                         1,
                                         4
                                       ],
                               'capturing_group' => {
                                                    '_pos' => [
                                                              2,
                                                              3
                                                            ],
                                                    'constant' => 'a'
                                                  }
                             },
                     'ws1' => ' ',
                     'ws2' => ' ',
                     'ws3' => ''
                   }
        };




=== TEST 17: capure ( ... )
--- regex: " ( a ) "
--- ast
$VAR1 = {
          'quant' => {
                     '_pos' => [
                               0,
                               7
                             ],
                     'greedy' => '',
                     'quant' => '',
                     'term' => {
                               '_pos' => [
                                         1,
                                         6
                                       ],
                               'capturing_group' => {
                                                    'quant' => {
                                                               '_pos' => [
                                                                         2,
                                                                         5
                                                                       ],
                                                               'greedy' => '',
                                                               'quant' => '',
                                                               'term' => {
                                                                         '_pos' => [
                                                                                   3,
                                                                                   4
                                                                                 ],
                                                                         'constant' => 'a'
                                                                       },
                                                               'ws1' => ' ',
                                                               'ws2' => ' ',
                                                               'ws3' => ''
                                                             }
                                                  }
                             },
                     'ws1' => ' ',
                     'ws2' => ' ',
                     'ws3' => ''
                   }
        };




=== TEST 18: named capure ( ... )
--- regex: ' $abc := (a) '
--- ast
$VAR1 = {
          'quant' => {
                     '_pos' => [
                               0,
                               13
                             ],
                     'greedy' => '',
                     'quant' => '',
                     'term' => {
                               'named_capture' => {
                                                  '_pos' => [
                                                            1,
                                                            12
                                                          ],
                                                  'ident' => {
                                                             'variable' => '$abc'
                                                           },
                                                  'rule' => {
                                                            '_pos' => [
                                                                      9,
                                                                      12
                                                                    ],
                                                            'capturing_group' => {
                                                                                 '_pos' => [
                                                                                           10,
                                                                                           11
                                                                                         ],
                                                                                 'constant' => 'a'
                                                                               }
                                                          }
                                                }
                             },
                     'ws1' => ' ',
                     'ws2' => ' ',
                     'ws3' => ''
                   }
        };




=== TEST 19: non-capture groups
--- regex: ' [ a ] '
--- ast
$VAR1 = {
          'quant' => {
                     '_pos' => [
                               0,
                               7
                             ],
                     'greedy' => '',
                     'quant' => '',
                     'term' => {
                               'quant' => {
                                          '_pos' => [
                                                    2,
                                                    5
                                                  ],
                                          'greedy' => '',
                                          'quant' => '',
                                          'term' => {
                                                    '_pos' => [
                                                              3,
                                                              4
                                                            ],
                                                    'constant' => 'a'
                                                  },
                                          'ws1' => ' ',
                                          'ws2' => ' ',
                                          'ws3' => ''
                                        }
                             },
                     'ws1' => ' ',
                     'ws2' => ' ',
                     'ws3' => ''
                   }
        };




=== TEST 20: named capture + [ ... ]
--- regex: ' $a := [a] '
--- ast
$VAR1 = {
          'quant' => {
                     '_pos' => [
                               0,
                               11
                             ],
                     'greedy' => '',
                     'quant' => '',
                     'term' => {
                               'named_capture' => {
                                                  '_pos' => [
                                                            1,
                                                            10
                                                          ],
                                                  'ident' => {
                                                             'variable' => '$a'
                                                           },
                                                  'rule' => {
                                                            '_pos' => [
                                                                      8,
                                                                      9
                                                                    ],
                                                            'constant' => 'a'
                                                          }
                                                }
                             },
                     'ws1' => ' ',
                     'ws2' => ' ',
                     'ws3' => ''
                   }
        };




