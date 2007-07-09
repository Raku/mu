use t::lib::AST;

plan tests => 1 * blocks();

run_tests();

__DATA__

=== TEST 1: constant
--- regex: ab
--- ast
$VAR1 = {
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
                     'ws2' => ' ',
                     'greedy' => '',
                     'quant' => '',
                     'ws1' => ' ',
                     'ws3' => '',
                     'term' => {
                               'metasyntax' => {
                                               '_pos' => [
                                                         1,
                                                         5
                                                       ],
                                               'metasyntax' => '$a',
                                               'modifier' => ''
                                             }
                             }
                   }
        };




=== TEST 4: metasyntax <@...>
--- regex: ' <@foo> '
--- ast
$VAR1 = {
          'quant' => {
                     'ws2' => ' ',
                     'greedy' => '',
                     'quant' => '',
                     'ws1' => ' ',
                     'ws3' => '',
                     'term' => {
                               'metasyntax' => {
                                               '_pos' => [
                                                         1,
                                                         7
                                                       ],
                                               'metasyntax' => '@foo',
                                               'modifier' => ''
                                             }
                             }
                   }
        };




