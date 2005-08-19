###
### simple-expr-parser.pl
###

## Chapter 8 section 4

use v6;

use Parser;# :all;
use Lexer;# :all;
require 'it2stream.p6';

## SETUP PARSER
my $expression;
my $parser = sub { $expression.(@_) };
$expression = alternate(concatenate(lookfor('INT'),
                                    lookfor(['OP', '+']),
                                    $parser),
                        concatenate(lookfor('INT'),
                                    lookfor(['OP', '*']),
                                    $parser),
                        concatenate(lookfor(['OP', '(']),
                                    $parser,
                                    lookfor(['OP', ')'])),
                        lookfor('INT'));

my $entire_input = concatenate($parser, &End_of_Input);
my @input = q[2 * 3 + (4 * 5)];

## SETUP LEXER
my $input = sub { return @input.shift };
my $lexer = iterator_to_stream(
               make_lexer($input,
                       ['TERMINATOR', ";\n*|\n+"                 ], 
                       ['INT',        '\d+'                      ],
                       ['PRINT',      '\bprint\b'                ],
                       ['IDENTIFIER', '[A-Za-z_]\w*'             ],
                       ['OP',         '\*\*|[-=+*/()]'           ],
                       ['WHITESPACE', '\s+',          sub { "" } ],
               )
             ); 

say 'lexer looks like: ', show($lexer, 10);

my($result, $remaining_input) = $entire_input.($lexer);
if ?$result {
  say $result.perl;
} else {
  say "Didn't get anything back, must be a parse error.";
}
