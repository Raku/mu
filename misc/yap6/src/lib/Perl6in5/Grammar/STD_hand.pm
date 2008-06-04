package Perl6in5::Grammar::STD_hand;

use warnings;
#use strict; #haha

use base 'Exporter';
our @EXPORT = qw ( make_parser );

use Perl6in5::Compiler::Trace; # set env var TRACE for trace output

# isa Perl6in5::Grammar someday (to inherit some of the common features below)

use Perl6in5::Grammar ':all'; # heehee; a source filter.
use Perl6in5::Compiler::Stream ':all';
use Perl6in5::Compiler::Parser ':all';
use Perl6in5::Compiler::Lexer ':all';

use Data::Dumper;
use Math::BigFloat;

no warnings 'reserved';
$Data::Dumper::Deparse = 1;

# The identifier of each of your rules must begin with a lowercase letter.   sorry.

#rule someRule sub { debug 'hello world' };


sub make_parser {
    my $input = shift;
    
    my $wS = sub { "" };
    
    my $lexer = iterator_to_stream(make_lexer($input,

    [ 'TRM'             ,qr/;\n*|\n+/         ,                            ],
    [ 'DLR'             ,qr/\$/               ,                            ],
    [ 'INT'             ,qr/\d+/              ,                            ],
    [ 'SAY'             ,qr/\bsay\b/          ,                            ],
    [ 'ID'              ,qr|[A-Za-z_]\w*|     ,                            ],
    [ 'OP'              ,qr#\*\*|["-=+*/()]#   ,                            ],
    [ 'WS'              ,qr/\s+/              ,$wS                         ],

    )); 

    my %VAR;
    $N{$End_of_Input} = 'End of Input';
    $N{$nothing} = 'nothing';

    my ($base, $expression, $factor, $program, $statement, $term, $scopeDeclarator, $sVar, $stringLit);
    my ($Base, $Expression, $Factor, $Program, $Statement, $Term, $ScopeDeclarator, $SVar, $StringLit);

    $Base               = parser { $base            ->(@_) };
    $Expression         = parser { $expression      ->(@_) };
    $Factor             = parser { $factor          ->(@_) };
    $Program            = parser { $program         ->(@_) };
    $Statement          = parser { $statement       ->(@_) };
    $Term               = parser { $term            ->(@_) };
    $ScopeDeclarator    = parser { $scopeDeclarator ->(@_) };
    $SVar               = parser { $sVar            ->(@_) };
    $StringLit          = parser { $stringLit       ->(@_) };

    # these hash keys are coderef (addresses)
    $N{ $End_of_Input      } = 'EOI';
    $N{ $nothing           } = '(nothing)';
    $N{ $Expression        } = 'Expression';
    $N{ $Term              } = 'Term';
    $N{ $Factor            } = 'Factor';
    $N{ $Statement         } = 'Statement';
    $N{ $Base              } = 'Base';
    $N{ $Program           } = 'Program';
    $N{ $ScopeDeclarator   } = 'my|our';
    $N{ $SVar              } = 'sVar';
    $N{ $StringLit         } = 'StringLit';
    
    sub execnow (&) { $_[0]->() }

    sub say (@) { print $_."\n" for @_ }

    sub adn (@) { 
        say "Adding AST node: ".(Dumper([map("$_",@_)]));
        Dumper([map("$_",@_)]);
    }

    my $handle_say_statement =
    #sub { print $_[1]."\n" };
    sub { adn('__PACKAGE__::say',$_[1]) };

    my $handle_assignment_statement =
    #sub { $VAR{ $_[0] } = $_[2] };
    sub { # keep a parse-time pad of declared variable names
            adn('__PACKAGE__::assign',@_[0,2]) };

    my $handle_declarator = 
    sub { bless([$_[0]] => 'Tuple') };

    $sVar = concatenate(l('DLR'),l('ID')) >> sub {
            "$_[0]$_[1]";
            #bless(["$_[0]$_[1]"] => 'Tuple')
        };

    my $handle_declaration_statement =
    #sub { $VAR{ $_[0] } = $_[2] };
    sub {   if ($_[0]) {
                warn "declaration of $_[1] masks ".
                "earlier declaration in the same scope" if exists $VAR{$_[1]};
                # keep a parse-time pad of declared variable names..
                # later such checking will be done during analysis
                # (after parsing)
                adn('__PACKAGE__::declare',($_[0],$_[1]))
            } else {
                die "$_[1] has yet to be declared" unless exists $VAR{$_[1]};
            }
            $_[3] = "$_[3]";
            shift;
            trace " \%N size... ".scalar(keys(%N));
            trace "setting $_[0] to $_[2]";
            $VAR{$_[0]} = $_[2];
            trace " \%N size... ".scalar(keys(%N));
            $handle_assignment_statement->(@_) };

    my $handle_infix_addition =
    sub { my $term = $_[1];
          sub { $_[0] + $term }};

    my $handle_infix_subtraction =
    sub { my $term = $_[1];
          sub { $_[0] - $term }}; 

    my $handle_infix_multiplication =
    sub { my $factor = $_[1];
          sub { $_[0] * $factor }};
          
    my $handle_infix_division =
    sub { my $factor = $_[1];
          warn "cannot divide by 0." unless $factor;
          $factor ||= 1; #silly, I know; this is only a toy interpreter.
          sub {$_[0] / $factor}};
          
    my $handle_infix_exponentiation_factor = sub { $_[1] };

    my $handle_empty_nothing = sub { 1 };

    my $handle_infix_exponentiation_operation = sub { $_[0] ** $_[1] };

    my $handle_variable_lookup = sub { die "Undeclared variable $_[0]" unless exists $VAR{$_[0]}; "$VAR{$_[0]}"; };

    my $handle_base_value = sub { $_[1] };

    my $handle_cascade =
    sub { my ($first, $rest) = @_;
          for my $f (@$rest) { 
              $first = $f->($first); 
          } $first};

    my $handle_string_lit = sub { $_[1] };

    my $handle_integer_inst =
    sub { Math::BigFloat->new($_[0]) };

    rule program {
        error(star($Statement) - $End_of_Input);
    };

    rule stringLit {
        l('OP','"') - l('ID') - l('OP','"') >> $handle_string_lit
    };

    rule scopeDeclarator {
        l('ID','my') | l('ID','our');
    };

    rule statement {
        l('SAY') - $Expression - l('TRM')
                 >> $handle_say_statement
               | (option($ScopeDeclarator) > $handle_declarator) - $sVar - l('OP', '=') - $Expression - l('TRM')
                 >> $handle_declaration_statement
    };

    rule expression {
    $Term - star(l('OP', '+') - $Term >>  $handle_infix_addition
             | l('OP', '-') - $Term >> $handle_infix_subtraction) 
                  >> $handle_cascade
    };

    rule term {
        $Factor - star(l('OP', '*') - $Factor >> $handle_infix_multiplication
                     | l('OP', '/') - $Factor >> $handle_infix_division)
        >> $handle_cascade
    };

    rule factor {
        $Base - ( l('OP', '**') - $Factor >> $handle_infix_exponentiation_factor
                | $nothing >> $handle_empty_nothing )
        >> $handle_infix_exponentiation_operation
    };

    rule base {
     (l('INT') > $handle_integer_inst)
          | ($sVar > $handle_variable_lookup)
          | l('OP', '(') - $Expression - l('OP', ')')
          >> $handle_base_value
    };

    sub {
        eval { $program->($lexer) };
        if ($@) {
            print "  Syntax Error...!";
            return 255;
        } else {
            return 0;
        }
    }
}

1;