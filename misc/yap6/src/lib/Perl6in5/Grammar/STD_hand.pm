package Perl6in5::Grammar::STD_hand;

use warnings;
#use strict; #haha

use base 'Exporter';
our @EXPORT = qw ( make_parser );

use Perl6in5::Compiler::Trace; # set env var TRACE for trace output

# isa Perl6in5::Grammar someday (to inherit some of the common features below)

use Perl6in5::Grammar ':all'; # heehee; a source filter.
#use P65GL;
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
    [ 'USEV6'           ,qr/use\s+v6;/,                            ],
    [ 'NL'              ,qr/\n+/              ,                            ],
    [ 'ID'              ,qr|[A-Za-z_]\w*|     ,                            ],
    [ 'INT'             ,qr|\d+|              ,                            ],
    [ 'WS'              ,qr/\s+/              ,$wS                         ],
    [ 'C'               ,qr#.#                ,                            ],

    )); 

    my %VAR;
    $N{$End_of_Input} = 'End of Input';
    $N{$nothing} = 'nothing';

    my ($base, $expr, $factor, $program, $stmt, $term, $sVar, $stringLit,
        $usev6, $comma, $stmtList, $blockList, $newline, $blkDecl, $blkBare, $blkPrmbl, $blkType, $blkRetT, $blkModf, $word, $keyword, $keywords, $stmtTrm, $scpDecl, $block, $arg, $blkPrms);
    my ($Base, $Expr, $Factor, $Program, $Stmt, $Term, $SVar, $StringLit,
        $Usev6, $Comma, $StmtList, $BlockList, $Newline, $BlkDecl, $BlkBare, $BlkPrmbl, $BlkType, $BlkRetT, $BlkModf, $Word, $Keyword, $Keywords, $StmtTrm, $ScpDecl, $Block, $Arg, $BlkPrms);

    $Base               = parser { $base            ->(@_) };
    $Expr               = parser { $expr            ->(@_) };
    $Factor             = parser { $factor          ->(@_) };
    $Program            = parser { $program         ->(@_) };
    $Stmt               = parser { $stmt            ->(@_) };
    $Term               = parser { $term            ->(@_) };
    $SVar               = parser { $sVar            ->(@_) };
    $StringLit          = parser { $stringLit       ->(@_) };
    $Usev6              = parser { $usev6       ->(@_) };
    $Comma              = parser { $comma       ->(@_) };
    $StmtList         = parser { $stmtList      ->(@_) };
    $BlockList        = parser { $blockList     ->(@_) };
    $Newline          = parser { $newline       ->(@_) };
    $BlkDecl          = parser { $blkDecl       ->(@_) };
    $BlkBare          = parser { $blkBare       ->(@_) };
    $BlkPrmbl         = parser { $blkPrmbl      ->(@_) };
    $BlkType          = parser { $blkType       ->(@_) };
    $BlkRetT          = parser { $blkRetT       ->(@_) };
    $BlkModf          = parser { $blkModf       ->(@_) };
    $Word             = parser { $word          ->(@_) };
    $Keyword          = parser { $keyword       ->(@_) };
    $Keywords         = parser { $keywords      ->(@_) };
    $StmtTrm          = parser { $stmtTrm       ->(@_) };
    $ScpDecl          = parser { $scpDecl       ->(@_) };
    $Block            = parser { $block         ->(@_) };
    $Arg              = parser { $arg           ->(@_) };
    $BlkPrms          = parser { $blkPrms       ->(@_) };

    # these hash keys are coderef (addresses)
    $N{ $End_of_Input      } = 'EOI';
    $N{ $nothing           } = '(nothing)';
    $N{ $Expr              } = 'Expr';
    $N{ $Term              } = 'Term';
    $N{ $Factor            } = 'Factor';
    $N{ $Stmt              } = 'Stmt';
    $N{ $Base              } = 'Base';
    $N{ $Program           } = 'Program';
    $N{ $ScpDecl           } = 'ScpDecl';
    $N{ $SVar              } = 'SVar';
    $N{ $StringLit         } = 'StringLit';
    
    sub execnow (&) { $_[0]->() }

    sub say (@) { print $_."\n" for @_ }

    sub adn (@) { 
        say "Adding AST node: ".(Dumper([map("$_",@_)]));
        Dumper([map("$_",@_)]);
    }

    my $handle_say_stmt =
    sub { adn('__PACKAGE__::say',$_[1]) };

    my $handle_assignment_stmt =
    sub { # keep a parse-time pad of declared variable names
            adn('__PACKAGE__::assign',@_[0,2]) };

    sub ch { # parse for a single character.
        my $p;
        $p = l('C',$_[0]);
        $N{$p} = "$_[0]";
        $p;
    }

    sub w { # look for a wrapped entity.  first parm is split into the wrappers.
        my ($d,$e) = split(//,$_[0]);
        my $p;
        $p = concatenate(ch($d),$_[1],ch($e));
        $N{$p} = "$d ".$N{$_[1]}." $e";
        $p;
    }
    
    sub keyword {
        my $ins = shift;
        my $p;
        $p = l('ID',$ins);
        $N{$p} = "$ins";
        $p;
    }
    
    sub keywords {
        my @args = @_;
        my $p;
        $p = alternate(map(keyword($_),@args));
        $N{$p} = join('|',@args);
        $p;
    }
    
    #sub k { goto &keyword }
    
    sub clist {
        my $ins = shift;
        my $p;
        $p = commalist($ins,$Comma,', ');
        $N{$p} = 'clist of['.$N{$ins}.']';
        $p;
    }
    
    sub gt0 { # hit on 1 or more of the contained. (gt0 == greater than zero)
        my $p;
        $p = concatenate($_[0],star($_[0]));
        $N{$p} = 'atleast1('.$N{$_[0]}.')';
        $p;
    }
    
    sub word {
        my $p;
        $p = concatenate(map(ch($_),split(//, $_[0])));
        $N{$p} = $_[0];
        $p;
    };
    
    my $handle_declarator = 
    sub { bless([$_[0]] => 'Tuple') };

    $sVar = concatenate(ch('$'),l('ID')) >> sub {
            "$_[0]$_[1]";
        };

    my $handle_declaration_stmt =
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
            $handle_assignment_stmt->(@_) };

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
    
    # rule perl5zone {
        # sl(); # grabs anything/everything...
    # }

    rule program {
        # treat everything as Perl 5 by default. haha!
        #o($Perl5zone)        -        # can't slurp "up until" until we're backtracking
                        # I need a function "until($p,$q)" that returns a parser that 
                        # iterates through the stream of tokens, trying $q against the
                        # remaining input at each token, essentially using $q as a stop
                        # phrase, then returns $p's and $q's matches, concatenated.
     #parser { sub {} }
        o($Usev6 - error(o($StmtList))) - $End_of_Input
        # an optional Perl 5 section # use v6; # error trapper #optional Stmt list
    };
    
    rule usev6 {
        l('USEV6')
    };
    
    rule comma {
        ch(',')
    };
    
    rule stmtList {
        ($Stmt - o($StmtTrm - ($StmtList | $BlockList))) | $BlockList
        # the first Stmt in the list  # 0 or more Terminator-Stmt
        # since Block-NewLine includes its own implied following terminator...
    };

    rule blockList {
        $Block - o(($Newline | $StmtTrm) - o($StmtList))
    };

    rule newline {
        l('NL')
    };

    rule blkDecl { 
        ($BlkPrmbl - $BlkBare)
    };
    
    rule blkBare {
        w('{}',$StmtList)
    };
    
    rule block {
        $BlkDecl | $BlkBare
    };
    
    rule blkPrmbl { # block preamble
        o($BlkModf - $BlkRetT) - $BlkType - o(l('ID')) - o(w('()',$BlkPrms)) #- o($BlkTrts)
    };

    rule blkType {
        keywords('sub','method','submethod','regex','token','rule','macro')
    };

    rule blkRetT {
        l('ID')  # just take any class/type name for now :)
      #  $Clype    # Class/Type
    };

    rule blkModf {
        keywords('multi','proto','only')
    };
    
    rule arg {
        ch('h')
    };
    
    rule blkPrms {
        commalist($Arg)
    };
    
    rule stmtTrm {
        gt0(ch(';'))
    };

    rule scpDecl {
        keywords('my','our')
    };
    
    rule stmt { # bare expression (its result sets $_ for its scope)
        (keyword('say') - $Expr
                 >> $handle_say_stmt
               | ((o($ScpDecl) > $handle_declarator) - $sVar - ch('=') - $Expr )
                 >> $handle_declaration_stmt) | $Expr
    };

    rule expr {
    (($Term - star(ch('+') - $Term >>  $handle_infix_addition
             | ch('-') - $Term >> $handle_infix_subtraction))
                  >> $handle_cascade) | $Block
    };

    rule term {
        $Factor - star(ch('*') - $Factor >> $handle_infix_multiplication
                     | ch('/') - $Factor >> $handle_infix_division)
                  >> $handle_cascade
    };

    rule factor {
        $Base - ( word('**') - $Factor >> $handle_infix_exponentiation_factor
                | $nothing >> $handle_empty_nothing )
        >> $handle_infix_exponentiation_operation
    };

    rule base {
     (l('INT') > $handle_integer_inst)
          | ($sVar > $handle_variable_lookup)
          | w('()',$Expr)
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