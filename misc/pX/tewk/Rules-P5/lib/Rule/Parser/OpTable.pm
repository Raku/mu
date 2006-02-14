#~/usr/bin/perl

package Rule::Parser::OpTable;
use base qw/Class::Accessor/;

use strict;
use warnings;
use constant PGE_OPTABLE_EMPTY => 0;
use constant PGE_OPTABLE_TERM => 1;
use constant PGE_OPTABLE_POSTFIX => 2;
use constant PGE_OPTABLE_CLOSE => 3;
use constant PGE_OPTABLE_PREFIX => 4;
use constant PGE_OPTABLE_INFIX => 5;
use constant PGE_OPTABLE_TERNARY => 6;
use constant PGE_OPTABLE_POSTCIRCUMFIX => 7;
use constant PGE_OPTABLE_CIRCUMFIX => 8;

BEGIN {
  __PACKAGE__->mk_accessors(qw/
    tokenTable 
    termTable 
    operatorTable 
    whitespaceTermTable 
    whitespaceOperatorTable/)
}

sub new {
  my $class = shift;
  my $object = bless {}, $class;
  $object->tokenTable({}) ;
  $object->termTable({});
  $object->operatorTable({});
  $object->whitespaceTermTable({});
  $object->whitespaceOperatorTable({}); 
  $object;
}

sub printHash
{
  my ($hash, $desc) = @_;
  while ( my ($k,$v) = each %$hash ) {
    print "$desc: $k => $v\n";
  }
  if ( not scalar keys %$hash )
  {
    print "$desc: empty\n";
  }
}

sub addToken {
  my ($self, $name, $relativePrecedence, $options, $match) = @_;
  #print "name: $name - rp: $relativePrecedence - options: $options - match: $match\n" ;
  my $equivalence = "=";

  $match = "Rule::Parser:Match" if ! defined $match; 
  $options = "left" if not $options;
  if ( $relativePrecedence and $relativePrecedence ne "" )
  {
    if ( substr $relativePrecedence, 0, 1 =~ m/[\=\<\>]/ )
    {
      my $tokenTableKey = substr $relativePrecedence, -1, 0;
      $equivalence = $self->tokenTable()->{$tokenTableKey}{equiv};
    }
    else
    {
      $equivalence = $self->tokenTable()->{$relativePrecedence}{equiv};
    }
  }

  my $noWhitespace = 0;
  $noWhitespace = 1 if ((index $options, "nows") >= 0);

  #print "name: $name - eq: $equivalence - rp: $relativePrecedence - options: $options - match: $match\n" ;
  my %token = ( name => $name, options => $options, equiv => $equivalence, match => $match, arity => 1 );
  my @nameSplit = split /:/, $name;
  #print "@nameSplit ", length @nameSplit, " ", $#nameSplit, "\n";
  my @spacePresent = split / /, $nameSplit[$#nameSplit];
  if ( @spacePresent > 1 )
  {
    $token{token2} = $spacePresent[1];
    my %tokenClone = %token;
    $tokenClone{syncat} = PGE_OPTABLE_CLOSE;
    $self->operatorTable()->{$spacePresent[1]} = %tokenClone;
    $self->whitespaceOperatorTable()->{$spacePresent[1]} = %tokenClone;
  }

  $token{token1} = $nameSplit[$#nameSplit];
  $self->tokenTable()->{$name} = \%token;

  my $syncat = $nameSplit[0];
  if ( $syncat eq "infix" )
  {
    $token{syncat} = PGE_OPTABLE_INFIX;
    $token{arity} = 2;
  }
  elsif ( $syncat eq "postfix" )
  {
    $token{syncat} = PGE_OPTABLE_POSTFIX;
  }
  elsif ( $syncat eq "circumfix" )
  {
    $token{syncat} = PGE_OPTABLE_CIRCUMFIX;
  }
  elsif ( $syncat eq "prefix" )
  {
    $token{syncat} = PGE_OPTABLE_PREFIX;
  }
  elsif ( $syncat eq "postcircumfix" )
  {
    $token{syncat} = PGE_OPTABLE_POSTCIRCUMFIX;
    $token{arity} = 2;
  }
  elsif ( $syncat eq "ternary" )
  {
    $token{syncat} = PGE_OPTABLE_TERNARY;
    $token{arity} = 3;
  }
  elsif ( $syncat eq "close" )
  {
    $token{syncat} = PGE_OPTABLE_CLOSE;
    $token{arity} = 0;
  }
  else
  {
    $token{syncat} = PGE_OPTABLE_TERM;
  }
}

sub parse
{
  my ($self, $matchObj) = @_;
  my @termStack = [];
  my @operatorStack = [];
  my @tokenStack = [];
  my $termEmpty = $self->termTable()->{""};
  my $operatorEmpty = $self->operatorTable()->{""};

  ($matchObj, my $target, my $matchObjFrom, my $matchObjPos) = Parser::Rule::Match::newfrom($matchObj, 0);
  my $position = $matchObjPos;
  my $lastPosition = length $target;
  my @bsrStack = [];
  #XXX
  my $operator;
  expect_term:
  goto null_term if $position >= $lastPosition;
 
  my $currentTermTable = $self->whitespaceTermTable();
  (substr $target, $position, $lastPosition) =~ m/[^\s]/;
  my $whitespacePosition = $-[0];
  $currentTermTable = $self->termTable() if ($whitespacePosition <= $position);

  expect_term_1:
  my $key = $$currentTermTable->lkey($target, $whitespacePosition);
  my $token = $$currentTermTable{$key};
  unless ($token)
  {
    push @bsrStack, "bsr_token_match_1";
    goto token_match;
    bsr_token_match_1:
    goto found_term if $operator;
    goto null_term if ($key eq "");
  }
  expect_term_empty:
  if ( $termEmpty )
  {
    $token = $termEmpty;
    $key = "";
    $whitespacePosition = $position;
    push @bsrStack, "bsr_token_match_2";
    goto token_match;
    bsr_token_match_2:
    goto found_term if $operator;
  }
  null_term:
  if (@tokenStack)
  {
    my $top = $tokenStack[-1];
    goto missing_term if ((index $$top["opts"], "nullterm") < 1);
    ($operator, my $stringTemp, my $tempPos, my $tempPos2) = newfrom( $matchObj, $whitespacePosition, "PGE::Match");
    my $temp2 = $whitespacePosition;
    push @termStack, $operator;
    goto expect_operator;
  }
  else
  {
    goto missing_term;
  }
  missing_term:
  push @termStack, newfrom( $matchObj, $whitespacePosition, "PGE::Match" );
  goto end;
  found_term:
  my $tokenCat = $$token{syncat};
  $position = $operator->to();
  goto oper_shift if ( $tokenCat == PGE_OPTABLE_PREFIX );
  goto oper_shift if ( $tokenCat == PGE_OPTABLE_CIRCUMFIX );
  push @termStack, $operator;

  expect_oper:
  goto end if ( $position >= $lastPosition );
  my $currentOperatorTable = $self->whitespaceOperatorTable();
  
  (substr $target, $position, $lastPosition) =~ m/[^\s]/;
  $whitespacePosition = $-[0];
  $currentOperatorTable = $self->operatorTable() if ( $whitespacePosition <= $position );

  expect_oper_1:
  $key = $$currentOperatorTable->lkey($target, $whitespacePosition);
  $token = $$currentOperatorTable{$key};
  if ( $token )
  {
    push @bsrStack, "bsr_token_match2_1";
    goto token_match;
    bsr_token_match2_1:
    goto found_oper if $operator;          
  }

  expect_oper_empty:
  goto end unless $operatorEmpty;
  $token = $operatorEmpty;
  $key = "";
  $whitespacePosition = $position;
  push @bsrStack, "bsr_token_match_3";
  goto token_match;
  bsr_token_match_3:
  goto end unless $operator;

  found_oper:
  $tokenCat = $$token{syncat};

  shift_reduce:
  my $topcat = PGE_OPTABLE_EMPTY;
  if ( @tokenStack <= 0 )
  {
    goto end if $tokenCat == PGE_OPTABLE_CLOSE;
    goto oper_shift;
  }

  shift_reduce_1:
  my $top = $tokenStack[-1];
  $topcat = $$top{syncat};
  goto oper_reduce if ( $topcat == PGE_OPTABLE_POSTFIX );
  goto oper_close if ( $topcat == PGE_OPTABLE_CLOSE );
  goto oper_shift if ( $topcat == PGE_OPTABLE_POSTCIRCUMFIX );
  #$temp1 = $$token{eqiv};
  #$temp2 = $$top{eqiv};
  goto oper_shift if $$token{eqiv} > $$top{eqiv};
  goto shift_reduce_2 if ( $topcat == PGE_OPTABLE_TERNARY );
  goto ternary_error if ( $topcat == PGE_OPTABLE_TERNARY );
  goto oper_shift;

  shift_reduce2:
  #if ( $temp1 >= $temp2 )
  if ( $$token{eqiv} > $$top{eqiv} )
  {
    goto oper_shift if ( (index $$top{opts}, "right" ) >=0 );
  }
  
  oper_reduce:
  push @bsrStack, "bsr_reduce_1";
  goto reduce;
  bsr_reduce_1:
  goto shift_reduce;

  oper_close:
  goto oper_reduce if $topcat < PGE_OPTABLE_TERNARY;
  goto end if $key ne $$top{token2};

  oper_shift:
  push @tokenStack, $token;
  push @operatorStack, $operator;
  $position = $operator->to();
  goto expect_term if ( $tokenCat >= PGE_OPTABLE_PREFIX );
  goto expect_oper if ( $tokenCat >= PGE_OPTABLE_POSTFIX );
  goto expect_term if ( $tokenCat >= PGE_OPTABLE_TERNARY );
  goto expect_oper;

  reduce:
  my $temp1 = pop @tokenStack;
  my $temp2 = $$temp1{syncat};
  if ( $temp2 == PGE_OPTABLE_CLOSE )
  {
    $temp1 = pop @tokenStack;
    $temp2 = pop @operatorStack;
  }
  reduce_1:
  my $arity = $$temp1{arity};
  $temp1 = pop @operatorStack;

  reduce_args:
  goto reduce_saveterm if $arity < 1;
  $temp2 = pop @termStack;
  if ( $temp2 )
  {
    $arity--;
    $$temp1[$arity] = $temp2;
    goto reduce_args;
  }
  reduce_backtrack:
  $position = $temp1->from();
  goto reduce_end if $arity > 1;
  $temp1 = $temp2;
  reduce_save_term:
  push @termStack, $temp1;
  reduce_end:
  goto (pop @bsrStack);

  token_match:
  $matchObjPos = $whitespacePosition;
  my $match = $$token{match};
  #TODO == should be isa
  goto tok_match_sub if ( $match == "Sub" );
  ($operator) = newfrom($matchObj, $whitespacePosition, $match);
  #What does this line 
  #$temp1 = length $key + $whitespacePosition;
  goto tok_match_end;

  tok_match_sub:
  $operator = &$$match($matchObj);
  tok_match_end:
  $$operator{type} = $$token{name};
  goto (pop @bsrStack);

  end:
  if ( @tokenStack >= 1 )
  {
    push @bsrStack, "bsr_reduce_2";
    goto reduce;
    bsr_reduce_2:
    goto end;
  }
  end_1:
  goto end_2 if @termStack < 1;
  $temp1 = pop @termStack;
  goto end_2 unless $temp1;
  $$matchObj{expr} = $temp1;
  $matchObjPos = $position;
  return ( $matchObj );
  end_2:
  $matchObjPos = -1;
  return ( $matchObj );

  ternary_error:
  $matchObjPos = -1;
  die "Missing ternary close at offset $whitespacePosition \n";
  return ( $matchObj );

}


sub to_string {
  my $self = shift;
}

__PACKAGE__;
