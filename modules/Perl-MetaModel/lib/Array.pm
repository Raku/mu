
use v6;

role Array--Perl6;

does Map;
does List;

# push @foo, self



&Array::pop<Array> := &Array::splice<Array>.assuming(:offset(-1) :length(1));

multi sub Array::pop () returns Scalar {
    pop @CALLER::_;
}

 multi sub Array::push (@array is rw : *@values) returns Int {
   Array::splice(@array, @array.elems, 0, @values);
   @array.elems;
 }
 &Array::shift<Array> := &Array::splice<Array>.assuming(:offset(0) :length(1));

 multi sub Array::shift () returns Scalar {
   Array::shift @CALLER::_;
 }
 multi sub Array::splice (       @array is rw 
                                 : Int ?$offset = 0,
                                   Int ?$length,
                                       *@values
                                ) returns List is rw
 multi sub Array::unshift (@array is rw : *@values) returns Int {
   Array::splice(@array, 0, 0, @values);
   @array.elems;
 }

 multi sub Array::grep (@values :      Code *&test  ) returns Lazy
 multi sub Array::grep (@values,   MatchTest $test  ) returns Lazy
 multi sub  Perl6::List::grep (MatchTest $test :   *@values) returns Lazy {
   gather {
     for @values -> $x {
       take $x if $x ~~ $test;
     }
   }
 }
 multi sub Array::join (@values,   Str $delimiter) returns Str
 multi sub  Perl6::List::join (Str $delimiter : *@values) returns Str {
   my $str = ~@values[0];
   for 1..@values.end {
     $str ~= $delimiter ~ @values[$_];
   }
   $str;
 }
 &join<> := &join<Str>.assuming:delimiter(' ');

 multi sub Array::map (@values,   Code $expression) returns Lazy 
 multi sub  Perl6::List::map (Code $expression : *@values) returns Lazy {
   gather {
     while @values {
       take $expression
          .( splice(@values, 0, $expression.arity) );
     }
   }
 }
 multi sub Array::reduce (@values : Code *&expression) returns Scalar
 multi sub  Perl6::List::reduce (Code $expression : *@values) returns Scalar {
   my $res;
   for @values -> $cur {
        FIRST {$res = $cur; next;}
     $res = &$expression($res, $cur);
   }
   $res;
 }


 multi sub Perl6::Hash::reverse (%hash) returns Hash is default {
   my %result;
   for %hash.kv -> $k, $v {
     %result{$v} = $k;
   }
   %result;
 }

 multi sub Array::reverse (   @values) returns Lazy|Str {
 multi sub  Perl6::List::reverse (: *@values) returns Lazy|Str {
   given want {
     when List {
       gather {
         1 while take pop @values;
       }
     }
     when Scalar {
       reverse @values ==> join;
     }
   }
 }
 type KeyExtractor  ::= Code(Any) returns Any;
 type Comparator    ::= Code(Any, Any) returns Int;
 type SortCriterion ::= KeyExtractor
                      | Comparator
                      | Pair(KeyExtractor, Comparator);

 multi sub Array::sort(                 @values is rw,
                                              *&by
                              :           Bit +$inplace
                             ) returns Array

 multi sub Array::sort(                 @values is rw,
                                SortCriterion  @by
                              :           Bit +$inplace
                             ) returns Array

 multi sub Array::sort(                 @values is rw
                              : SortCriterion +$by = &infix:<cmp>,
                                          Bit +$inplace
                             ) returns Array

 multi sub  Perl6::List::sort(  SortCriterion  @by
                              :               *@values
                             ) returns List

 multi sub  Perl6::List::sort(: SortCriterion  $by = &infix:<cmp>,
                                              *@values
                             ) returns List
 multi sub Perl6::Lists::zip (: Array *@lists, Bit +$shortest) returns Lazy {
   gather {
     while $shortest ?? all (@lists) !! any(@lists) {
       for @lists -> @list {
         take shift @list;
       }
     }
   }
 }

