
use v6;

role List--Perl6;

# what is below is ripped straight out of S29.  I think most of these
# actually belong on "Map"

 multi sub grep (Any|Junction $test : *@values) returns List {
   gather {
     for @values -> $x {
       take $x if $x ~~ $test;
     }
   }
 }

 multi sub join (Str $delimiter : *@values) returns List {
   my $str = ~@values[0];
   for 1..@values.end {
     $str ~= $delimiter ~ @values[$_];
   }
   $str;
 }
 &join<> := &join<Str>.assuming:delimiter(' ');

 multi sub map (Code $expression : *@values) returns List {
   gather {
     while @values {
       take $expression
          .( splice(@values, 0, $expression.arity) );
     }
   }
 }

 multi sub reduce (Code $expression : *@values) returns List {
   my $res;
   for @values -> $cur {
        FIRST {$res = $cur; next;}
     $res = &$expression($res, $cur);
   }
   $res;
 }

 multi sub reverse (%hash) returns Hash is default {
   my %result;
   for %hash.kv -> $k, $v {
     %result{$v} = $k;
   }
   %result;
 }

 multi sub reverse (: *@values) returns List|Str {
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

 multi sub sort(Criterion @by : *@values) returns List
 multi sub sort(Criterion $by : *@values) returns List
 &sort<> := &sort<Criterion>.assuming(by => &infix:<cmp>);

 type KeyExtractor ::= Code(Any) returns Any;
 type Comparator   ::= Code(Any, Any) returns Int;
 type Criterion    ::= KeyExtractor | Comparator
                       | Pair(KeyExtractor, Comparator);



 multi sub zip (Array *@lists) returns List {
   gather {
     while any(@lists) {
       for @lists -> @list {
         take shift @list;
       }
     }
   }
 }

