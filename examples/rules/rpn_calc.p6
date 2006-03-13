#!/usr/bin/pugs


use v6;

grammar rpn_data {
   rule key       { <?ident> }
   rule value     { \N* };
   rule statement { <key>\h*=\h*<value>\n* }
   rule config    { [\n*<statement>]* }
}

my $data = "
test1 = 1 2 + 
test2 = 1 2 3 + - 
";

say "Match 1:";
my $config = $data ~~ /<config>/;
say match_describe( $config,0);

for $config<config><statement> -> $o  {
    say "$o<key> == $o<value> == {evaluate("$o<value>")}";
}


sub match_describe (Match $o, Num $indent) {
   my $desc;
   if ( @$o.elems ){
     $desc ~= "[\n" ~ join("" , map { match_describe($_, $indent + 1) } @$o ) ~ "{"\t" x $indent}],";
   } elsif (  %$o.keys.elems ) {
      $desc ~= "{"\t" x $indent}\{\n";
      
      for (keys %$o) {
        $desc ~= "{"\t" x ($indent+1)}'$_' := { match_describe($o.{$_},$indent + 1)}\n";
      }  
      $desc ~= "{"\t" x $indent}\},\n";
   } else  {
      $desc ~= "'$o'";
   }
   return "$desc";
}

# RPN calc stolen from examples/rpn/p6/RPN.pm

sub evaluate (Str $expr) returns Int {
    my @stack;
    for ($expr.split()) -> $tok {
        if $tok ~~ /-? \d+/ {
            @stack.push($tok);
            next;
        }
        my $x = @stack.pop() err die "Stack underflow\n";
        my $y = @stack.pop() err die "Stack underflow\n";

        # given/when is a sexy new P6 construct that can avoid
        # long if/elsif/else chains
        given $tok {
            when '+' { @stack.push($y + $x) }
            when '-' { @stack.push($y - $x) }
            when '*' { @stack.push($y * $x) }
            when '/' { @stack.push(int($y / $x)) }
            default  { die "Invalid token:\"$tok\"\n" }
        }
    }

    @stack.elems == 1 or die "Invalid stack:[@stack[]]\n";
    return @stack[0];
}
