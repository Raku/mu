use v6;

say "Loading BASIC grammar...";

#Grammar Basic {
   rule var       { <ident> \$ }
   rule string    { <-[\"]>+ }
   rule expr      { <var> | \"<string>\" }
   rule expr_list { [ <expr> ',' ]* <expr> }
   
   rule f_let    { LET   <var> '=' <expr>}
   rule f_print  { PRINT <expr> }
   rule f_goto   { GOTO  (\d+) }
  
   rule command {
           <f_let>   | 
           <f_print> |
           <f_goto> 
        }

   rule line  { [\s* <command> ';']+ }
   token program { [<line> \n+]+ }
#}


sub expr_to_string (Match $expr) {
  return $expr<string> if $expr.exists('string');
}

do {
  my $basic_program;
  my $i = 0;
  my $line;
  repeat while $line.chars {
   print "{$i++}: ";
   $line = =$*IN;
   last if $line eq "exit";
   $basic_program ~= $line ~ "\n";
  }  
   $basic_program.say;
   my $parsed = $basic_program ~~ /<program>/;
#   $parsed.perl.say;
   execute($parsed);
};

sub execute (Match $basic) {
  say "Running...";
  my $i = 0;
  for @($basic<program><line>) -> $line {
   say "{$i++}: $line";
   for @($line<command>) -> $cmd {
     for $cmd.keys {
       when 'f_print' { print expr_to_string($cmd<f_print><expr>) }
      }
   }
  }
  say "\nDone.";
}
