#!/usr/bin/pugs
use v6;

say "Loading BASIC grammar...";

#grammar Basic {
   rule var       { <ident> \$ }
   rule string    { <-["]>+ }
   rule expr      { [<var> | "<string>" }
   rule expr_list { [<expr> \s* , \s* ]* <expr> }
   
   rule f_let   { LET   \s+ <var> \s* = \s* <expr>}
   rule f_print  { PRINT \s+ <expr> }
   rule f_goto   { GOTO  \s+ (\d+) }
  
   rule command {  
           <f_let>   | 
           <f_print> |
           <f_goto> 
        }

   rule line  { [\s*<command>;]+ }
   rule program { [<line>\n+]+ }
#}


sub expr_to_string (Match $expr) {
  return %$expr.<string> if defined %$expr.<string>;
}

do {
  my $basic_program;
  my $i = 0;
  my $line = "123";
  do {
   print "{$i++}: ";
   $line = =$*IN;
#  exit if $line eq "exit";
   $basic_program ~= $line ~ "\n";
  } until(! $line.chars );
  my $parsed = $basic_program ~~ /<program>/;
  $parsed.perl.say;
  execute($parsed) if $parsed;
};

sub execute (Match $basic) {
  say "Running...";
  my $i = 0;
  for @{$basic<program><line>} -> $line {
#   say "{$i++}: $line";
   for @{$line<command>} -> $cmd {
     for %$cmd.keys {
       when 'f_print' { print expr_to_string(%$cmd<f_print><expr>) }
      }
   }
  }
  say "\nDone.";
}
