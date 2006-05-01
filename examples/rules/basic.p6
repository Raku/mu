#!/usr/bin/pugs
use v6;

say "Loading BASIC grammar...";

#grammar Basic {
   rule var   { <ident> \$   }
 
   
   rule let   { LET <var>  }
   rule print { PRINT <var>}
   rule goto  { GOTO \d+   }
  
   rule expr  {  <let> | <print> | <goto> }
   rule line  { (<expr>;)+ }
   rule program { (<line>\n+)+ }
#}


my $basic_program = "LET X$; PRINT X$;\n";
my $parsed = $basic_program ~~ /<program>/;
say $parsed.perl;


=begin DATA
Loading BASIC grammar...
\Match.new(
  ok => bool::true, 
  from => 15, 
  to => 18, 
  str => "$;\n", 
  sub_pos => (), 
  sub_named =>
    { "program" =>
        Match.new(
          ok => bool::true, 
          from => 15, 
          to => 18, 
          str => "$;\n", 
          sub_pos =>
            ((Match.new(
                ok => bool::true, 
                from => 15, 
                to => 18, 
                str => "$;\n", 
                sub_pos => (), 
                sub_named =>
                  { "line" =>
                      Match.new(
                        ok => bool::true, 
                        from => 15, 
                        to => 17, 
                        str => "$;", 
                        sub_pos =>
                          ((Match.new(
                              ok => bool::true, 
                              from => 15, 
                              to => 17, 
                              str => "$;", 
                              sub_pos => (), 
                              sub_named =>
                                { "expr" =>
                                    Match.new(
                                      ok => bool::true, 
                                      from => 15, 
                                      to => 16, 
                                      str => "$", 
                                      sub_pos => (), 
                                      sub_named =>
                                        { "print" =>
                                            Match.new(
                                              ok => bool::true, 
                                              from => 15, 
                                              to => 16, 
                                              str => "$", 
                                              sub_pos => (), 
                                              sub_named => {}
                                            )
                                        }
                                    )
                                }
                            ),),), 
                        sub_named => {}
                      )
                  }
              ),),), 
          sub_named => {}
        )
    }
)
