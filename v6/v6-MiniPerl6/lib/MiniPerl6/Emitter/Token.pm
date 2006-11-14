use v6-alpha;

# no <after .. > - so it doesn't need to match backwards
# no backtracking on quantifiers
# <%hash> must be a hash-of-token

class Rul {
    sub perl5 ( $rx ) {
        return
            '( perl5rx( substr( $str, $MATCH.to ), \'^(' ~ $rx ~ ')\' ) ' ~ 
            ' ?? ( 1 + $MATCH.to( $0.chars + $MATCH.to )) ' ~
            ' !! (0) ' ~
            ')'
    };
    
    sub constant ( $str ) {
            #my $str1;
            # { use v5; $str1 = $str; $str1 =~  s/\\(.)/$1/g; use v6; }
        
            my $len := $str.chars;
            if ( $len ) {
                '( ' ~ $str.perl ~ ' eq substr( $str, $MATCH.to, ' ~ $len ~ ') ' ~
                '  ?? (1 + $MATCH.to( ' ~ $len ~ ' + $MATCH.to ))' ~
                '  !! (0) ' ~
                ')';
            }
            else {
                return '1'
            }        
    }
}

class Rul::Quantifier {
    has $.term;
    has $.quant;
    has $.greedy;
    has $.ws1;
    has $.ws2;
    has $.ws3;
    method emit {
        # TODO
        $.term.emit;
    }
}

class Rul::Or {
    has @.or;
    method emit {
        '(' ~ (@.or.>>emit).join(' || ') ~ ')';
    }
}

class Rul::Concat {
    has @.concat;
    method emit {
        '(' ~ (@.concat.>>emit).join(' && ') ~ ')';
    }
}

class Rul::Subrule {
    has $.metasyntax;
    method emit {
        my $meth := ( 1 + index( $.metasyntax, '.' ) )
            ?? $.metasyntax 
            !! ( '$grammar.' ~ $.metasyntax );
        'do { ' ~
          'my $m2 := ' ~ $meth ~ '($str, $MATCH.to); ' ~
          ## 'my $m2 := ' ~ $meth ~ '($str, { "pos" => $MATCH.to, "KEY" => $key }); ' ~
          'if $m2 { $MATCH.to( $m2.to ); $MATCH{"' ~ $.metasyntax ~ '"} := $m2; 1 } else { 0 } ' ~
        '}'
    }
}

class Rul::Var {
    has $.sigil;
    has $.twigil;
    has $.name;
    method emit {
        # Normalize the sigil here into $
        # $x    => $x
        # @x    => $List_x
        # %x    => $Hash_x
        # &x    => $Code_x
        my $table := {
            '$' => '$',
            '@' => '$List_',
            '%' => '$Hash_',
            '&' => '$Code_',
        };
        $table{$.sigil} ~ $.name
    }
}

class Rul::Constant {
    has $.constant;
    method emit {
        my $str := $.constant; 
        Rul::constant( $str );
    }
}

class Rul::Dot {
    method emit {
        '( \'\' ne substr( $str, $MATCH.to, 1 ) ' ~
        '  ?? (1 + $MATCH.to( 1 + $MATCH.to ))' ~
        '  !! (0) ' ~
        ')';
    }
}

class Rul::SpecialChar {
    has $.char;
    method emit {
          my $char := $.char;
          if $char eq 'n' {
              return Rul::perl5( '(?:\n\r?|\r\n?)' )
          };
          if $char eq 'N' {
              return Rul::perl5( '(?!\n\r?|\r\n?).' )
          };
          # TODO
          #for ['r','n','t','e','f','w','d','s'] {
          #  if $char eq $_ {
          #      return Rul::perl5(   "\\$_"  );
          #  }
          #};
          #for ['R','N','T','E','F','W','D','S'] {
          #  if $char eq $_ {
          #      return Rul::perl5( "[^\\$_]" );
          #  }
          #};
          if $char eq '\\' {
            $char := '\\\\' 
          };
          return Rul::constant( $char );
      }
}

class Rul::Block {
    has $.closure;
    method emit {
        'do ' ~ $.closure
    }
}

# TODO
class Rul::InterpolateVar {
    has $.var;
    method emit {
        1
    };
#        my $var = $.var;
#        # if $var.sigil eq '%'    # bug - Moose? no method 'sigil'
#        {
#            my $hash := $var;
#            $hash := $hash.emit;
#           'do {
#                state @sizes := do {
#                    # Here we use .chr to avoid sorting with {$^a<=>$^b} since
#                    # sort is by default lexographical.
#                    my %sizes := '~$hash~'.keys.map:{ chr(chars($_)) => 1 };
#                    [ %sizes.keys.sort.reverse ];
#                };
#                my $match := 0;
#                my $key;
#                for @sizes {
#                    $key := ( $MATCH.to <= chars( $s ) ?? substr( $s, $MATCH.to, $_ ) !! \'\' );
#                    if ( '~$hash~'.exists( $key ) ) {
#                        $match = $grammar.'~$hash~'{$key}.( $str, { pos => ( $_ + $MATCH.to ), KEY => $key });
#                        last if $match;
#                    }
#                }
#                if ( $match ) {
#                    $MATCH.to: $match.to;
#                    $match.bool: 1;
#                }; 
#                $match;
#            }';
#        }
#    }
}

class Rul::NamedCapture {
    has $.rule;
    has $.ident;
    method emit {
        "# TODO: named capture " ~ $.ident ~ " := \n" ~ $.rule.emit ~ "\n"
    }
}

class Rul::Before {
    has $.rule;
    method emit {
        "# TODO: before \n" ~ $.rule.emit ~ "\n"
    }
}

class Rul::NegateCharClass {
    has $.chars;
    method emit {
        '1 # TODO: negate char class ' ~ $.chars ~ "\n"
    }
}

class Rul::CharClass {
    has $.chars;
    method emit {
        '1 # TODO: char class ' ~ $.chars ~ "\n"
    }
}
