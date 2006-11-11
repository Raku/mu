use v6-alpha;

# no <after .. > - so it doesn't need to match backwards
# no backtracking on quantifiers
# <%hash> must be a hash-of-token

class Rul {
    sub perl5( $rx ) {
        return
            '( ( substr( $str, $m.to ) ~~ m:P5/^(' ~ $rx ~ ')/ ) ' ~ 
            ' ?? ( 1 + $m.to( $0.chars + $m.to )) ' ~
            ' !! (0) ' ~
            ')'
    }
    
    sub constant( $str ) {
            my $len := $str.chars;
            if ( $len ) {
                '( ' ~ $str.perl ~ ' eq substr( $str, $m.to, ' ~ $len ~ ') ' ~
                '  ?? (1 + $m.to( ' ~ $len ~ ' + $m.to ))' ~
                '  !! (0) ' ~
                ')';
            }
            else {
                return '1'
            }        
    }
}

class Rul::Or {
    has @.or;
    method emit {
        '(' ~ @.or.>>emit.join(' || ') ~ ')';
    }
}

class Rul::Concat {
    has @.concat;
    method emit {
        '(' ~ @.concat.>>emit.join(' && ') ~ ')';
    }
}

class Rul::Subrule {
    has $.metasyntax;
    method emit {
        'do { ' ~
          'my $m2 := ' ~ $.metasyntax ~ '( str => $str, grammar => $grammar, pos => $m.to, KEY => $key ); ' ~
          'if $m2 { $m.to( $m2.to ); $m{"' ~ $.metasyntax ~ '"} := $m2; 1 } else { 0 } ' ~
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
        Rul::constant( $.constant );
    }
}

class Rul::SpecialChar {
    has $.char;
    method emit {
      my $char := $.char;
      return Rul::perl5( '(?:\n\r?|\r\n?)' )
        if $char eq 'n';
      return Rul::perl5( '(?!\n\r?|\r\n?).' )
        if $char eq 'N';
      for < r n t e f w d s > {
        return Rul::perl5(   "\\$_"  ) if $char eq $_;
      }
      for < R N T E F W D S > {
        return Rul::perl5( "[^\\$_]" ) if $char eq $_;
      }
      $char := '\\\\' if $char eq '\\';
      return Rul::constant( $char );
  }
}

class Rul::InterpolateVar {
    has $.var;
    method emit {
        my $var = $.var;
        # if $var.sigil eq '%'    # bug - Moose? no method 'sigil'
        {
            my $hash := $var;
            $hash := $hash.emit;
           'do {
                state @sizes := do {
                    # Here we use .chr to avoid sorting with {$^a<=>$^b} since
                    # sort is by default lexographical.
                    my %sizes := '~$hash~'.keys.map:{ chr(chars($_)) => 1 };
                    [ %sizes.keys.sort.reverse ];
                };
                my $match := 0;
                my $key;
                for @sizes {
                    $key := ( $m.to <= chars( $s ) ?? substr( $s, $m.to, $_ ) !! \'\' );
                    if ( '~$hash~'.exists( $key ) ) {
                        $match = '~$hash~'{$key}.( str => $str, grammar => $grammar, pos => ( $_ + $m.to ), KEY => $key );
                        last if $match;
                    }
                }
                if ( $match ) {
                    $m.to: $match.to;
                    $match.bool: 1;
                }; 
                $match;
            }';
        }
    }
}

