use v6-alpha;

class KindaPerl6::Visitor::Token {

    # This visitor is a Regex-AST to Perl6-AST converter
    # It only generates "ratchet" regex code
    
    method visit ( $node, $node_name ) {

        if ( $node_name eq 'Token' ) {
            
            # say 'Processing Token';
            
            my $perl6_source := ($node.regex).emit_token;
            
            # Emitted Perl 6 "method" code

            my $source := 'method ' ~ $node.name ~ ' ( $grammar: $str, $pos ) { ' 
                ~ 'my $MATCH; $MATCH := ::KindaPerl6::Perl5::Match( \'str\' => $str, \'from\' => $pos, \'to\' => $pos, \'bool\' => 1 ); ' 
                ~ '$MATCH.bool( ' ~ $perl6_source ~ '); ' 
                ~ 'return $MATCH }';
            # say 'Intermediate code: ', $source;

            # Compile the new Perl 6 code

            my $ast := KindaPerl6::Grammar.term( $source );
            
            #    my $visitor_dump_ast := KindaPerl6::Visitor::Perl.new();
            #    say 'Intermediate ast: ', ($$ast).emit( $visitor_dump_ast );

            # Return Perl 6 AST

            return $$ast;
        }
    }

}

# no <after .. > - so it doesn't need to match backwards
# no backtracking on quantifiers
# <%hash> must be a hash-of-token

class Rul {
#    sub perl5 ( $rx ) {
#        return
#            '( perl5rx( substr( $str, $MATCH.to ), \'^(' ~ $rx ~ ')\' ) ' ~ 
#            ' ?? ( 1 + $MATCH.to( $0.chars + $MATCH.to )) ' ~
#            ' !! (0) ' ~
#            ')'
#    };
    
    sub constant ( $str ) {
            #my $str1;
            # { use v5; $str1 = $str; $str1 =~  s/\\(.)/$1/g; use v6; }
        
            my $len := $str.chars;
            if $str eq '\\' {
                $str := '\\\\';
            };
            if $str eq '\'' {
                $str := '\\\'';
            };
            if ( $len ) {
                '( ( \'' ~ $str ~ '\' eq substr( $str, $MATCH.to, ' ~ $len ~ ')) ' ~
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
    method emit_token {
        # TODO
        $.term.emit_token;
    }
}

class Rul::Or {
    method emit_token {
        'do { ' ~
            'my $pos1 := $MATCH.to(); do{ ' ~ 
            (@.or.>>emit_token).join('} || do { $MATCH.to( $pos1 ); ') ~
        '} }';
    }
}

class Rul::Concat {
    method emit_token {
        '(' ~ (@.concat.>>emit_token).join(' && ') ~ ')';
    }
}

class Rul::Subrule {
    method emit_token {
        my $meth := ( 1 + index( $.metasyntax, '.' ) )
            ?? $.metasyntax 
            !! ( '$grammar.' ~ $.metasyntax );
        'do { ' ~
          'my $m2 := ' ~ $meth ~ '($str, $MATCH.to); ' ~
          ## 'my $m2 := ' ~ $meth ~ '($str, { 'pos' => $MATCH.to, 'KEY' => $key }); ' ~
          'if $m2 { $MATCH.to( $m2.to ); $MATCH{\'' ~ $.metasyntax ~ '\'} := $m2; 1 } else { 0 } ' ~
        '}'
    }
}

class Rul::SubruleNoCapture {
    method emit_token {
        my $meth := ( 1 + index( $.metasyntax, '.' ) )
            ?? $.metasyntax 
            !! ( '$grammar.' ~ $.metasyntax );
        'do { ' ~
          'my $m2 := ' ~ $meth ~ '($str, $MATCH.to); ' ~
          'if $m2 { $MATCH.to( $m2.to ); 1 } else { 0 } ' ~
        '}'
    }
}

class Rul::Var {
    method emit_token {
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
    method emit_token {
        my $str := $.constant; 
        Rul::constant( $str );
    }
}

class Rul::Dot {
    method emit_token {
        '( (\'\' ne substr( $str, $MATCH.to, 1 )) ' ~
        '  ?? (1 + $MATCH.to( 1 + $MATCH.to ))' ~
        '  !! (0) ' ~
        ')';
    }
}

class Rul::SpecialChar {
    method emit_token {
        my $char := $.char;
        #say 'CHAR ',$char;
        if $char eq 'n' {
            my $rul := ::Rul::SubruleNoCapture( 'metasyntax' => 'newline' );
            $rul := $rul.emit_token;
            #say 'NEWLINE ', $rul;
            return $rul;
            # Rul::perl5( '(?:\n\r?|\r\n?)' )
        };
        if $char eq 'N' {
            my $rul := ::Rul::SubruleNoCapture( 'metasyntax' => 'not_newline' );
            $rul := $rul.emit_token;
            return $rul;
            # Rul::perl5( '(?!\n\r?|\r\n?).' )
        };
        if $char eq 'd' {
            my $rul := ::Rul::SubruleNoCapture( 'metasyntax' => 'digit' );
            $rul := $rul.emit_token;
            return $rul;
        };
        if $char eq 's' {
            my $rul := ::Rul::SubruleNoCapture( 'metasyntax' => 'space' );
            $rul := $rul.emit_token;
            return $rul;
        };
        # TODO
        #for ['r','n','t','e','f','w','d','s'] {
        #  if $char eq $_ {
        #      return Rul::perl5(   '\\$_'  );
        #  }
        #};
        #for ['R','N','T','E','F','W','D','S'] {
        #  if $char eq $_ {
        #      return Rul::perl5( '[^\\$_]' );
        #  }
        #};
        #if $char eq '\\' {
        #  $char := '\\\\' 
        #};
        return Rul::constant( $char );
    }
}

class Rul::Block {
    method emit_token {
        #return 'do ' ~ $.closure;
        'do { ' ~ 
             'my $ret := ( sub {' ~
                'do {' ~ 
                   $.closure ~
                '}; ' ~
                '\'974^213\' } ).();' ~
             'if $ret ne \'974^213\' {' ~
                '$MATCH.capture( $ret ); ' ~
                # '$MATCH.bool( 1 ); ' ~
                'return $MATCH;' ~
             '};' ~
             '1' ~
        '}'
    }
}

# TODO
class Rul::InterpolateVar {
    method emit_token {
        say '# TODO: interpolate var ' ~ $.var.emit_token ~ '';
        die();
    };
#        my $var = $.var;
#        # if $var.sigil eq '%'    # bug - Moose? no method 'sigil'
#        {
#            my $hash := $var;
#            $hash := $hash.emit_token;
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
    method emit_token {
        say '# TODO: named capture ' ~ $.ident ~ ' := ' ~ $.rule.emit_token ~ '';
        die();
    }
}

class Rul::Before {
    method emit_token {
        'do { ' ~
            'my $tmp := $MATCH; ' ~
            '$MATCH := ::KindaPerl6::Perl5::Match( \'str\' => $str, \'from\' => $tmp.to, \'to\' => $tmp.to, \'bool\' => 1  ); ' ~
            '$MATCH.bool( ' ~
                $.rule.emit_token ~
            '); ' ~
            '$tmp.bool( ?$MATCH ); ' ~
            '$MATCH := $tmp; ' ~
            '?$MATCH; ' ~
        '}'
    }
}

class Rul::NotBefore {
    method emit_token {
        'do { ' ~
            'my $tmp := $MATCH; ' ~
            '$MATCH := ::KindaPerl6::Perl5::Match( \'str\' => $str, \'from\' => $tmp.to, \'to\' => $tmp.to, \'bool\' => 1  ); ' ~
            '$MATCH.bool( ' ~
                $.rule.emit_token ~
            '); ' ~
            '$tmp.bool( !$MATCH ); ' ~
            '$MATCH := $tmp; ' ~
            '?$MATCH; ' ~
        '}'
    }
}

class Rul::NegateCharClass {
    # unused
    method emit_token {
        say "TODO NegateCharClass";
        die();
    #    'do { ' ~
    #      'my $m2 := $grammar.negated_char_class($str, $MATCH.to, \'' ~ $.chars ~ '\'); ' ~
    #      'if $m2 { 1 + $MATCH.to( $m2.to ) } else { 0 } ' ~
    #    '}'
    }
}

class Rul::CharClass {
    # unused
    method emit_token {
        say "TODO CharClass";
        die();
    #    'do { ' ~
    #      'my $m2 := $grammar.char_class($str, $MATCH.to, \'' ~ $.chars ~ '\'); ' ~
    #      'if $m2 { 1 + $MATCH.to( $m2.to ) } else { 0 } ' ~
    #    '}'
    }
}

class Rul::Capture {
    # unused
    method emit_token {
        say "TODO RulCapture";
        die();
    }
}

=begin

=head1 NAME 

KindaPerl6::Visitor::Token - AST processor for Regex emulation

=head1 SYNOPSIS

    my $visitor_token := KindaPerl6::Visitor::Token.new();
    $ast = $ast.emit( $visitor_token );

=head1 DESCRIPTION

This module transforms regex AST into plain-perl AST.

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2007 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end
