use v6-alpha;

class KindaPerl6::Visitor::Token {

    # This visitor is a Regex-AST to Perl6-AST converter
    # It only generates "ratchet" regex code
    
    method visit ( $node, $node_name ) {

        if ( $node_name eq 'Token' ) {
            
            # say 'Processing Token';

            my $perl6_source := ($node.regex).emit_token;
            
            # Emitted Perl 6 "method" code
            my $source := 'method ' ~ $node.name ~ ' ( $str, $pos ) { ' 
                ~ 'if (%*ENV{"KP6_TOKEN_DEBUGGER"}) { say ">>> token '~ $node.name ~' at " ~ $pos ~ " of (" ~ $str ~ ")"; };'
                ~ 'if (!(defined($str))) { $str = $_; };  my $MATCH;'
                ~ '$MATCH = Match.new(); $MATCH.match_str = $str; $MATCH.from = $pos; $MATCH.to = ($pos + 0); $MATCH.bool = 1; '
                ~ '$MATCH.bool = ' ~ $perl6_source ~ '; ' 
                ~ 'if (%*ENV{"KP6_TOKEN_DEBUGGER"}) { if ($MATCH.bool) { say "<<< token '~ $node.name ~' returned true to ("~$MATCH.to~")"; } else {say "<<< token '~ $node.name ~' returned false "; } };'
                ~ 'return $MATCH }';
            #warn 'Intermediate code: ', $source;

            # Compile the new Perl 6 code

            #say $source;
            my $ast := KindaPerl6::Grammar.term( $source );
            
            #    my $visitor_dump_ast := KindaPerl6::Visitor::Perl.new();
            #    say 'Intermediate ast: ', ($$ast).emit( $visitor_dump_ast );

            # Return Perl 6 AST

            return $$ast;
        }
        0;
    }

}

# no <after .. > - so it doesn't need to match backwards
# no backtracking on quantifiers
# <%hash> must be a hash-of-token

class Rule {
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
            if $str eq Main::backslash() {
                $str := Main::backslash() ~ Main::backslash();
            };
            if $str eq Main::singlequote() {
                $str := Main::backslash() ~ Main::singlequote();
            };
            if ( $len ) {
                'do {if (length($str) <  ' ~ $len ~ ') {(0)} else { if (' ~
                Main::singlequote() ~ $str ~ Main::singlequote() ~ ' eq substr($str, $MATCH.to, ' ~ $len ~ ')) {' ~
                '$MATCH.to = (' ~ $len ~ ' + $MATCH.to);  1;} else {(0)}}}';
            }
            else {
                return '1'
            }        
    }
}

class Rule::Quantifier {
    method emit_token {
        # TODO
        $.term.emit_token;
    }
}

class Rule::Or {
    method emit_token {
        'do { ' ~
            'my $pos1 = ($MATCH.to + 0); do{ ' ~ 
            (@.or.>>emit_token).join('} || do { $MATCH.to = ($pos1 + 0); ') ~
        '} }';
    }
}

class P5Token {
    method emit_token {
        'do { my $m2 = match_p5rx("' ~ $.regex ~ '",$str,($pos+0)); if ($m2) { $MATCH.to = $m2.to + 0; 1 } else { 0 } }';
    }
}


class Rule::Concat {
    method emit_token {
        '(' ~ (@.concat.>>emit_token).join(' && ') ~ ')';
    }
}

class Rule::Subrule {
    method emit_token {

        if (substr( $.metasyntax, 0, 1) eq Main::singlequote()) {
            return Rule::constant(substr(substr($.metasyntax, 1),0,$.metasyntax.chars - 2));
        };

        my $meth := ( 1 + index( $.metasyntax, '.' ) )
          ?? $.metasyntax 
            !! ( 'self.' ~ $.metasyntax );
        return 'do { ' ~
          'my $m2 = ' ~ $meth ~ '($str, $MATCH.to); ' ~
          ## 'my $m2 := ' ~ $meth ~ '($str, { 'pos' => $MATCH.to, 'KEY' => $key }); ' ~
          'if $m2 { $MATCH.to = ($m2.to + 0); $MATCH{\'' ~ $.metasyntax ~ '\'} = $m2; 1 } else { 0 } ' ~
          '}';
    };
}

class Rule::SubruleNoCapture {
    method emit_token {
        my $meth := ( 1 + index( $.metasyntax, '.' ) )
            ?? $.metasyntax 
            !! ( 'self.' ~ $.metasyntax );
        'do { ' ~
          'my $m2 = ' ~ $meth ~ '($str, $MATCH.to); ' ~
          'if $m2 { $MATCH.to = ($m2.to + 0); 1 } else { 0 } ' ~
        '}'
    }
}

class Rule::Var {
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

class Rule::Constant {
    method emit_token {
        my $str := $.constant; 
        Rule::constant( $str );
    }
}

class Rule::Dot {
    method emit_token {
        'do { if (\'\' ne substr( $str, $MATCH.to, 1 )) {' ~
        '   ($MATCH.to = (1 + $MATCH.to )); 1 } else {' ~
        '   0 } ' ~
        '}';
    }
}

class Rule::SpecialChar {
    method emit_token {
        my $char := $.char;
        #say 'CHAR ',$char;
        if $char eq 'n' {
            my $rul := ::Rule::SubruleNoCapture( 'metasyntax' => 'newline' );
            $rul := $rul.emit_token;
            #say 'NEWLINE ', $rul;
            return $rul;
            # Rule::perl5( '(?:\n\r?|\r\n?)' )
        };
        if $char eq 'N' {
            my $rul := ::Rule::SubruleNoCapture( 'metasyntax' => 'not_newline' );
            $rul := $rul.emit_token;
            return $rul;
            # Rule::perl5( '(?!\n\r?|\r\n?).' )
        };
        if $char eq 'd' {
            my $rul := ::Rule::SubruleNoCapture( 'metasyntax' => 'digit' );
            $rul := $rul.emit_token;
            return $rul;
        };
        if $char eq 's' {
            my $rul := ::Rule::SubruleNoCapture( 'metasyntax' => 'space' );
            $rul := $rul.emit_token;
            return $rul;
        };
        # TODO
        #for ['r','n','t','e','f','w','d','s'] {
        #  if $char eq $_ {
        #      return Rule::perl5(   '\\$_'  );
        #  }
        #};
        #for ['R','N','T','E','F','W','D','S'] {
        #  if $char eq $_ {
        #      return Rule::perl5( '[^\\$_]' );
        #  }
        #};
        #if $char eq '\\' {
        #  $char := '\\\\' 
        #};
        return Rule::constant( $char );
    }
}

class Rule::Block {
    method emit_token {
        #XXX - avoid code -> ast -> code 
        #warn $.closure.emit_perl6;
        return 'do { ' ~ 
             'my $ret = self.'~$.closure ~ '($MATCH);' ~
             'if $ret ne "sTrNgE V4l" {' ~
                'if (%*ENV{"KP6_TOKEN_DEBUGGER"}) { say "<<< some closure returing... " }; ' ~
                '$MATCH.result = $ret; ' ~
                '$MATCH.bool = 1; ' ~
                'return $MATCH;' ~
             '};' ~
             '1' ~
        '}'
    }
}

# TODO
class Rule::InterpolateVar {
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

class Rule::NamedCapture {
    method emit_token {
        say '# TODO: named capture ' ~ $.ident ~ ' := ' ~ $.rule.emit_token ~ '';
        die();
    }
}

class Rule::Before {
    method emit_token {
        if $.assertion_modifier eq '!' {
            return
                'do { ' ~
                    'my $MATCH; ' ~
                    '$MATCH = Match.new(); $MATCH.match_str = $str; $MATCH.from = $pos; $MATCH.to = ($pos + 0); $MATCH.bool = 1; ' ~
                    '$MATCH.bool = !(' ~
                        $.rule.emit_token ~
                    '); $MATCH.to = ($MATCH.from + 0); ' ~
                    '$MATCH.bool; ' ~
                '}'
        }
        else {
            return
                'do { ' ~
                    'my $MATCH; ' ~
                    '$MATCH = Match.new(); $MATCH.match_str = $str; $MATCH.from = $pos; $MATCH.to = ($pos + 0); $MATCH.bool = 1; ' ~
                    '$MATCH.bool =  ' ~
                        $.rule.emit_token ~
                    '; $MATCH.to = ($MATCH.from + 0); ' ~
                    '$MATCH.bool; ' ~
                '}'
        }
    }
}

class Rule::NegateCharClass {
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

class Rule::CharClass {
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

class Rule::Capture {
    # unused
    method emit_token {
        say "TODO RuleCapture";
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
