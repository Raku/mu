use v6-alpha;

# This visitor is a plugin for the perl5 emitter.
# It emits p6-regex as p5-regex.

# See: temp/backtracking-recursive-subrule.pl
    
class KindaPerl6::Visitor::EmitPerl5Regex {

    # 'EmitPerl5' predefines all nodes, except 'token'
    use KindaPerl6::Visitor::EmitPerl5;

    method visit ( $node ) {
        $node.emit_perl5;
    };

}

class Token {

    # XXX - TODO

    # This visitor is a Regex-AST to Perl6-AST converter
    # It only generates "ratchet" regex code
    
    method emit_perl5 ( $node, $node_name ) {

        if ( $node_name eq 'Token' ) {
            
            # say 'Processing Token';
            
            my $perl6_source := ($node.regex).emit_perl5;
            
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

class Rule {

    # XXX - where is this class used?
    
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

class Rule::Quantifier {
    method emit_perl5 {
        # TODO
        $.term.emit_perl5;
    }
}

class Rule::Or {
    method emit_perl5 {
        'do { ' ~
            'my $pos1 := $MATCH.to(); do{ ' ~ 
            (@.or.>>emit_perl5).join('} || do { $MATCH.to( $pos1 ); ') ~
        '} }';
    }
}

class Rule::Concat {
    method emit_perl5 {
        '(' ~ (@.concat.>>emit_perl5).join(' && ') ~ ')';
    }
}

class Rule::Subrule {
    method emit_perl5 {
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

class Rule::SubruleNoCapture {
    method emit_perl5 {
        my $meth := ( 1 + index( $.metasyntax, '.' ) )
            ?? $.metasyntax 
            !! ( '$grammar.' ~ $.metasyntax );
        'do { ' ~
          'my $m2 := ' ~ $meth ~ '($str, $MATCH.to); ' ~
          'if $m2 { $MATCH.to( $m2.to ); 1 } else { 0 } ' ~
        '}'
    }
}

class Rule::Var {
    method emit_perl5 {
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
    method emit_perl5 {
        my $str := $.constant; 
        Rule::constant( $str );
    }
}

class Rule::Dot {
    method emit_perl5 {
        '( (\'\' ne substr( $str, $MATCH.to, 1 )) ' ~
        '  ?? (1 + $MATCH.to( 1 + $MATCH.to ))' ~
        '  !! (0) ' ~
        ')';
    }
}

class Rule::SpecialChar {
    method emit_perl5 {
        my $char := $.char;
        #say 'CHAR ',$char;
        if $char eq 'n' {
            my $rul := ::Rule::SubruleNoCapture( 'metasyntax' => 'newline' );
            $rul := $rul.emit_perl5;
            #say 'NEWLINE ', $rul;
            return $rul;
            # Rule::perl5( '(?:\n\r?|\r\n?)' )
        };
        if $char eq 'N' {
            my $rul := ::Rule::SubruleNoCapture( 'metasyntax' => 'not_newline' );
            $rul := $rul.emit_perl5;
            return $rul;
            # Rule::perl5( '(?!\n\r?|\r\n?).' )
        };
        if $char eq 'd' {
            my $rul := ::Rule::SubruleNoCapture( 'metasyntax' => 'digit' );
            $rul := $rul.emit_perl5;
            return $rul;
        };
        if $char eq 's' {
            my $rul := ::Rule::SubruleNoCapture( 'metasyntax' => 'space' );
            $rul := $rul.emit_perl5;
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
    method emit_perl5 {
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
class Rule::InterpolateVar {
    method emit_perl5 {
        say '# TODO: interpolate var ' ~ $.var.emit_perl5 ~ '';
        die();
    };
#        my $var = $.var;
#        # if $var.sigil eq '%'    # bug - Moose? no method 'sigil'
#        {
#            my $hash := $var;
#            $hash := $hash.emit_perl5;
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
    method emit_perl5 {
        say '# TODO: named capture ' ~ $.ident ~ ' := ' ~ $.rule.emit_perl5 ~ '';
        die();
    }
}

class Rule::Before {
    method emit_perl5 {
        'do { ' ~
            'my $tmp := $MATCH; ' ~
            '$MATCH := ::KindaPerl6::Perl5::Match( \'str\' => $str, \'from\' => $tmp.to, \'to\' => $tmp.to, \'bool\' => 1  ); ' ~
            '$MATCH.bool( ' ~
                $.rule.emit_perl5 ~
            '); ' ~
            '$tmp.bool( ?$MATCH ); ' ~
            '$MATCH := $tmp; ' ~
            '?$MATCH; ' ~
        '}'
    }
}

class Rule::NotBefore {
    method emit_perl5 {
        'do { ' ~
            'my $tmp := $MATCH; ' ~
            '$MATCH := ::KindaPerl6::Perl5::Match( \'str\' => $str, \'from\' => $tmp.to, \'to\' => $tmp.to, \'bool\' => 1  ); ' ~
            '$MATCH.bool( ' ~
                $.rule.emit_perl5 ~
            '); ' ~
            '$tmp.bool( !$MATCH ); ' ~
            '$MATCH := $tmp; ' ~
            '?$MATCH; ' ~
        '}'
    }
}

class Rule::NegateCharClass {
    # unused
    method emit_perl5 {
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
    method emit_perl5 {
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
    method emit_perl5 {
        say "TODO RuleCapture";
        die();
    }
}

=begin

=head1 NAME 

KindaPerl6::Visitor::Token - AST processor for P6-Regex to P5-Regex transformation

=head1 SYNOPSIS

    my $visitor_token := KindaPerl6::Visitor::EmitPerl5Regex.new();
    $ast = $ast.emit( $visitor_token );

=head1 DESCRIPTION

This module transforms regex AST into plain-perl5-regex code.

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
