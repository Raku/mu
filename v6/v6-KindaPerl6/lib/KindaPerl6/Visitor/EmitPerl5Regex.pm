use v6-alpha;

# This visitor is a plugin for the perl5 emitter.
# It emits p6-regex as p5-regex.

# XXX - It only generates "ratchet" regex code

# See: temp/backtracking-recursive-subrule.pl
    
class KindaPerl6::Visitor::EmitPerl5Regex {

    # 'EmitPerl5' predefines all nodes, except 'Token'
    use KindaPerl6::Visitor::EmitPerl5;

    method visit ( $node ) {
        $node.emit_perl5;
    };

}

class Token {    
    method emit_perl5 {
        my $regex_source := ($.regex).emit_perl5;
        my $source := 
              'use vars qw($_rule_' ~ $.name ~ '); ' 
            ~ '$_rule_' ~ $.name ~ ' = qr/' 
            
            ~ '(?{ '
            ~   'local $GLOBAL::_M = [ $GLOBAL::_M, \'create\', pos(), \\$_ ]; '
            ~   '$GLOBAL::_M2 = $GLOBAL::_M; '
            ~ '})'

            ~ $regex_source 
            
            ~ '(?{ '
            ~   'local $GLOBAL::_M = [ $GLOBAL::_M, \'to\', pos() ]; '
            ~   '$GLOBAL::_M2 = $GLOBAL::_M; '
            ~ '})'

            ~ '/x; ' 

            # create the method, using the OO metamodel
            # OUTER::<$_> := string to match
            # OUTER::<$/> := match result
            
            ~ '$::X->{_dispatch}( $::X, "HOW", )->{_dispatch}( '
            ~   '$::X->{_dispatch}( $::X, "HOW", ), '
                ~ '"add_method", '
                ~ '$::Str->{_dispatch}( $::Str, "new", "' ~ $.name ~ '" ), '

                ~ '$::Method->{_dispatch}( $::Method, "new", '
                    ~ 'sub { '
                    ~    'local $GLOBAL::_Class = shift; '
                    ~    'undef $GLOBAL::_M2; '
                    ~    '( ref($_) ? $_->{_dispatch}( $_, "str" )->{_value} : $_ ) =~ '
                    ~      '/$_rule_' ~ $.name ~ '/; '
                    ~    'Match::from_global_data( $GLOBAL::_M2 ); '
                    
                    # XXX TODO - modify outer $/
                    ~    '$MATCH = '
                    
                    ~    '$GLOBAL::MATCH = shift @Match::Matches; '
                    ~ '} '
                ~ '), '

            ~ '); '
                        
        return $source;
    }
}

class Rule::Quantifier {
    method emit_perl5 {
        # TODO
        $.term.emit_perl5 ~ $.quant ~ $.greedy;
    }
}

class Rule::Or {
    method emit_perl5 {
          '(?:' ~ (@.or.>>emit_perl5).join('|') ~ ')';
    }
}

class Rule::Concat {
    method emit_perl5 {
        '(?:' ~ (@.concat.>>emit_perl5).join('') ~ ')';
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
        if $str eq '\\' {
            return '\\\\';
        };
        if $str eq '\'' {
            return '\\\'';
        };
        $str;
    }
}

class Rule::Dot {
    method emit_perl5 {
        '(?:\n\r?|\r\n?|\X)';
    }
}

class Rule::SpecialChar {
    method emit_perl5 {
        my $char := $.char;
        #say 'CHAR ',$char;
        if $char eq 'n' {
            return '(?:\n\r?|\r\n?)';
        };
        if $char eq 'N' {
            return '(?:(?!\n\r?|\r\n?)\X)';
        };
        if $char eq '\\' {
            return '\\\\';
        };
        if $char eq '\'' {
            return '\\\'';
        };
        return '\\' ~ $char;  # ???
    }
}

class Rule::Block {
    method emit_perl5 {
        # XXX - change to new Match style
        '(?{ ' ~ 
             'my $ret = ( sub {' ~
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
        ' })'
    }
}

# TODO
class Rule::InterpolateVar {
    method emit_perl5 {
        say '# TODO: interpolate var ' ~ $.var.emit_perl5 ~ '';
        die();
    };
}

class Rule::Before {
    method emit_perl5 {
        '(?=' ~ $.rule.emit_perl5 ~ ')'
    }
}

class Rule::NotBefore {
    method emit_perl5 {
        '(?!' ~ $.rule.emit_perl5 ~ ')'
    }
}

class Rule::NegateCharClass {
    # unused
    method emit_perl5 {
        say "TODO NegateCharClass";
        die();
    }
}

class Rule::CharClass {
    # unused
    method emit_perl5 {
        say "TODO CharClass";
        die();
    }
}

class Rule::SubruleNoCapture {
    method emit_perl5 {
        # TODO
        #my $meth := ( 1 + index( $.metasyntax, '.' ) )
        #    ?? $.metasyntax ~ ' ... TODO '
        #    !! ( '\'$\'.$GLOBAL::_Class.\'::_rule_' ~ $.metasyntax ~ '\'' );
        
        # XXX - Temporary hack
        my $meth := '\'$_rule_' ~ $.metasyntax ~ '\'';

        # XXX - param passing
        
          '(?:'
            ~ '(??{ eval ' ~ $meth ~ ' })'
            ~ '(?{ '
            ~   'local $GLOBAL::_M = [ $GLOBAL::_M, "discard_capture" ]; '
            ~ '})'
        ~ ')'
    }
}

class Rule::Subrule {
    method emit_perl5 {
        # TODO
        #my $meth := ( 1 + index( $.metasyntax, '.' ) )
        #    ?? $.metasyntax ~ ' ... TODO '
        #    !! ( '\'$\'.$GLOBAL::_Class.\'::_rule_' ~ $.metasyntax ~ '\'' );
        
        # XXX - Temporary hack
        my $meth := '\'$_rule_' ~ $.metasyntax ~ '\'';

        # XXX - named capture; param passing
        
        # TODO - capture-to-array
        
          '(?:'
            ~ '(??{ eval ' ~ $meth ~ ' })'
            ~ '(?{ '
            ~   'local $GLOBAL::_M = [ $GLOBAL::_M, "named_capture", "' ~ $.ident ~ '" ]; '
            ~ '})'
        ~ ')'
    }
}

class Rule::NamedCapture {
    method emit_perl5 {

        if $.capture_to_array {
              '(?:'
                ~ '(?{ '
                ~   'local $GLOBAL::_M = [ $GLOBAL::_M, \'create\', pos(), \\$_ ]; '
                ~ '})'
                ~ $.rule.emit_perl5 
                ~ '(?{ '
                ~   'local $GLOBAL::_M = [ $GLOBAL::_M, \'to\', pos() ]; '
                ~   'local $GLOBAL::_M = [ $GLOBAL::_M, "named_capture_to_array", "' ~ $.ident ~ '" ]; '
                ~ '})'
            ~ ')'
        }
        else {    
              '(?:'
                ~ '(?{ '
                ~   'local $GLOBAL::_M = [ $GLOBAL::_M, \'create\', pos(), \\$_ ]; '
                ~ '})'
                ~ $.rule.emit_perl5 
                ~ '(?{ '
                ~   'local $GLOBAL::_M = [ $GLOBAL::_M, \'to\', pos() ]; '
                ~   'local $GLOBAL::_M = [ $GLOBAL::_M, "named_capture", "' ~ $.ident ~ '" ]; '
                ~ '})'
            ~ ')'
        }
    }
}

class Rule::Capture {
    method emit_perl5 {
    
        if $.capture_to_array {
              '(?:'
                ~ '(?{ '
                ~   'local $GLOBAL::_M = [ $GLOBAL::_M, \'create\', pos(), \\$_ ]; '
                ~ '})'
                ~ $.rule.emit_perl5 
                ~ '(?{ '
                ~   'local $GLOBAL::_M = [ $GLOBAL::_M, \'to\', pos() ]; '
                ~   'local $GLOBAL::_M = [ $GLOBAL::_M, "positional_capture_to_array", ' ~ $.position ~ ' ]; '
                ~ '})'
            ~ ')'
        }
        else {    
              '(?:'
                ~ '(?{ '
                ~   'local $GLOBAL::_M = [ $GLOBAL::_M, \'create\', pos(), \\$_ ]; '
                ~ '})'
                ~ $.rule.emit_perl5 
                ~ '(?{ '
                ~   'local $GLOBAL::_M = [ $GLOBAL::_M, \'to\', pos() ]; '
                ~   'local $GLOBAL::_M = [ $GLOBAL::_M, "positional_capture", ' ~ $.position ~ ' ]; '
                ~ '})'
            ~ ')'
        }
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
