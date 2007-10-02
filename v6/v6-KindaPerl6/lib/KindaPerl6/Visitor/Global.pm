use v6-alpha;

class KindaPerl6::Visitor::Global {

    method visit ( $node, $node_name ) {
    
        #say "Global ",$node_name;
    
        if    ( $node_name eq 'CompUnit' )
        {
            if ($node.body) {
            ($node.body).emit( $self );
            return $node;
            }
        }
        
        if    ( $node_name eq 'Lit::Code' )
        {
            COMPILER::put_pad( $node.pad );
            my $stmt;
            for @($node.body) -> $stmt {
                $stmt.emit( $self );
            };
            COMPILER::drop_pad;
            return $node;
        }
        
        if    ( $node_name eq 'Var' )
        {
            #say "MetaClass - Class: ", $node.name ," Lexicals: ";
            #my $lexicals := (($node.body).pad).lexicals;
            #for @$lexicals -> $var { say $var; }
            #my $var := ((@$lexicals)[0]).var;
        
            #say "variable: ", $node.sigil, $node.twigil, $node.name;
            #say "pad: ", $.pad.perl;

            if ( COMPILER::current_pad ).declaration( $node ) {
                # say "ok - declaration ", $node.name;
            }
            else {
                # TODO - lookup into the GLOBAL namespace; die if undeclared there
                #warn "undeclared variable: [", $node.sigil, ':', $node.twigil, ':', $node.name, ']';
                if     ($node.name eq '/')         # $/
                    || ($node.name eq '_')         # @_ $_ %_
                    || ($node.twigil eq '.')       # attribute
                    || ( ( $node.sigil eq '&') && ( $node.name eq 'self' ) )  # ???
                {
                    # don't modify special vars (yet?)
                    #warn "special variable: ", $node.sigil, ':', $node.twigil, ':', $node.name;
                }
                else {                  
                    $node.namespace( [ 'GLOBAL' ] );
                }
            }
            return $node;                    
        };
        return;
    };

}

=begin

=head1 NAME 

KindaPerl6::Visitor::Global - Look up lexicals and add a C<GLOBAL::> lookup if needed

=head1 DESCRIPTION

This visitor looks up lexical variables like C<$/>, C<$_> and C<@_>
and adds a GLOBAL:: namespace quantifier to them.

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
