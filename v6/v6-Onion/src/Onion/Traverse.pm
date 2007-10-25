use v6-alpha;

class KindaPerl6::Traverse {

    sub visit ( $visitor, $node, $node_name, $path ) {
        #say "visit " ~ $node ~ ' name ' ~ $node_name;
        
        if !(defined( $path )) {
            $path := [ ];
        }
        
        if $node.isa('Array') {
            my $result := [ ];
            my $subitem;
            for @($node) -> $subitem {
                push @$result, visit_subnode( $visitor, $subitem, $path ); 
            };
            return $result;
        };

        if $node.isa('Hash') {
            my $result := { };
            my $subitem;
            for keys %($node) -> $subitem {
                $result{ $subitem } := visit_subnode( $visitor, $node{$subitem}, $path ); 
            };
            return $result;
        };

        if $node.isa('Str') {
            return $node;
        };

        if $node.isa('Pad') {
            return $node;
        };

        # do not include (arrays, pads, str) in the path
        $path := [ $node, @($path) ];        
        #say "Path: ",$path.perl;

        my $result := $visitor.visit( $node, $node_name, $path );
        if ( $result ) {
            return $result;
        };
        
        my $result := { };
        my $data := $node.attribs;
        my $item;
        for keys %($data) -> $item {
            $result{$item} := visit_subnode( $visitor, $data{$item}, $path );         
        };
        return $node.new(%$result);
        
    }

    sub visit_subnode ( $visitor, $subnode, $path ) {
        if (!(defined $subnode)) {
            return;
        }
        if     $subnode.isa('Array') 
            || $subnode.isa('Hash') 
            || $subnode.isa('Str') 
            || $subnode.isa('Pad') 
        {
            return visit( $visitor, $subnode, undef, $path );
        }
        else {
            return $subnode.emit( $visitor, $path );
        }
    }

}


=begin

=head1 NAME 

KindaPerl6::Traverse - Tree traverser for KindaPerl6 AST

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2006 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end
