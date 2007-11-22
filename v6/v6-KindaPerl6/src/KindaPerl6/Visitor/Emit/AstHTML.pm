
use v6-alpha;

class KindaPerl6::Visitor::Emit::AstHTML {

    # This visitor builds a html version of the ".perl" representation of the AST

    method visit ( $node, $node_name ) {
        my $result := '';
        #my $item;
        #my $subitem;

        $result := $result ~ '<span class="' ~ Main::mangle_ident( $node_name ) ~ '">';

        $result := $result ~ "::" ~ $node_name ~ "( ";
        my $data := $node.attribs;
        for keys %($data) -> $item {
            $result := $result ~ " " ~ $item ~ " => ";
            if ($data{$item}).isa('Array') {
                $result := $result ~ "[ ";
                for @($data{$item}) -> $subitem {
                    if $subitem.isa('Array') {
                        $result := $result ~ ' [ ... ], ';
                    }
                    else {
                        $result := $result ~ $subitem.emit( self ) ~ ", ";
                    };
                };
                $result := $result ~ " ], ";
            }
            else {
            if ($data{$item}).isa('Hash') {
                $result := $result ~ "{ ";
                for keys %($data{$item}) -> $subitem {
                    $result := $result
                        ~ $subitem
                        ~ ' => '
                        ~ (($data{$item}){$subitem}).emit( self )
                        ~ ", ";
                };
                $result := $result ~ " }, ";
            }
            else {
            if ($data{$item}).isa('Str') {
                $result := $result ~ "\'" ~ $data{$item} ~ "\', ";
            }
            else {
                $result := $result ~ ($data{$item}).emit( self ) ~ ", ";
            };
            };
            };
        };
        $result := $result ~ ") ";

        $result := $result ~ '</span>';

    };

}

=begin

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
