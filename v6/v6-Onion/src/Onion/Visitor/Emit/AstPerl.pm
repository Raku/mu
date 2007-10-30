
use v6-alpha;

class Onion::Visitor::Emit::AstPerl {

    # This visitor builds a ".perl" representation of the AST

    method visit ( $node ) {

        if $node.isa('Array') {
            my $result := '';
            $result := $result ~ "[ ";
            my $subitem;
            for @($node) -> $subitem {
                $result := $result ~ self.visit($subitem) ~ ', ';
            }
            return $result ~ " ]";
        };
    
        if $node.isa('Str') {
            return "\'" ~ $node ~ "\'";
        };

        if $node.isa('Int') {
            return $node;
        };
        
        my $result := '';
        $result := $result ~ "::" ~ ($node.WHAT) ~ "( ";
        my $data := $node.attribs;
        my $item;
        for keys %($data) -> $item {
            $result := $result ~ " " ~ $item ~ " => ";
            if ($data{$item}).isa('Array') {
                $result := $result ~ self.visit( $data{$item}) ~ ", ";
            }
            else { 
            if ($data{$item}).isa('Hash') {
                $result := $result ~ "{ ";
                my $subitem;
                for keys %($data{$item}) -> $subitem {
                    $result := $result 
                        ~ $subitem 
                        ~ ' => '
                        ~ self.visit(($data{$item}){$subitem}) 
                        ~ ", ";
                };
                $result := $result ~ " }, ";
            } 
            else {
            if (($data{$item}).isa('Str')) || (($data{$item}).isa('Int')) {
                $result := $result ~ self.visit( $data{$item}) ~ ", ";
            } 
            else {
                if ($data{$item}) {
                $result := $result ~ self.visit($data{$item}) ~ ", "; 
                }
            };
            };
            };
        };
        $result := $result ~ ") ";
    };
}

=begin

=head1 NAME

Onion::Visitor::Emit::AstPerl - Build a C<.perl> representation of the AST

=head1 DESCRIPTION

Builds a C<.perl> representation of the AST. This is the only visitor
used for the C<--ast> option.

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
