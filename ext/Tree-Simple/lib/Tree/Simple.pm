
module Tree::Simple-0.0.1;
use v6;

my %INSTANCES;

sub Tree::Simple::new (+$node, +$parent) returns Int is export {
    my $self = substr(rand() ~ "", 2, 15);
    my %tree = (
        node     => $node,
        parent   => $parent, 
        depth    => ($parent.defined ?? ($parent.depth() + 1) :: 0),
        children => []
    );
    %INSTANCES{$self} = %tree;
    return $self;
}

sub depth ($self:) returns Int {
    return %INSTANCES{$self}<depth>;
}

sub node ($self: ?$node) returns Any {
    %INSTANCES{$self}<node> = $node if $node.defined;
    return %INSTANCES{$self}<node>;
}

sub parent ($self: ?$parent) returns Int {
    if $parent {
        %INSTANCES{$self}<parent> = $parent;
        %INSTANCES{$self}<depth> = $parent.depth() + 1;
    }
    return %INSTANCES{$self}<parent>;
}

sub is_root ($self:) returns Bool {
    return (%INSTANCES{$self}<parent> == undef ?? 1 :: 0) ;
}

sub is_leaf ($self:) returns Bool {
    return ($self.child_count() == 0);
}

sub child_count ($self:) returns Int {
    return $self.get_all_children() + 0;
}

sub add_child ($self: $child) returns Void {
    $child.parent($self);    
    my %self := %INSTANCES{$self};    
    my @children;
    @children.push(%self<children>) if %self<children>;
    @children.push($child);
    %self<children> = \@children;
}

sub add_children ($self: *@children) returns Void {
    for @children -> $child {
        $self.add_child($child);
    }
}

sub get_child ($self: $index) returns Int {
    my %self := %INSTANCES{$self};
    return %self<children>[$index];
}

sub get_all_children ($self:) returns Array {
    my %self := %INSTANCES{$self};
    return %self<children>;    
}

sub traverse ($self: Code $func) returns Void {
    for $self.get_all_children() -> $child {
        $func($child);
        $child.traverse($func);
    }
}

=pod

=head1 NAME

Tree::Simple - An simple I<n>-ary Tree

=head1 SYNOPSIS

  use Tree::Simple;

=head1 DESCRIPTION

This is an inside-out object implementation based on the perl5 module Tree::Simple.

=head1 METHODS

=over 4

=item B<Tree::Simple::new(?$node, ?$parent)>

=item B<depth ($self:) returns Int>

=item B<node ($self: ?$node) returns Any>

=item B<parent ($self: ?$parent) returns Int>

=item B<is_root ($self:) returns Bool>

=item B<is_leaf ($self:) returns Bool>

=item B<child_count ($self:) returns Int>

=item B<add_child ($self: $child) returns Void>

=item B<add_children ($self: *@children) returns Void>

=item B<get_child ($self: $index) returns Int>

=item B<get_all_children ($self:) returns Array>

=item B<traverse ($self: Code $func) returns Void>

=back

=head1 SEE ALSO

Perl 5 L<Tree::Simple>

=head1 AUTHOR

stevan little, E<lt>stevan@iinteractive.comE<gt>

=head1 COPYRIGHT

Copyright (c) 2005. Stevan Little. All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
