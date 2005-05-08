
module Tree::Simple-0.0.1;
use v6;

my @INSTANCES = [ 'we start at 1 otherwise things get ugly' ];

sub Tree::Simple::new (+$node, +$parent) returns Int is export {
    my $instance = +@INSTANCES; # get the next index
    my %tree = (
        node     => $node,
        parent   => $parent, 
        depth    => ($parent.defined ?? ($parent.depth() + 1) :: 0),
        children => []
    );
    @INSTANCES[$instance] = %tree;
    return $instance;
}

sub depth ($instance:) returns Int {
    return @INSTANCES[$instance]<depth>;
}

sub node ($instance: ?$node) returns Any {
    @INSTANCES[$instance]<node> = $node if $node.defined;
    return @INSTANCES[$instance]<node>;
}

sub parent ($instance: ?$parent) returns Int {
    if $parent {
        @INSTANCES[$instance]<parent> = $parent;
        @INSTANCES[$instance]<depth> = $parent.depth() + 1;
    }
    return @INSTANCES[$instance]<parent>;
}

sub is_root ($instance:) returns Bool {
    return ($instance.parent() == undef ?? 1 :: 0) ;
}

sub is_leaf ($instance:) returns Bool {
    return ($instance.child_count() == 0);
}

sub child_count ($instance:) returns Int {
    return $instance.get_all_children() + 0;
}

sub add_child ($instance: $child) returns Void {
    $child.parent($instance);    
    my %self := @INSTANCES[$instance];    
    my @children;
    @children.push(%self<children>) if %self<children>;
    @children.push($child);
    %self<children> = \@children;
}

sub add_children ($instance: *@children) returns Void {
    for @children -> $child {
        $instance.add_child($child);
    }
}

sub get_child ($instance: $index) returns Int {
    my %self := @INSTANCES[$instance];
    return %self<children>[$index];
}

sub get_all_children ($instance:) returns Array {
    my %self := @INSTANCES[$instance];
    return %self<children>;    
}

sub traverse ($instance: Code $func) returns Void {
    for $instance.get_all_children() -> $child {
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

=item B<depth ($instance:) returns Int>

=item B<node ($instance: ?$node) returns Any>

=item B<parent ($instance: ?$parent) returns Int>

=item B<is_root ($instance:) returns Bool>

=item B<is_leaf ($instance:) returns Bool>

=item B<child_count ($instance:) returns Int>

=item B<add_child ($instance: $child) returns Void>

=item B<add_children ($instance: *@children) returns Void>

=item B<get_child ($instance: $index) returns Int>

=item B<get_all_children ($instance:) returns Array>

=item B<traverse ($instance: Code $func) returns Void>

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