
use v6;

class Tree::Visitor-0.0.1;

has $.include_trunk is rw;

has @!results;
has $!filter_function;

submethod BUILD (Code $!filter_function, Bool $.include_trunk) {}

method has_node_filter   ($self:) returns Bool { $.filter_function.defined }
method get_node_filter   ($self:) returns Code { $!filter_function         }
method clear_node_filter ($self:) returns Void { $!filter_function = undef }
method set_node_filter   ($self: Code $filter_function) { $!filter_function = $filter_function }

# XXX This is non-idiomatic Perl -- better: method results ($self:) is rw {...}
sub set_results ($self: @results) returns Void  { @!results = @results }
sub get_results ($self:)          returns Array { @!results }

# it is abstract
method visit ($self: Tree $tree) returns Void { ... }

=pod

=head1 AUTHOR

stevan little, E<lt>stevan@iinteractive.comE<gt>

=head1 COPYRIGHT

Copyright (c) 2005. Stevan Little. All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
