
=head1 CLASS DIAGRAM

=begin uml

attributes:
  bar: string

associations:
  bar: MyOtherClass

=end uml

=head2

=cut

use v6;

use Doc::Dialect::uml qw(uml_from);

class MyClass is uml(%=POD<uml>) {

	method foo {

	}
}

