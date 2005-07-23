
if (Perl6 == undefined) var Perl6 = function () {};

Perl6.Instance = function (_class, attributes) {
    this._class = _class;
    this.attributes = attributes || {};
}

Perl6.Instance.prototype.meta = function () {
    return this._class.meta();
}

Perl6.Instance.prototype.isa = function (class_name) {
    return this._class.meta().is_a(class_name);
}

Perl6.Instance.prototype.can = function (method_name) {
    return WALKMETH(this._class.meta().dispatcher(':canonical'), method_name, { 'for' : 'instance' });
}

Perl6.Instance.prototype.toString = function () {
    return "Perl6.Instance=[" + this._class.meta().identifier() + "]";
}

/*

=pod

=head1 NAME

Perl6.Instance - Base class for Instances in the Perl 6 Meta Model

=head1 DESCRIPTION

This is mearly the instance structure, do not think of this as a Javascript object which is 
supposed to behave like a Perl 6 object, but as a Perl6 object instance all it's own.

=head1 METHODS 

=over 4

=item B<new Perl6.Instance (name, attributes)>

=item B<meta>

=item B<isa (class_name)>

=item B<can (method_name)>

=back

=head1 AUTHOR

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut

*/