
if (Perl6 == undefined) var Perl6 = function () {};

Perl6.Method = function (associated_with, code) {
    this._associated_with = associated_with;
    this._code            = code;
}

Perl6.Method.CURRENT_CLASS_STACK    = [];
Perl6.Method.CURRENT_INVOCANT_STACK = [];

Perl6.Method.prototype.associated_with = function () {
    return this._associated_with;
}

Perl6.Method.prototype.call = function (inv, args) {
    Perl6.Method.CURRENT_CLASS_STACK.push(this._associated_with);
    Perl6.Method.CURRENT_INVOCANT_STACK.push(inv);
    var rval = this._code(inv, args);
    Perl6.Method.CURRENT_INVOCANT_STACK.pop();
    Perl6.Method.CURRENT_CLASS_STACK.pop();
    return rval;    
}

Perl6.Method.prototype.toString = function () {
    return "Perl6.Method=[" + this.associated_with() + "]";
}

/*

=pod

=head1 NAME

Perl6.Method - Base class for Methods in the Perl 6 Meta Model

=head1 DESCRIPTION

=head1 METHODS 

=over 4

=item B<new Perl6.Method (associated_with code)>

=item B<call (inv, args)>

=item B<associated_with>

=back

=head1 AUTHOR

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut

*/