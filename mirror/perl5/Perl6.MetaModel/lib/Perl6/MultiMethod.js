
if (Perl6 == undefined) var Perl6 = function () {};

Perl6.MultiMethod = function (associated_with, methods) {
    this._associated_with = associated_with;
    this._methods         = methods;
}

Perl6.MultiMethod.prototype._find_appropriate_method = function (inv, args) {
    var num_params = args.length + 1; // + 1 for the invocant
    //document.write("... looking for matching method with  : " + num_params + "<BR>");    
    var method;
    for (var i = 0; i < this._methods.length; i++) {
        var current = this._methods[i];
        // this only check the number of args, and 
        // not the types or anything else
        //document.write('current method arity is ' + current.arity() + "<BR>");
        if (current.arity() == num_params) {
            method = current;
            break;
        }
    }
    return method;
}

Perl6.MultiMethod.prototype.call = function (inv, args) {
    args = args || [];    
    //document.write("Multi-Method is called with inv:(" + inv + ") and num args : " + args.length + "<BR>");
    var method = this._find_appropriate_method(inv, args);
    if (method == undefined) throw "No Approriate Method found in MultiMethod";
    var rval = method.call(inv, args);
    return rval;
}

Perl6.MultiMethod.prototype.toString = function () {
    return "Perl6.MultiMethod=[" + this.associated_with() + "]";
}

/*

=pod

=head1 NAME

Perl6.MultiMethod - Base class for Multi-Methods in the Perl 6 Meta Model

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