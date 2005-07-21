
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

