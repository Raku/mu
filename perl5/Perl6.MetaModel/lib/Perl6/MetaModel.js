
if (Perl6 == undefined) var Perl6 = {};

Perl6.MetaModel = function () {};

function require (classname) {
    document.write("<SCRIPT LANGUAGE='javascript' SRC='" + require.INC + "/" + classname.split('.').join('/') + ".js'></SCR" + "IPT>");    
}
require.INC = '.';

function iv (label, value) {    
    var self = SELF();
    if (value != undefined) self.attributes[label] = value;
    return self.attributes[label]; 
}

Perl6.MetaModel.CURRENT_DISPATCHER = [];

function call_method (inv, label, args) {
    var dispatcher; 
    var options;
    if (inv['_class']) {
        dispatcher = inv._class.meta().dispatcher();
        options = { 'for' : 'instance' };
    }
    else {
        dispatcher = inv.meta().dispatcher();
        options = { 'for' : 'class' };        
    }
    var method = WALKMETH(dispatcher, label, options);
    Perl6.MetaModel.CURRENT_DISPATCHER.push([ dispatcher, label, options, inv, args ]);        
    if (method == undefined) throw "Method " + label + " not found for " + inv;
    var rval = method.call(inv, args);
    Perl6.MetaModel.CURRENT_DISPATCHER.pop();    
    return rval;
}

function next_METHOD () {
    var curr_context = Perl6.MetaModel.CURRENT_DISPATCHER[Perl6.MetaModel.CURRENT_DISPATCHER.length - 1];
    var dispatcher = curr_context[0];
    var label      = curr_context[1]; 
    var options    = curr_context[2];                 
    var inv        = curr_context[3];
    var args       = curr_context[4];        
    var method = WALKMETH(dispatcher, label, options); 
    if (method == undefined) throw "Method " + label + " not found for " + inv;    
    return method.call(inv, args);    
}

function WALKCLASS (dispacther, options) {
    return dispacther.next();
}

function WALKMETH (dispatcher, label, options) {
    options = options || {};
    var current;
    while (current = WALKCLASS(dispatcher, options)) {
        //alert(current);
        if (current.has_method(label, options['for'])) {
            //alert(current + " has " + label);            
            break;
        }
    }
    if (current == undefined) {
        //alert(current + " does not have " + label);            
        return undefined;
    }
    return current.get_method(label, options['for']);
}

function CLASS () {
    if (Perl6.Method.CURRENT_CLASS_STACK.length == 0) {
        throw "You cannot call \$?CLASS from outside of a MetaModel method";
    }
    return Perl6.Method.CURRENT_CLASS_STACK[Perl6.Method.CURRENT_CLASS_STACK.length - 1];
}

function SELF () {
    if (Perl6.Method.CURRENT_INVOCANT_STACK.length == 0) {
        throw "You cannot call \$?SELF from outside of a MetaModel method";
    }
    return Perl6.Method.CURRENT_INVOCANT_STACK[Perl6.Method.CURRENT_INVOCANT_STACK.length - 1];    
}

/*

=pod

=head1 NAME

Perl6.MetaModel - Javascript Prototype of the Perl 6 Metaclass model

=head1 DESCRIPTION

This set of javascript modules is a prototype for the Perl 6 Metaclass model, which 
is the model which descibes the interactions of classes, objects and roles in the Perl
6 object space. 

I am prototyping this in Javascript as part of the PIL -> JS compiler to run on
a JS VM. It is a port of the Perl 5 prototype.

=head1 A NOTE ABOUT DEPENDENCIES

When using the Perl6.MetaModel, this file needs to be loaded first. This is for 2 reasons.
First, this module implements a (simplistic) C<require> method which can be used to load
other modules. And secondly, many of the modules in the C<Perl6.> namespace utilize some 
of the function found in this module. 

=head1 FUNCTIONS

=over 4

=item B<WALKMETH (dispatcher, label, options)>

=item B<WALKCLASS (dispatcher, options)>

=item B<SELF>

=item B<CLASS>

=back

=head1 SEE ALSO

=over 4

=item All the Perl 6 documentation. 

In particular the Apocolypse and Synopsis 12 which describes the object system.

=item L<Class::Role>, L<Class::Roles> & L<Class::Trait>

The first two are early attempts to prototype role behavior, and the last is an implementation
of the Trait system based on the paper which originally inspired Roles.

=item Any good Smalltalk book.

I prefer the Brown book by Adele Goldberg and David Robinson, but any one which talks about the
smalltalk metaclasses is a good reference.

=item CLOS

The Common Lisp Object System has a very nice meta-model, and plently of reference on it. In 
particular there is a small implementation of CLOS called TinyCLOS which is very readable (if 
you know enough Scheme that is)

=back

=head1 AUTHOR

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut

*/