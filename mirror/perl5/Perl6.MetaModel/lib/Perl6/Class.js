
if (Perl6 == undefined) var Perl6 = function () {};

if (Perl6.MetaClass   == undefined || Perl6.MetaClass.prototype.version == undefined) require('Perl6.MetaClass');     
if (Perl6.Method      == undefined) require('Perl6.Method');   
if (Perl6.MultiMethod == undefined) require('Perl6.MultiMethod');     
if (Perl6.Attribute   == undefined) require('Perl6.Attribute');   

Perl6.Class = function (name, options) {
    this.META = new Perl6.MetaClass();
    _extract_name(this, name);
    _process_options(this, options || {});
}

Perl6.Class.prototype.meta = function () {
    return this.META;
}

Perl6.Class.prototype.isa = function (class_name) {
    return this.META.is_a(class_name);
}

Perl6.Class.prototype.can = function (method_name) {
    return WALKMETH(this.meta().dispatcher(':canonical'), method_name, { 'for' : 'class' });
}

Perl6.Class.prototype.toString = function () {
    return "Perl6.Class=[" + this.meta().identifier() + "]";
}

// private function

function _extract_name (my_class, long_name) {
    var short_name = long_name.split('-');
    if (short_name[0] != undefined) {
        my_class.meta().name(short_name[0]);
    }
    if (short_name[1] != undefined) {
        my_class.meta().version(short_name[1]);
    }    
    if (short_name[2] != undefined) {
        my_class.meta().authority(short_name[2]);
    }    
}

function _process_options (my_class, options) {
    if (options['is']) {
        var is = options['is'];
        var supers = [];
        for (var i = 0; i < is.length; i++) {
            supers[i] = is[i].meta();
        }
        my_class.meta().superclasses(supers);
    }
    if (options['does']) {
        ;
    }   
    if (options['instance']) {
        var instance = options['instance'];
        if (instance['methods']) {
            for (var label in instance['methods']) {
                var method = new Perl6.Method(my_class.meta().name(), instance['methods'][label]);
                my_class.meta().add_method(label, method, 'instance');
            }
        }
        if (instance['multi_methods']) {
            for (var label in instance['multi_methods']) {
                var methods = [];
                for (var i = 0; i < instance['multi_methods'][label].length; i++) {
                    var body = instance['multi_methods'][label][i][0];
                    var arity = instance['multi_methods'][label][i][1];                    
                    methods[methods.length] = new Perl6.Method(my_class.meta().name(), body, arity);
                }
                var multi_method = new Perl6.MultiMethod(my_class.meta().name(), methods);
                my_class.meta().add_method(label, multi_method, 'instance');
            }
        }        
        if (instance['attrs']) {
            var attrs = instance['attrs'];
            for (var i = 0; i < attrs.length; i++) {
                var name;
                var props;
                if (typeof attrs[i] == 'object') {
                    name = attrs[i][0];
                    props = attrs[i][1];                    
                }
                else {
                    name = attrs[i];
                }
                var attribute = new Perl6.Attribute(my_class.meta().name(), name, props);
                my_class.meta().add_attribute(name, attribute, 'instance');
            }
        }
    }
    if (options['class']) {
        var _class = options['class']
        if (_class['methods']) {
            for (var label in _class['methods']) {
                var method = new Perl6.Method(my_class.meta().name(), _class['methods'][label]);
                my_class.meta().add_method(label, method, 'class');
            }
        }
        if (_class['multi_methods']) {
            for (var label in _class['multi_methods']) {
                var methods = [];
                for (var i = 0; i < _class['multi_methods'][label].length; i++) {
                    var body = _class['multi_methods'][label][i][0];
                    var arity = _class['multi_methods'][label][i][1];    
                    //document.write("Adding multi-method " + label + " to " + my_class.meta().name() + " with arity of " + arity + "<BR>");                
                    methods[methods.length] = new Perl6.Method(my_class.meta().name(), body, arity);
                }
                //document.write("adding ")
                var multi_method = new Perl6.MultiMethod(my_class.meta().name(), methods);
                my_class.meta().add_method(label, multi_method, 'class');
            }
        }                
        
        if (_class['attrs']) {
            var attrs = _class['attrs'];
            for (var i = 0; i < attrs.length; i++) {
                var name;
                var props;
                if (typeof attrs[i] == 'Array') {
                    name = attrs[i][0];
                    props = attrs[i][1];                    
                }
                else {
                    name = attrs[i];
                }                
                var attribute = new Perl6.Attribute(my_class.meta().name(), name, props);
                my_class.meta().add_attribute(name, attribute, 'instance');
            }
        }        
    }
}

/*

=pod

=head1 NAME

Perl6.Class - Base class for Classes in the Perl 6 Meta Model

=head1 DESCRIPTION

=head1 METHODS 

=over 4

=item B<new Perl6.Class (name, options)>

=item B<meta>

=item B<isa (class_name)>

=item B<can (method_name)>

=back

=head1 AUTHOR

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut

*/
