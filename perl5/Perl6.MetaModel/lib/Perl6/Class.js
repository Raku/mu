
if (Perl6 == undefined) var Perl6 = function () {};

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
        var supers = [];
        for (var i = 0; i < options['is'].length; i++) {
            supers[i] = options['is'][i].meta();
        }
        my_class.meta().superclasses(supers);
    }
    if (options['does']) {
        ;
    }   
    if (options['instance']) {
        var instance = options['instance'];
    }
    if (options['class']) {
        var _class = options['class']
        if (_class['methods']) {
            for (label in _class['methods']) {
                var method = new Perl6.Method(my_class.meta().name(), _class['methods'][label]);
                my_class.meta().add_method(label, method, 'class');
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