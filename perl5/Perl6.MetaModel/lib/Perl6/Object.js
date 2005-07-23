
require('Perl6.Class');
require('Perl6.Instance');

if (Perl6 == undefined) var Perl6 = function () {};

Perl6.Object = new Perl6.Class('Perl6::Object', {
    'class' : {
        'methods' : {
            'new' : function (klass, params) {
                //document.write("new called with " + klass + " -> " + params + "<BR>");                
                return klass.can('bless').call(klass, params);
            },
            'bless' : function (klass, params) {
                //document.write("bless called with " + klass + " -> " + params + "<BR>");                
                var instance = klass.can('CREATE').call(klass, params);
                instance.can('BUILDALL').call(instance, params);
                return instance;
            },
            'CREATE' : function (klass, params) {
                var attrs = {};
                var dispatcher = klass.meta().dispatcher(':descendant');
                var mc;
                while (mc = WALKCLASS(dispatcher)) {
                    var attr_list = mc.get_attribute_list();
                    for (var i = 0; i < attr_list.length; i++) {
                        var attr_obj = mc.get_attribute(attr_list[i]);
                        //document.write("Perl6.Class - allocating space for ... " + attr_list[i] + "<BR>");
                        attrs[attr_list[i]] = attr_obj.instantiate_container();
                    }
                }
                return new Perl6.Instance(klass, attrs);
            }
        }
    },
    'instance' : {
        'methods' : {
            'BUILDALL' : function (self, params) {
                //document.write("BUILDALL called with " + params + "<BR>");
                var dispatcher = self.meta().dispatcher(':descendant');
                var method;
                while (method = WALKMETH(dispatcher, 'BUILD')) {
                    //method.force_call(self, params);
                    method.call(self, params);
                }
            },
            'DESTROYALL' : function (self, params) {
                var dispatcher = self.meta().dispatcher(':ascendant');
                var method;
                while (method = WALKMETH(dispatcher, 'DESTROY')) {
                    //method.force_call(self, params);
                    method.call(self, params);
                }
            },
            'BUILD' : function (self, params) {
                for (param in params) {
                    //document.write("BUILD setting " + param + " with " + params[param] + "<BR>");                    
                    // set the instance variables
                    iv(param, params[param]);
                }
            }
        }
    }
});