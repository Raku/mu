#!./pugs

use v6;
use Test::PIL::Bootstrap;

# check that we have our components

pil_is_ok('::Class',   '... ::Class is defined');
pil_is_ok('::Object',  '... ::Object is defined');
pil_is_ok('::Package', '... ::Package is defined');
pil_is_ok('::Module',  '... ::Module is defined');

# check the isa relationships

pil_is_eq('::Class.isa("Class")',   'true', '... ::Class.isa(Class)');
pil_is_eq('::Class.isa("Object")',  'true', '... ::Class.isa(Object)');
pil_is_eq('::Class.isa("Package")', 'true', '... ::Class.isa(Package)');
pil_is_eq('::Class.isa("Module")',  'true', '... ::Class.isa(Module)');

pil_is_eq('::Module.isa("Module")',  'true', '... ::Module.isa(Module)');
pil_is_eq('::Module.isa("Package")', 'true', '... ::Module.isa(Package)');
pil_is_eq('::Module.isa("Object")',  'true', '... ::Module.isa(Object)');
                         
pil_is_eq('::Package.isa("Package")', 'true', '... ::Package.isa(Package)');
pil_is_eq('::Package.isa("Object")',  'true', '... ::Package.isa(Object)');

pil_is_eq('::Object.isa("Object")',  'true', '... ::Object.isa(Object)');

# check the class API

for (qw(
    new bless CREATE BUILDALL BUILD DESTROYALL
    add_method has_method get_method get_method_list
    add_attribute has_attribute get_attribute get_attribute_list get_attributes
    superclasses set_superclasses subclasses add_subclass MRO
    dispatcher is_a isa can 
    name set_name version set_version authority set_authority identifier   
    )) -> $method_name {
    pil_is_eq(
    (
        '[::Class.can("' ~ $method_name ~ '"), ' ~
        '::Object.can("' ~ $method_name ~ '"), ' ~
        '::Module.can("' ~ $method_name ~ '"), ' ~
        '::Package.can("' ~ $method_name ~ '")]'
    ),
    '[true, true, true, true]', 
    '... (::Class, ::Object, ::Module, ::Package).can(' ~ $method_name ~ ')');
          
}
