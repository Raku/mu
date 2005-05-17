#!/usr/bin/pugs

use v6;
use Test;

plan 3;

=pod

This test is based on the ideas in F<docs/create_your_own_object_model.pod>
and is probably totally wrong and needs re-thinking. 

However it does compile :)

=cut

use Perl::Meta::Class;
use Perl::Meta::Method;
use Perl::Meta::Property;

=pod 

{
   'class' => MetaClass,
   MetaClass => {
       'name' => 'AbstractClass',
       'parent' => undef,
       'instance_vars' => {
           AbstractClass => { 
               'name'          => "",
               'parents'       => [],
               'method_table'  => {}
           }
       },
       'method_table' => {
           'create_instance' => { 
               die "You cannot instantiate an AbstractClass";
           },
           'add_method_to_class' => ($class, $label) -> {
               $class.<method_table>.<$label> = \&abstractMethodHandler;
           },
           ...
       }
   }
}

=cut

## AbstractClass meta-class
my $abstract_class;

lives_ok {
    $abstract_class = Perl::Meta::Class::new('AbstractClass');

    # create properties
    $abstract_class.addProperty('name',         Perl::Meta::Property.new(:type<Str>)  );
    $abstract_class.addProperty('parents',      Perl::Meta::Property.new(:type<Array>));
    $abstract_class.addProperty('method_table', Perl::Meta::Property.new(:type<Hash>) );

    # create methods
    $abstract_class.addMethod('create_instance', 
        Perl::Meta::Method.new(code => sub {
            die "You cannot instantiate an AbstractClass"
        })
    );

    $abstract_class.addMethod('add_method_to_class', 
        Perl::Meta::Method.new(code => sub ($self, $label) {
            $self.properties()<method_table>{$label} = sub { 
                die "You cannot call an method on an AbstactClass" 
            }
        })
    );
}, '... created the AbstractClass ok';

=pod

{
   'class' => MetaClass,
   MetaClass => {
       'name' => 'Class',
       'parent' => AbstractClass,
       'instance_vars' => {
           Class => { 
               'instance_var' => {}
           }
       },
       'method_table' => {
           'create_instance' => { 
               return merge(
                   Class.parent.<instance_vars>,
                   { 'class' => Class, Class.<instance_vars> }
               ); 
           },
           'add_method_to_class' => ($class: $label, $impl) -> {
               $class<method_table><$label> = $impl;
           },
           ...
       }
   }
}

=cut

## Class meta-class

my $class;

lives_ok {
    $class = Perl::Meta::Class::new('Class');

    # set parent
    $class.superclass($abstract_class);

    # create properties
    $class.addProperty('instance_vars', Perl::Meta::Property.new(:type<Hash>));

    # create methods
    $class.addMethod('create_instance', 
        Perl::Meta::Method.new(code => sub ($self) {
            # ... how do I create an instance??
        })
    );

    $class.addMethod('add_method_to_class', 
        Perl::Meta::Method.new(code => sub ($self, $label, $impl) {
            $self.properties()<method_table>{$label} = $impl;
        })
    );
}, '... created the Class okay';

=pod

{
   'class' => MetaClass,
   MetaClass => {
       'name' => 'ThreadSafeClass',
       'parent' => Class,
       'instance_vars' => {
           'semaphore' => <semaphore>
       },
       'method_table' => {
           'add_method_to_class' => ($class: $label, $impl) -> {
               $class<method_table><$label> = ($self: @args) {
                   $self<semaphore>.acquire();
                   @return_vals =
		    $self.invoke_method($impl, @args);
                   $self<semaphore>.release();                        
                   return @return_vals;
               }
           },
           ...
       }
   }
}

=cut

## ThreadSafeClass meta-class

my $thread_safe_class;
lives_ok {
    $thread_safe_class = Perl::Meta::Class::new('ThreadSafeClass');

    # set parent
    $thread_safe_class.superclass($class);

    # create properties
    $thread_safe_class.addProperty('semaphore', Perl::Meta::Property.new(:type<Semaphore>));

    # create methods
    $thread_safe_class.addMethod('add_method_to_class', 
        Perl::Meta::Method.new(code => sub ($self, $label, $impl) {
            $self.properties()<method_table>{$label} = sub ($inv, @args) {
                $inv.semaphore().lock();
                my @return_vals = $impl(@args);
                $inv.semaphore().unlock();
                return @return_vals; 
            }
        })
    );
}, '... created the ThreadSafeClass ok';