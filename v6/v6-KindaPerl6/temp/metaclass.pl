
# TODO - create ClassMOP.pm visitor to transform OO calls into Class::MOP calls

use Class::MOP;

    Class::MOP::Class->create('Bar' => (
        version      => '0.01',
        superclasses => [ 'Foo' ],
        attributes => [
        ],
        methods => {
        },
    ) );
    
    Bar->meta->add_method('bar' => sub { 789 });

print Bar->bar;