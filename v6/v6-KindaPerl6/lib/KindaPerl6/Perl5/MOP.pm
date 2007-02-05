package KindaPerl6::MOP;
use Class::MOP;
use base 'Class::MOP::Class';

eval {
    $_->alias_method("HOW" => sub { (shift)->meta }) 
} foreach Class::MOP::get_all_metaclass_instances;

sub create {
    my $class = shift;
    my $new_class = shift;
    $new_class = $$new_class; # unbox to p5
    my $meta = $class->SUPER::create( $new_class );
    $meta->alias_method("HOW" => sub { (shift)->meta }); 
    return $meta;    
}

1;
