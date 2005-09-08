#!/usr/bin/perl

use strict;
use warnings;

BEGIN { do "lib/gnosis.pl" };

# ... this constructs the rest of the ::Class
# the result of this file is the (demiurge)

$::Class->add_method('new' => ::make_method(sub {
    my ($class, %params) = @_;
    return $class->bless(undef, %params);    
}, $::Class));

$::Class->add_method('bless' => ::make_method(sub {
    my ($class, $canidate, %params) = @_;
    $canidate ||= 'P6opaque'; # opaque is our default
    my $self = $class->CREATE(repr => $canidate, %params);
    $self->BUILDALL(%params);
    return $self;  
}, $::Class));

$::Class->add_method('CREATE' => ::make_method(sub { 
    my ($class, %params) = @_;
    ($params{repr} eq 'P6opaque') 
        || confess "Sorry, No other types other than 'P6opaque' are currently supported";    
    # this just gathers all the 
    # attributes that were defined
    # for the instances.
    my %attrs;
    my $dispatcher = $class->dispatcher(':descendant');
    while (my $c = ::WALKCLASS($dispatcher)) {
        foreach my $attr ($c->get_attribute_list()) {
            my $attr_obj = $c->get_attribute($attr);
            $attrs{$attr} = ::instantiate_attribute_container($attr_obj);
        }
    }
    # this is our P6opaque data structure
    # it's nothing special, but it works :)
    my $self = ::create_opaque_instance(\$class, %attrs);              
    # and now return it ...
    return $self;
}, $::Class));

$::Class->add_method('BUILDALL' => ::make_method(sub { 
    my ($self, %params) = @_;
    my $dispatcher = ::opaque_instance_class($self)->dispatcher(':descendant');
    while (my $method = ::WALKMETH($dispatcher, 'BUILD')) { 
        $method->($Perl6::Submethod::FORCE, $self, %params);                  
    }      
}, $::Class));

$::Class->add_method('BUILD' => ::make_submethod(sub { 
    my ($self, %params) = @_;
    foreach my $key (keys %params) {
        # XXX -
        # The default BUILD method should accept
        # params which are not included in the 
        # attributes. It will do nothing with them
        # but it will allow them to exist.
        # - (see t_oo/submethods.t)
        ::opaque_instance_attrs($self)->{$key} = $params{$key}
            # NOTE:
            # this is an ugly way to do this, ideally
            # we would peek into the instance structure
            # itself and see if we had the spot, and
            # otherwise ignore it ... but this will do
            if ::opaque_instance_class($self)->find_attribute_spec($key);
    }
}, $::Class));

$::Class->add_method('DESTROYALL' => ::make_method(sub { 
    my ($self) = @_;
    my $dispatcher = ::opaque_instance_class($self)->dispatcher(':ascendant');
    while (my $method = ::WALKMETH($dispatcher, 'DESTROY')) {  
        $method->($Perl6::Submethod::FORCE, $self);   
    }  
}, $::Class));

$::Class->add_method('isa' => ::make_method(sub { 
    my ($self, $class_name) = @_;
    return undef unless $class_name;
    return 1 if $self->name eq $class_name;  
    my $dispatcher = $self->dispatcher(':canonical');
    while (my $next = $dispatcher->()) {    
        return 1 if $self->name eq $next->name;
    }
    return 0; 
}, $::Class));

$::Class->add_method('can' => ::make_method(sub { 
    my ($self, $label) = @_;
    return undef unless $label;
    return ::WALKMETH(::opaque_instance_class($self)->dispatcher(':canonical'), $label);
}, $::Class));

$::Class->add_method('id' => ::make_method(sub { ::opaque_instance_id(shift) }, $::Class));

$::Class->add_method('superclasses' => ::make_method(sub {        
    my ($self, $superclasses) = @_;
    if (defined $superclasses) {
        confess "You must pass the superclasses in an ARRAY ref"
            unless ref($superclasses) eq 'ARRAY';
        # XXX -
        # we should check that none of the classes passed to us
        # are also subclasses of us, this is circular inheritance
        # and not allowed.
        ::opaque_instance_attrs($self)->{'@:superclasses'} = $superclasses; 
        # clear the MRO now
        ::opaque_instance_attrs($self)->{'@:MRO'} = [];
        # and recalculate it ..
        $self->MRO();
    }
    ::opaque_instance_attrs($self)->{'@:superclasses'};
}, $::Class));

$::Class->add_method('_merge' => ::make_private_method(sub {                
    my ($self, @seqs) = @_;
    my @res;
    while (1) {
        # remove all empty seqences
        my @nonemptyseqs = (map { (@{$_} ? $_ : ()) } @seqs);
        # return the list if we have no more no-empty sequences
        return @res if not @nonemptyseqs; 
        my $cand; # a canidate ..
        foreach my $seq (@nonemptyseqs) {
            $cand = $seq->[0]; # get the head of the list
            my $nothead;            
            foreach my $sub_seq (@nonemptyseqs) {
                # XXX - this is instead of the python "in"
                my %in_tail = (map { $_ => 1 } @{$sub_seq}[ 1 .. $#{$sub_seq} ]);
                # NOTE:
                # jump out as soon as we find one matching
                # there is no reason not too. However, if 
                # we find one, then just remove the '&& last'
                $nothead++ && last if exists $in_tail{$cand};      
            }
            last unless $nothead; # leave the loop with our canidate ...
            $cand = undef;        # otherwise, reject it ...
        }
        confess "Inconsistent hierarchy" if not $cand;
        push @res => $cand;
        # now loop through our non-empties and pop 
        # off the head if it matches our canidate
        foreach my $seq (@nonemptyseqs) {
            shift @{$seq} if $seq->[0] eq $cand;
        }
    }
}, $::Class));

$::Class->add_method('MRO' => ::make_method(sub { 
    my $self = shift;
    unless (@{::opaque_instance_attrs($self)->{'@:MRO'}}) {
        ::opaque_instance_attrs($self)->{'@:MRO'} = [
            $self->_merge(
                [ $self ],                                      # the class we are linearizing
                (map { [ $_->MRO() ] } @{$self->superclasses}), # the MRO of all the superclasses
                [ @{$self->superclasses} ]                      # a list of all the superclasses
            )
        ];
    }
    return @{::opaque_instance_attrs($self)->{'@:MRO'}};
}, $::Class));

$::Class->add_method('dispatcher' => ::make_method(sub {
    my ($self, $order) = @_;   
    $order = ':ascendant' # C3 is the canonical order
        if not(defined($order)) || $order eq ':canonical';
    my $dispatcher;
    if ($order eq ':preorder') {
        $dispatcher = $self->_make_preorder_dispatcher();
    }
    elsif ($order eq ':breadth') {
        $dispatcher = $self->_make_breadth_dispatcher();
    }
    elsif ($order eq ':descendant') {
        $dispatcher = $self->_make_descendant_dispatcher();
    }    
    elsif ($order eq ':ascendant') {
        $dispatcher = $self->_make_ascendant_dispatcher();
    }   
    else {
        confess 'Unsupported dispatch order ($order)'
    }
    return $dispatcher;  
}, $::Class));

$::Class->add_method('_make_dispatcher_iterator' => ::make_private_method(sub {
    my (undef, @values) = @_;
    my $counter = 0;
    return sub { $values[$counter++] };
}, $::Class));

$::Class->add_method('_make_preorder_dispatcher' => ::make_private_method(sub {
    my @stack = $::SELF->_make_dispatcher_iterator($::SELF);
    return sub {
        TOP: {
            if (defined $stack[-1]) {
                # get the iterator on the top of the stack
                # get the current value out of the iterator
                my $current_class = $stack[-1]->();
                # if current is null then ...
                if (not defined $current_class) {
                    # that iterator is exhausted and we 
                    # need to pop it off the stack ...
                    pop @stack;
                    # now go back to the top and start over
                    redo TOP;
                }
                else {
                    push @stack => $::SELF->_make_dispatcher_iterator(@{$current_class->superclasses})
                        if $current_class->superclasses;
                }             
                return $current_class;
            }
            return undef;
        }
    };    
}, $::Class));

$::Class->add_method('_make_breadth_dispatcher' => ::make_private_method(sub {
    my @stack = $::SELF->_make_dispatcher_iterator($::SELF);
    return sub {
        TOP:
            if (scalar(@stack) != -0) {
                # get the iterator on the top of the stack
                # get the current value out of the iterator
                my $current_class = $stack[0]->();
                # if current is null then ...
                if (not defined $current_class) {
                    # that iterator is exhausted and we 
                    # need to pop it off the stack ...
                    shift @stack;
                    # now go back to the top and start over
                    goto TOP;
                }
                else {
                    push @stack => $::SELF->_make_dispatcher_iterator(@{$current_class->superclasses})
                        if $current_class->superclasses;
                }             
                return $current_class;
            }
            return undef;
    };
}, $::Class));

$::Class->add_method('_make_descendant_dispatcher' => ::make_private_method(sub {
    my @MRO = $::SELF->MRO();
    return $::SELF->_make_dispatcher_iterator(reverse @MRO);
}, $::Class));

$::Class->add_method('_make_ascendant_dispatcher' => ::make_private_method(sub {
    my @MRO = $::SELF->MRO();
    return $::SELF->_make_dispatcher_iterator(@MRO);
}, $::Class));

$::Class->add_method('is_a' => ::make_method(sub {        
    my ($self, $class) = @_;
    return 0 unless defined $class;
    return 1 if ::opaque_instance_id($self) == ::opaque_instance_id($class);
    my $dispatcher = $self->dispatcher(':canonical');
    while (my $next = $dispatcher->()) {    
        return 1 if ::opaque_instance_id($next) eq ::opaque_instance_id($class);
    }
    return 0; 
}, $::Class));

$::Class->add_method('_get_method_table' => ::make_private_method(sub {         
    my ($self, $params) = @_;
    # default to instance ... 
    $params->{for} = 'instance' if not exists $params->{for};
    my $method_table;
    if (lc($params->{for}) eq 'instance' ||
        lc($params->{for}) eq 'submethod') {
        return ::opaque_instance_attrs($self)->{'%:methods'};
    }
    elsif (lc($params->{for}) eq 'class') {
        return ::opaque_instance_attrs($self)->{'%:class_methods'};
    }
    elsif (lc($params->{for}) eq 'private') {
        return ::opaque_instance_attrs($self)->{'%:private_methods'};
    }        
    else {
        confess "There is no " . $params->{for} . " method table";
    }
}, $::Class));

$::Class->add_method('has_method' => ::make_method(sub {
    my ($self, $label, %params) = @_;
    $self->get_method($label, %params) ? 1 : 0;                    
}, $::Class));

$::Class->add_method('get_method' => ::make_method(sub {
    my ($self, $label, %params) = @_;
    confess "You must provide a method label"
        unless defined $label;
    my $method_table = $self->_get_method_table(\%params);
    return $method_table->{$label};                
}, $::Class));

$::Class->add_method('add_attribute' => ::make_method(sub {
    my ($self, $label, $attribute) = @_;
    (defined $label && defined $attribute && blessed($attribute))
        || confess "InsufficientArguments : you must provide an attribute and a label";
    #$self->_create_accessor($attribute);          
    if (blessed($attribute) eq 'Perl6::Attribute') {
        ::opaque_instance_attrs($self)->{'%:attributes'}->{$label} = $attribute;
    }
    elsif (blessed($attribute) eq 'Perl6::ClassAttribute') {
        ::opaque_instance_attrs($self)->{'%:class_attributes'}->{$label} = $attribute;
    }
    else {
        confess "I do not recognize the attribute type ($attribute)";
    }    
}, $::Class));

$::Class->add_method('_get_attribute_table' => ::make_private_method(sub {         
    my ($self, $params) = @_;
    # default to instance ... 
    $params->{for} = 'instance' if not exists $params->{for};    
    my $method_table;
    if (lc($params->{for}) eq 'instance') {
        return ::opaque_instance_attrs($self)->{'%:attributes'};
    }
    elsif (lc($params->{for}) eq 'class') {
        return ::opaque_instance_attrs($self)->{'%:class_attributes'};
    }      
    else {
        confess "There is no " . $params->{for} . " attribute table";
    }
}, $::Class));

$::Class->add_method('get_attribute' => ::make_method(sub {
    my ($self, $label, %params) = @_;
    (defined $label)
        || confess "InsufficientArguments : you must provide a label";
    my $table = $self->_get_attribute_table(\%params);                    
    return $table->{$label};
}, $::Class));
                           

$::Class->add_method('has_attribute' => ::make_method(sub {
    my ($self, $label, %params) = @_;
    return $self->get_attribute($label, %params) ? 1 : 0;
}, $::Class));


$::Class->add_method('get_attribute_list' => ::make_method(sub {
    my ($self, %params) = @_;
    my $table = $self->_get_attribute_table(\%params);                  
    return keys %{$table};
}, $::Class));

# "spec" here means "whatever annotation went with this attribute when it's declared"
$::Class->add_method('find_attribute_spec' => ::make_method(sub {
    my ($self, $label, %params) = @_;
    # go in BUILD order
    my $dispatcher = $self->dispatcher(':descendant');
    while (my $next = $dispatcher->()) {   
        return $next->get_attribute($label, %params)
            if $next->has_attribute($label, %params)
    } 
    return undef;
}, $::Class));

# now add the $::Class attributes

$::Class->add_attribute('@:MRO'              => ::make_attribute('@:MRO'));
$::Class->add_attribute('@:superclasses'     => ::make_attribute('@:superclasses'));
$::Class->add_attribute('%:private_methods'  => ::make_attribute('%:private_methods'));
$::Class->add_attribute('%:attributes'       => ::make_attribute('%:attributes'));
$::Class->add_attribute('%:methods'          => ::make_attribute('%:methods'));
$::Class->add_attribute('%:class_attributes' => ::make_attribute('%:class_attributes'));
$::Class->add_attribute('%:class_methods'    => ::make_attribute('%:class_methods'));

1;
