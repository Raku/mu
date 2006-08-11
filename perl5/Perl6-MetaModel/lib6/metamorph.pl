
=pod

This is a rough translation of metamorph.pl into Perl 6. It is mostly 
an experiment in using the MOP, and not meant to be anything definitive.

=cut


::Class.meta.add_method('new', method (%params) returns Class { 
    $?CLASS.bless(undef, %params);
});

::Class.meta.add_method('bless' => method ($canidate, %params) returns Class {
    $canidate //= 'P6opaque'; # opaque is our default
    my Class $self = $?CLASS.CREATE(repr => $canidate, %params);
    $self.BUILDALL(%params);
    return $self;  
});

::Class.meta.add_method('CREATE' => method (%params) { 
    # this just gathers all the 
    # attributes that were defined
    # for the instances.
    my %attrs;
    my Dispatcher $dispatcher = $class.dispatcher(:descendant);
    for WALKCLASS($dispatcher) -> Class $c {
        for $c.get_attribute_list() -> $attr {
            my $attr_obj = $c.get_attribute($attr);
            %attrs{$attr} = instantiate_attribute_container($attr_obj);
        }
    }
    # this is our P6opaque data structure
    # it's nothing special, but it works :)
    my OpaqueInstance $self = create_opaque_instance(\$class, %attrs);              
    # and now return it ...
    return $self;
});

::Class.meta.add_method('BUILDALL' => method (%params) returns Void { 
    my Dispatcher $dispatcher = $self.meta.dispatcher(:descendant);
    for WALKMETH($dispatcher, 'BUILD') -> Method $method { 
        $method.(self, %params);                  
    }      
});

::Class.meta.add_method('BUILD' => submethod (%params) returns Void {
    for %params.keys -> $key {
        # XXX -
        # The default BUILD method should accept
        # params which are not included in the 
        # attributes. It will do nothing with them
        # but it will allow them to exist.
        # - (see t_oo/submethods.t)
        opaque_instance_attr(self, $key) = $params{$key}
            # NOTE:
            # this is an ugly way to do this, ideally
            # we would peek into the instance structure
            # itself and see if we had the spot, and
            # otherwise ignore it ... but this will do
            if $self.meta.find_attribute_spec($key);
    }
});

::Class.meta.add_method('DESTROYALL' => method returns Void { 
    my Dispatcher $dispatcher = self.meta.dispatcher(:ascendant);
    for WALKMETH($dispatcher, 'BUILD') -> Method $method { 
        $method.(self, %params);                  
    }      
});

::Class.meta.add_method('isa' => method (Str $class_name) returns Bool { 
    return undef unless $class_name;
    my Dispatcher $dispatcher = self.meta.dispatcher(:canonical);
    while (my $next = $dispatcher.next()) {
        return bool::true if $class_name eq $next.meta.name;
    }
    # if we are not a class of something
    # maybe they are asking of we are an 
    # instance of something,.. so we pass
    # it back up to our class
    return self.isa($class_name) 
        # however, we need to not do this 
        # for $::Class as that presents a 
        # meta-circularity issue, and it
        # loops endlessly
        unless $self ~~ ::Class;
    return bool::false;
});

## I ended here ... to be continued 

::Class.meta.add_method('can' => method () { 
    my ($self, $label) = @_;
    return undef unless $label;
    return ::WALKMETH(::opaque_instance_class($self)->dispatcher(':canonical'), $label);
});

::Class.meta.add_method('id' => method () { ::opaque_instance_id($::SELF) });

::Class.meta.add_method('superclasses' => method () {        
    my ($self, $superclasses) = @_;
    if (defined $superclasses) {
        confess "You must pass the superclasses in an ARRAY ref"
            unless ref($superclasses) eq 'ARRAY';
        foreach my $super (@{$superclasses}) {
            $super->add_subclass($self);
        }
        # XXX -
        # we should check that none of the classes passed to us
        # are also subclasses of us, this is circular inheritance
        # and not allowed.
        ::opaque_instance_attr($self => '@:superclasses') = $superclasses; 
        # clear the MRO now
        ::opaque_instance_attr($self => '@:MRO') = [];
        # and recalculate it ..
        $self->MRO();
    }
    ::opaque_instance_attr($self => '@:superclasses');
});

::Class.meta.add_method('subclasses' => method () {        
    ::opaque_instance_attr($::SELF => '@:subclasses');
});

::Class.meta.add_method('add_subclass' => method () {      
    my ($self, $subclass) = @_;  
    push @{::opaque_instance_attr($self => '@:subclasses')} => $subclass;
});

::Class.meta.add_method('_merge' => ::make_private_method(sub {                
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
});

::Class.meta.add_method('MRO' => method () { 
    my $self = shift;
    unless (@{::opaque_instance_attr($self => '@:MRO')}) {
        ::opaque_instance_attr($self => '@:MRO') = [
            $self->_merge(
                [ $self ],                                      # the class we are linearizing
                (map { [ $_->MRO() ] } @{$self->superclasses}), # the MRO of all the superclasses
                [ @{$self->superclasses} ]                      # a list of all the superclasses
            )
        ];
    }
    return @{::opaque_instance_attr($self => '@:MRO')};
});

::Class.meta.add_method('dispatcher' => method () {
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
});

::Class.meta.add_method('_make_dispatcher_iterator' => ::make_private_method(sub {
    my (undef, @values) = @_;
    my $counter = 0;
    return sub { return $counter if @_; $values[$counter++] };
});

::Class.meta.add_method('_make_preorder_dispatcher' => ::make_private_method(sub {
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
});

::Class.meta.add_method('_make_breadth_dispatcher' => ::make_private_method(sub {
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
});

::Class.meta.add_method('_make_descendant_dispatcher' => ::make_private_method(sub {
    my @MRO = $::SELF->MRO();
    return $::SELF->_make_dispatcher_iterator(reverse @MRO);
});

::Class.meta.add_method('_make_ascendant_dispatcher' => ::make_private_method(sub {
    my @MRO = $::SELF->MRO();
    return $::SELF->_make_dispatcher_iterator(@MRO);
});

::Class.meta.add_method('is_a' => method () {        
    my ($self, $class) = @_;
    return 0 unless defined $class;
    return 1 if ::opaque_instance_id($self) == ::opaque_instance_id($class);
    my $dispatcher = $self->dispatcher(':canonical');
    while (my $next = $dispatcher->()) {    
        return 1 if ::opaque_instance_id($next) eq ::opaque_instance_id($class);
    }
    return 0; 
});

::Class.meta.add_method('_get_method_table' => ::make_private_method(sub {         
    my ($self, $params) = @_;
    # default to instance ... 
    $params->{for} = 'instance' if not exists $params->{for};
    my $method_table;
    if (lc($params->{for}) eq 'instance' ||
        lc($params->{for}) eq 'submethod') {
        return ::opaque_instance_attr($self => '%:methods');
    }
    elsif (lc($params->{for}) eq 'class') {
        return ::opaque_instance_attr($self => '%:class_methods');
    }
    elsif (lc($params->{for}) eq 'private') {
        return ::opaque_instance_attr($self => '%:private_methods');
    }        
    else {
        confess "There is no " . $params->{for} . " method table";
    }
});

::Class.meta.add_method('has_method' => method () {
    my ($self, $label, %params) = @_;
    $self->get_method($label, %params) ? 1 : 0;                    
});

::Class.meta.add_method('get_method' => method () {
    my ($self, $label, %params) = @_;
    confess "You must provide a method label"
        unless defined $label;
    my $method_table = $self->_get_method_table(\%params);
    return $method_table->{$label};                
});

::Class.meta.add_method('get_method_list' => method () {
    my ($self, %params) = @_;
    my $table = $self->_get_method_table(\%params);                  
    return keys %{$table};
});

::Class.meta.add_method('remove_method' => method () {
    my ($self, $label, %params) = @_;
    confess "You must provide a method label"
        unless defined $label;
    my $method_table = $self->_get_method_table(\%params);
    delete $method_table->{$label};   
});

::Class.meta.add_method('add_attribute' => method () {
    my ($self, $label, $attribute) = @_;
    (defined $label && defined $attribute && blessed($attribute))
        || confess "InsufficientArguments : you must provide an attribute and a label";
    #$self->_create_accessor($attribute);          
    if (blessed($attribute) eq 'Perl6::Attribute') {
        ::opaque_instance_attr($self => '%:attributes')->{$label} = $attribute;
    }
    else {
        confess "I do not recognize the attribute type ($attribute)";
    }    
});

::Class.meta.add_method('_get_attribute_table' => ::make_private_method(sub {         
    my ($self, $params) = @_;
    # default to instance ... 
    $params->{for} = 'instance' if not exists $params->{for};    
    my $method_table;
    if (lc($params->{for}) eq 'instance') {
        return ::opaque_instance_attr($self => '%:attributes');
    }     
    else {
        confess "There is no " . $params->{for} . " attribute table";
    }
});

::Class.meta.add_method('get_attribute' => method () {
    my ($self, $label, %params) = @_;
    (defined $label)
        || confess "InsufficientArguments : you must provide a label";
    my $table = $self->_get_attribute_table(\%params);                    
    return $table->{$label};
});


::Class.meta.add_method('has_attribute' => method () {
    my ($self, $label, %params) = @_;
    return $self->get_attribute($label, %params) ? 1 : 0;
});


::Class.meta.add_method('get_attribute_list' => method () {
    my ($self, %params) = @_;
    my $table = $self->_get_attribute_table(\%params);                  
    return keys %{$table};
});

# "spec" here means "whatever annotation went with this attribute when it's declared"
::Class.meta.add_method('find_attribute_spec' => method () {
    my ($self, $label, %params) = @_;
    # go in BUILD order
    my $dispatcher = $self->dispatcher(':descendant');
    while (my $next = $dispatcher->()) {   
        return $next->get_attribute($label, %params)
            if $next->has_attribute($label, %params)
    } 
    return undef;
});

# now add the $::Class attributes

::Class.meta.add_attribute('@:MRO'              => ::make_attribute('@:MRO'));
::Class.meta.add_attribute('@:superclasses'     => ::make_attribute('@:superclasses'));
::Class.meta.add_attribute('@:subclasses'       => ::make_attribute('@:subclasses'));
::Class.meta.add_attribute('%:private_methods'  => ::make_attribute('%:private_methods'));
::Class.meta.add_attribute('%:attributes'       => ::make_attribute('%:attributes'));
::Class.meta.add_attribute('%:methods'          => ::make_attribute('%:methods'));
::Class.meta.add_attribute('%:class_methods'    => ::make_attribute('%:class_methods'));

## Now we make Class conform to the Package interface

::Class.meta.add_method('FETCH' => method () {
    my ($self, $label) = @_;
    (defined $label && $label)
        || confess "Cannot FETCH at (" . ($label || 'undef') . ")";
    if ($label =~ /^\&(.*)$/) {
        # check for instance method
        return $self->has_method($1, for => 'instance') ? 
                    $self->get_method($1, for => 'instance') 
                    :
                    # check for class method
                    $self->has_method($1, for => 'class') ?
                        $self->get_method($1, for => 'class')
                        :
                        # if all else fails, maybe it is 
                        # a sub, so we just  grab it from 
                        # the namespoace stash
                        ::opaque_instance_attr($self => '%:namespace')->{$label};
    }   
    # XXX -
    # this reg-exp is probably not correct ...
    elsif ($label =~ /^.(\.|\:).*$/) {
        # check for instance attribute
        return $self->has_attribute($label, for => 'instance') ?
                    $self->get_attribute($label, for => 'instance')
                    :
                    # class attributes are really just package 
                    # variables with an "our" scope... so we 
                    # just grab it from the namespoace stash
                    ::opaque_instance_attr($self => '%:namespace')->{$label};
    } 
    else {        
        # XXX -
        # we need to duplicate the ::Package code here
        # because calling next_METHOD here can be 
        # problematic since ::Class does not dispatch
        # in the same way other classes do (this should
        # be fixed at some point though, becuase it is 
        # actually wrong)
        ::opaque_instance_attr($self => '%:namespace')->{$label};
    }    
});

::Class.meta.add_method('STORE' => method () {
    my ($self, $label, $value) = @_;
    (defined $label && $label)
        || confess "Cannot STORE at (" . ($label || 'undef') . ")";    
    # only store method objects,.. regular subs go in the namespace
    if ($label =~ /^\&(.*)$/ && (blessed($value) && $value->isa('Perl6::Method'))) {
        return $self->add_method($1, $value);
    }  
    # XXX -
    # this reg-exp is probably not correct ...     
    elsif ($label =~ /^.(\.|\:).*$/ && (blessed($value) && $value->isa('Perl6::Attribute'))) {
        # only store instance attributes with the meta model, 
        # class attributes are just package scoped "our" variables
        # so they are added to the Package normally
        return $self->add_attribute($label, $value);
    } 
    else {
        # XXX -
        # we need to duplicate the ::Package code here
        # because calling next_METHOD here can be 
        # problematic since ::Class does not dispatch
        # in the same way other classes do (this should
        # be fixed at some point though, becuase it is 
        # actually wrong)
        if ($label =~ /^\&/ && ref($value) eq 'CODE') {
            $value = ::wrap_package_sub($value, $self);
        } 
        ::opaque_instance_attr($self => '%:namespace')->{$label} = $value;
    }
});

1;

__END__

=pod

=head1 NAME

metamorph

=head1 DESCRIPTION

=head1 AUTHORS

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut
