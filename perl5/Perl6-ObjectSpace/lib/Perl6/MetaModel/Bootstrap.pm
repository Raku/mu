#!/usr/bin/perl

use strict;
use warnings;

use Perl6::Runtime;

use Perl6::Core::Nil;
use Perl6::Core::Str;
use Perl6::Core::Num;
use Perl6::Core::Ref;
use Perl6::Core::Hash;
use Perl6::Core::List;
use Perl6::Core::Block;

use Perl6::MM::Opaque;
use Perl6::MM::Method;
use Perl6::MM::Attribute;

use Carp 'confess';
use Scalar::Util 'blessed';

# create our basic variables/functions for 
# the metamodel that the runtime needs to 
# have available ...
$::Class   = undef;
$::Object  = undef;
$::Module  = undef;
$::Package = undef;

# for sending messages to an opaque object 
# (very primitive at the moment)
sub opaque::send {
    my ($inv, $label, @args) = @_;
    #warn "calling send for $label";
    
    $label = str->new($label) 
        unless (blessed($label) && $label->isa('str'));
    
    my $args;
    if (blessed($args[0]) && $args[0]->isa('list')) {
        $args[0]->unshift($inv);
        $args = $args[0];
    }
    else {
        $args = list->new($inv, @args);
    }
    
    # gather all the classes to look through
    my @classes = ($inv->class);
    # we take the MRO first, however at some early
    # stages of the bootstrap, this is not yet 
    # populated with anything, so ....
    my $supers = $inv->get_attr(symbol->new('@:MRO'));
    # if nothing is in MRO, we take the superclasses
    # because we know that is there ...
    $supers = $inv->get_attr(symbol->new('@:superclasses')) 
        if $supers->is_empty == $bit::TRUE;    
        
    push @classes => $supers->to_native;    
        
    foreach my $class (@classes) {
        my $methods = $class->get_attr(symbol->new('%:methods'));
        return $methods->fetch($label)->do($args) 
            if $methods->exists($label) == $bit::TRUE;             
    }
    confess "Method (" . $label->to_native . ") not found in \$::Class " . $inv->id->to_native;             
}

$::ENV = Perl6::Runtime::get_top_level_env();

## ----------------------------------------------------------------------------
## some metamodel tools

=pod

sub *WALKMETH (block &dispatcher, symbol $label, hash %opts) returns (method | nil) {
    while (my $current = &dispatcher.()) {
        return $current.get_method($label, %opts)
            if $current.has_method($label, %opts);
    }
}


(closure *WALKMETH ((block &dispatcher) (symbol $label) (hash ?%opts))
    ((= ($current (&dispatcher do)))
        (while (!= ($current NIL)) 
            ((($current send (has_method $label ?%opts)) and 
                 (return ($current send (get_method $label ?%opts))))
             (= ($current (&dispatcher do)))))
        (NIL)))

=cut

$::ENV->create('WALKMETH' => closure->new(
        $::ENV,
        closure::params->new(
            symbol->new('&dispatcher' => 'block'),
            symbol->new('$label'      => 'symbol'),
            symbol->new('?%opts'      => 'hash'),                        
        ),
        sub {
            my $e = shift;
            my $dispatcher = $e->get('&dispatcher');
            my $label      = $e->get('$label');
            my $opts       = $e->get('?%opts');  
            
            my $current = $dispatcher->do();                                  
            while ($current != $nil::NIL) {
                if ($current->send('has_method' => $label) == $bit::TRUE) {
                    return $current->send('get_method' => $label);
                }
                $current = $dispatcher->do();                                                  
            }
            return $nil::NIL;        
        }
    )
);

=pod

sub *WALKCLASS (block &dispatcher) returns opaque {
    &dispatcher.()
}

(closure *WALKCLASS ((block &dispatcher))
    (&dispatcher do)

=cut

$::ENV->create('WALKCLASS' => closure->new(
        $::ENV,
        closure::params->new(
            symbol->new('&dispatcher' => 'block')
        ),
        sub {
            my $e = shift;
            my $dispatcher = $e->get('&dispatcher');
            return $dispatcher->do();        
        }
    )
);

## ----------------------------------------------------------------------------
## now begin creating the metamodel

=pod

class Class {
    has @:MRO;
    has @:subclasses;
    has @:superclasses;
    has %:private_methods;
    has %:attributes;
    has %:methods;
}

=cut

# create the basic Class
$::Class = opaque->new(
        reference->new($opaque::NULL_OBJECT),
        hash->new(
            attribute->new('@:MRO')             => list->new(),
            attribute->new('@:subclasses')      => list->new(),
            attribute->new('@:superclasses')    => list->new(),
            attribute->new('%:private_methods') => hash->new(),
            attribute->new('%:attributes')      => hash->new(),
            attribute->new('%:methods')         => hash->new(),                     
        )
    );
    
# link Class back to Class
$::Class->change_class($::Class);

=pod

method add_method (opaque $self: symbol $label, method $method) returns nil {
    %:methods{$label} = $method;
}

(method add_method ((opaque $self:) (symbol $label) (method $method))
    ((($self: get_attr (symbol new (%:methods))) store ($label $method))))

=cut

{
    # add the first method
    my $_add_method = method->new(
            $::ENV,
            closure::params->new(
                symbol->new('$self:'  => 'opaque'), 
                symbol->new('$label'  => 'symbol'), 
                symbol->new('$method' => 'method')
            ),
            sub {
                my $e      = shift;
                my $self   = $e->get('$self:');
                my $label  = $e->get('$label');
                my $method = $e->get('$method');
                $self->get_attr(symbol->new('%:methods'))->store($label, $method);
            }
        );

    # and use it to add itself to the $::Class
    $_add_method->do(list->new($::Class, symbol->new('add_method'), $_add_method));
}

=pod

method has_method (opaque $self: symbol $label) returns bit { 
    %:methods.exists($label);
}

(method has_method ((opaque $self:) (symbol $label))
    ((($self: get_attr (symbol new (%:methods))) exists ($label))))

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('has_method'), 
    # method body
        method->new(
            $::ENV,
            closure::params->new(
                symbol->new('$self:' => 'opaque'), 
                symbol->new('$label' => 'symbol')
            ),
            sub {
                my $e      = shift;
                my $self   = $e->get('$self:');
                my $label  = $e->get('$label');  
                $self->get_attr(symbol->new('%:methods'))->exists($label);                                                  
            }
        )
    )
);

=pod

method get_method (opaque $self: symbol $label) returns method { 
    %:methods{$label};
}

(method get_method ((opaque $self:) (symbol $label))
    ((($self: get_attr (symbol new (%:methods))) fetch ($label))))

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('get_method'), 
    # method body
        method->new(
            $::ENV,
            closure::params->new(
                symbol->new('$self:' => 'opaque'), 
                symbol->new('$label' => 'symbol')
            ),
            sub {
                my $e      = shift;
                my $self   = $e->get('$self:');
                my $label  = $e->get('$label');  
                $self->get_attr(symbol->new('%:methods'))->fetch($label);                                                  
            }
        )
    )
);

=pod

method remove_method (opaque $self: symbol $label) returns list { 
    %:methods.delete($label);
}

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('remove_method'), 
    # method body
        method->new(
            $::ENV,
            closure::params->new(
                symbol->new('$self:' => 'opaque'),
                symbol->new('$label' => 'symbol')                
            ),
            sub {
                my $e      = shift;
                my $self   = $e->get('$self:');
                my $label  = $e->get('$label');                 
                $self->get_attr(symbol->new('%:methods'))->remove($label);                                                  
            }
        )
    )
);

=pod

method get_method_list (opaque $self:) returns list { 
    %:methods.keys;
}

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('get_method_list'), 
    # method body
        method->new(
            $::ENV,
            closure::params->new(
                symbol->new('$self:' => 'opaque')
            ),
            sub {
                my $e      = shift;
                my $self   = $e->get('$self:');
                $self->get_attr(symbol->new('%:methods'))->keys();                                                  
            }
        )
    )
);

=pod

method new (opaque $class: hash %params) returns opaque {
    $class.bless(undef, %params);
}

(method new ((opaque $class:) (hash %params)) 
    ($class: send (bless NIL $params)))

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('new'),
    # method body
        method->new(
            $::ENV,
            closure::params->new(
                symbol->new('$class:' => 'opaque'), 
                symbol->new('?%params' => 'hash')
            ),
            sub {
                my $e      = shift;
                my $class  = $e->get('$class:');
                my $params = $e->get('?%params');
                $params = hash->new() if $params == $nil::NIL;
                return $class->send('bless' => (str->new(), $params));    
            }
        )
    )
);

=pod

method bless (opaque $class: str $canidate, hash %params) returns opaque {
    $canidate //= 'P6opaque';
    my $self = $class.CREATE(repr => $canidate, %params);
    $self.BUILDALL($params);
    return $self;
}

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('bless'),
    # method body
        method->new(
            $::ENV,
            closure::params->new(
                symbol->new('$class:'   => 'opaque'), 
                symbol->new('$canidate' => 'str'), 
                symbol->new('%params'   => 'hash')
            ),
            sub {
                my $e        = shift;
                my $class    = $e->get('$class:');
                my $canidate = $e->get('$canidate');                
                my $params   = $e->get('%params');

                # p6opaque is our default
                $canidate = str->new('P6opaque') if $canidate->to_bit == $bit::FALSE; 
                
                # create and init the object
                my $self = $class->send('CREATE' => ($canidate, $params));
                $self->send('BUILDALL' => ($params));
                return $self;                  
            }
        )
    )
);

=pod

method CREATE (opaque $class: str $repr, hash %params) returns opaque {
    my %attrs;
    my $dispatcher = $class.dispatcher(:descendant);
    for WALKCLASS($dispatcher) -> $c {
        for $c->get_attribute_list() -> $attr {
            %attrs{$attr->name} = $attr.instantiate_container();
        }
    }
    my $self = opaque.new(\$class, %attrs);
    return $self;
}

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('CREATE'),
    # method body
        method->new(
            $::ENV,
            closure::params->new(
                symbol->new('$class:' => 'opaque'), 
                symbol->new('$repr'   => 'str'), 
                symbol->new('%params' => 'hash')
            ),
            sub {
                my $e      = shift;
                my $class  = $e->get('$class:');
                my $repr   = $e->get('$repr');   # ignore this for now             
                my $params = $e->get('%params');

                my $attrs = hash->new();
                
                my $dispatcher = $class->send('dispatcher' => (symbol->new(':descendant')));
                
                my $WALKCLASS = $e->get('WALKCLASS');
                my $c = $WALKCLASS->do(list->new($dispatcher));
                
                while ($c != $nil::NIL) {
                    # get_attribute_list returns a list of symbol
                    foreach my $attr ($c->send('get_attribute_list')->to_native) { 
                        my $attr_obj = $c->send('get_attribute', symbol->new($attr->to_native));
                        $attrs->store($attr => $attr_obj->instantiate_container);
                    }
                    $c = $WALKCLASS->do(list->new($dispatcher));
                }

                my $self = opaque->new(reference->new($class), $attrs);     
                # and now return it ...
                return $self;            
            }
        )
    )
);

=pod

method BUILDALL (opaque $self: hash %params) returns nil {
    my $dispatcher = $self.dispatcher(:descendant);
    for WALKMETH($dispatcher, 'BUILD') -> $method {
        $method.($self, %params);
    }
}

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('BUILDALL'),
    # method body
        method->new(
            $::ENV,
            closure::params->new(
                symbol->new('$self:' => 'opaque'), 
                symbol->new('%params' => 'hash')
            ),
            sub {
                my $e      = shift;
                my $self   = $e->get('$self:');
                my $params = $e->get('%params');
                
                my $dispatcher = $self->class->send('dispatcher' => symbol->new(':descendant'));
                my $WALKMETH = $e->get('WALKMETH');
                my $method = $WALKMETH->do(list->new($dispatcher, symbol->new('BUILD')));
                
                while ($method != $nil::NIL) { 
                    $method->do(list->new($self, $params));                  
                    $method = $WALKMETH->do(list->new($dispatcher, symbol->new('BUILD')));
                }
                
                return $nil::NIL;                 
            }
        )
    )
);

=pod

method BUILD (opaque $self: hash %params) returns nil {
    for %params.kv -> $key, $value {
        # not sure if this is the 
        # correct syntax for fetching
        # an attribute symbolically
        $.($key) = $value if $.($key);
    }
}

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('BUILD'),
    # method body
        method->new(
            $::ENV,
            closure::params->new(
                symbol->new('$self:' => 'opaque'), 
                symbol->new('%params' => 'hash')
            ),
            sub {
                my $e      = shift;
                my $self   = $e->get('$self:');
                my $params = $e->get('%params');
                foreach my $key ($params->keys->to_native) {
                    my $symbol = symbol->new($key->to_native);
                    $self->set_attr($symbol => $params->fetch($key));
                        #if $self->class->send('find_attribute_spec' => ($symbol));
                } 
                return $nil::NIL;               
            }
        )
    )
);
                
=pod

method DESTROYALL (opaque $self:) returns nil {
    my $dispatcher = $self.dispatcher(:descendant);
    for WALKMETH($dispatcher, 'DESTROYALL') -> $method {
        $method.($self);
    }    
}

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('DESTROYALL'),
    # method body
        method->new(
            $::ENV,
            closure::params->new(
                symbol->new('$self:' => 'opaque'),                
            ),
            sub {
                my $e    = shift;
                my $self = $e->get('$self:');
                                
                my $dispatcher = $self->class->send('dispatcher' => (symbol->new(':ascendant')));
                
                my $WALKMETH = $e->get('WALKMETH');
                my $method = $WALKMETH->do(list->new($dispatcher, symbol->new('DESTROY')));

                while ($method != $nil::NIL) { 
                    $method->do(list->new($self));                  
                    $method = $WALKMETH->do(list->new($dispatcher, symbol->new('DESTROY')));
                }       
                
                return $nil::NIL;                                            
            }
        )
    )
);

=pod

method id (opaque $self:) returns num {
    # speculative syntax here :)
    unbox($self).id
}

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('id'),
    # method body
        method->new(
            $::ENV,
            closure::params->new(
                symbol->new('$self:' => 'opaque'),                
            ),
            sub {
                my $e    = shift;
                my $self = $e->get('$self:');
                return $self->id;
            }
        )
    )
);

=pod

method class (opaque $self:) returns opaque {
    # speculative syntax here :)
    unbox($self).class
}

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('class'),
    # method body
        method->new(
            $::ENV,
            closure::params->new(
                symbol->new('$self:' => 'opaque'),                
            ),
            sub {
                my $e    = shift;
                my $self = $e->get('$self:');
                return $self->class;                
            }
        )
    )
);

=pod

method superclasses (opaque $self: list ?@superclasses) returns list {
    if (@superclasses.defined) {
        for @superclasses -> $super {
            $super.add_subclass($self);
        }
        @:superclasses = @superclasses;
        @:MRO = ();
        $self.MRO();
    }
    return @:superclasses;
}

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('superclasses'),
    # method body
        method->new(
            $::ENV,
            closure::params->new(
                symbol->new('$self:'         => 'opaque'),                
                symbol->new('?@superclasses' => 'list'),                                
            ),
            sub {
                my $e            = shift;
                my $self         = $e->get('$self:');
                my $superclasses = $e->get('?@superclasses');
                
                if ($superclasses != $nil::NIL) {
                    foreach my $super ($superclasses->to_native) {
                        $super->send('add_subclass' => ($self));
                    }
                    $self->set_attr(symbol->new('@:superclasses') => $superclasses); 
                    # clear the MRO now
                    $self->set_attr(symbol->new('@:MRO') => list->new()); 
                    # and recalculate it ..
                    $self->send('MRO');
                }
                $self->get_attr(symbol->new('@:superclasses'));                
            }
        )
    )
);

=pod

method subclasses (opaque $self:) returns list {
    @:subclasses;
}

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('subclasses'),
    # method body
        method->new(
            $::ENV,
            closure::params->new(
                symbol->new('$self:' => 'opaque'),                
            ),
            sub {
                my $e    = shift;
                my $self = $e->get('$self:');
                $self->get_attr(symbol->new('@:subclasses'));                
            }
        )
    )
);

=pod

method add_subclass (opaque $self: opaque $subclass) returns nil {
    @:subclasses.push($subclass);
}

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('add_subclass'),
    # method body
        method->new(
            $::ENV,
            closure::params->new(
                symbol->new('$self:'    => 'opaque'),
                symbol->new('$subclass' => 'opaque'),                                
            ),
            sub {
                my $e        = shift;
                my $self     = $e->get('$self:');
                my $subclass = $e->get('$subclass');                
                $self->get_attr(symbol->new('@:subclasses'))->push($subclass);                  
            }
        )
    )
);

=pod

method _merge (opaque $self: list @seqs) returns list {
    ...
}

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('_merge'),
    # method body
        method->new(
            $::ENV,
            closure::params->new(
                symbol->new('$self:' => 'opaque'),                
                symbol->new('@seqs'  => 'list'),                                
            ),
            sub {
                my $e    = shift;
                my $self = $e->get('$self:');
                my $seqs = $e->get('@seqs');
                
                # convert this to native
                my @seqs = $seqs->to_native;
                
                my @res;
                while (1) {
                    # remove all empty seqences
                    my @nonemptyseqs = (map { (($_->is_empty == $bit::FALSE) ? $_ : ()) } @seqs);
                    # return the list if we have no more no-empty sequences
                    return list->new(@res) if not @nonemptyseqs; 
                    my $cand; # a canidate ..
                    foreach my $seq (@nonemptyseqs) {
                        $cand = $seq->fetch(num->new(0)); # get the head of the list
                        my $nothead;            
                        foreach my $sub_seq (@nonemptyseqs) {
                            # XXX - this is instead of the python "in"
                            my %in_tail = (map { $_ => 1 } $sub_seq->tail->to_native);
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
                        $seq->shift if $seq->fetch(num->new(0)) == $cand;
                    }
                }                
            }
        )
    )
);

=pod

method MRO (opaque $self:) returns list {
    unless (@:MRO) {
        $self._merge(
            [ $self ],
            @:superclasses.map:{ [ $_.MRO() ] },
            [ @:superclasses ]
        );
    }
    @:MRO;
}

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('MRO'),
    # method body
        method->new(
            $::ENV,
            closure::params->new(
                symbol->new('$self:' => 'opaque'),                
            ),
            sub {
                my $e    = shift;
                my $self = $e->get('$self:');
                
                # if it is empty ... 
                if ($self->get_attr(symbol->new('@:MRO'))->is_empty == $bit::TRUE) {
                    my @supers = $self->send('superclasses')->to_native;
                    my $MRO = $self->send('_merge' => list->new(
                        list->new(
                            list->new($self),                   # the class we are linearizing
                            (map { $_->send('MRO') } @supers ), # the MRO of all the superclasses
                            list->new(@supers)                  # a list of all the superclasses                        
                        )
                    ));
                    $self->set_attr(symbol->new('@:MRO') => $MRO);
                }
                return $self->get_attr(symbol->new('@:MRO'));                
            }
        )
    )
);

=pod

method dispatcher (opaque $self: symbol $order) returns block {
    $order = ':ascendant' 
        if !~$order || $order == ':canonical';
    ...
}

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('dispatcher'),
    # method body
        method->new(
            $::ENV,
            closure::params->new(
                symbol->new('$self:'  => 'opaque'),                
                symbol->new('?$order' => 'symbol'),                                
            ),
            sub {
                my $e     = shift;
                my $self  = $e->get('$self:');
                my $order = $e->get('?$order');
                
                $order = str->new(':ascendant') # C3 is the canonical order
                    if $order == $nil::NIL || $order->equal_to(str->new(':canonical'));                
                
                my $dispatcher;
                if ($order->equal_to(str->new(':descendant')) == $bit::TRUE) {
                    $dispatcher = $self->send('_make_descendant_dispatcher');
                }    
                elsif ($order->equal_to(str->new(':ascendant')) == $bit::TRUE) {
                    $dispatcher = $self->send('_make_ascendant_dispatcher');
                }   
                else {
                    confess 'Unsupported dispatch order ($order)'
                }
                return $dispatcher;                 
            }
        )
    )
);

=pod

method _make_dispatcher_iterator (opaque $self: list @values) returns block {
    my $counter = 0;
    return sub {
        return @values[$counter++];
    };
}

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('_make_dispatcher_iterator'),
    # method body
        method->new(
            $::ENV,
            closure::params->new(
                symbol->new('$self:'  => 'opaque'),                
                symbol->new('@values' => 'list'),                                
            ),
            sub {
                my $e    = shift;
                my $self = $e->get('$self:');
                
                $e->create('$counter' => num->new(0));
                return block->new(
                    $e,
                    sub { 
                        my $local   = shift;
                        my $counter = $local->get('$counter');
                        $local->set('$counter' => $counter->increment);                        
                        return $local->get('@values')->fetch($counter); 
                    }
                );                
            }
        )
    )
);

=pod

method _make_descendant_dispatcher (opaque $self:) returns block {
    $self._make_dispatcher_iterator(@:MRO.reverse);
}

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('_make_descendant_dispatcher'),
    # method body
        method->new(
            $::ENV,
            closure::params->new(
                symbol->new('$self:' => 'opaque'),                
            ),
            sub {
                my $e    = shift;
                my $self = $e->get('$self:');
                
                my $MRO = $self->send('MRO');
                return $self->send('_make_dispatcher_iterator' => list->new($MRO->reverse));                
            }
        )
    )
);

=pod

method _make_ascendant_dispatcher (opaque $self:) returns block {
    $self._make_dispatcher_iterator(@:MRO);
}

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('_make_ascendant_dispatcher'),
    # method body
        method->new(
            $::ENV,
            closure::params->new(
                symbol->new('$self:' => 'opaque'),                
            ),
            sub {
                my $e    = shift;
                my $self = $e->get('$self:');
                
                my $MRO = $self->send('MRO');
                return $self->send('_make_dispatcher_iterator' => list->new($MRO));             
            }
        )
    )
);

=pod

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('is_a'),
    # method body
        method->new(
            $::ENV,
            closure::params->new(
                symbol->new('$self:' => 'opaque'), 
                symbol->new('$class' => 'opaque'),                
            ),
            sub {
                my $e     = shift;
                my $self  = $e->get('$self:');
                my $class = $e->get('$class');                
                
                return bit->new(1) 
                    if $self->id->equal_to($class->id) == $bit::TRUE;
                    
                my $dispatcher = $self->send('dispatcher', symbol->new(':canonical'));
                my $next = $dispatcher->do();
                while ($next != $nil::NIL) {    
                    return bit->new(1) 
                        if $next->id->equal_to($class->id) == $bit::TRUE;
                    $next = $dispatcher->do();
                }
                return bit->new(0);                 
            }
        )
    )
);

=pod

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('add_attribute'),
    # method body
        method->new(
            $::ENV,
            closure::params->new(
                symbol->new('$self:'     => 'opaque'), 
                symbol->new('$label'     => 'symbol'), 
                symbol->new('$attribute' => 'attribute'),                                 
            ),
            sub {
                my $e         = shift;
                my $self      = $e->get('$self:');
                my $label     = $e->get('$label');
                my $attribute = $e->get('$attribute');                                
                
                $self->get_attr(symbol->new('%:attributes'))->store($label => $attribute);                   
            }
        )
    )
);

=pod

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('get_attribute'),
    # method body
        method->new(
            $::ENV,
            closure::params->new(
                symbol->new('$self:' => 'opaque'),  
                symbol->new('$label' => 'symbol'),                                
            ),
            sub {
                my $e     = shift;
                my $self  = $e->get('$self:');
                my $label = $e->get('$label'); 
                $self->get_attr(symbol->new('%:attributes'))->fetch($label);                                 
            }
        )
    )
);


$::Class->send('add_attribute', (symbol->new('@:MRO')             => attribute->new('@:MRO'             => 'list')));
$::Class->send('add_attribute', (symbol->new('@:superclasses')    => attribute->new('@:superclasses'    => 'list')));
$::Class->send('add_attribute', (symbol->new('@:subclasses')      => attribute->new('@:subclasses'      => 'list')));
$::Class->send('add_attribute', (symbol->new('%:private_methods') => attribute->new('%:private_methods' => 'hash')));
$::Class->send('add_attribute', (symbol->new('%:attributes')      => attribute->new('%:attributes'      => 'hash')));
$::Class->send('add_attribute', (symbol->new('%:methods')         => attribute->new('%:methods'         => 'hash')));
                                                                   
=pod

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('has_attribute'),
    # method body
        method->new(
            $::ENV,
            closure::params->new(
                symbol->new('$self:' => 'opaque'),  
                symbol->new('$label' => 'symbol'),                                
            ),
            sub {
                my $e     = shift;
                my $self  = $e->get('$self:');
                my $label = $e->get('$label'); 
                $self->get_attr(symbol->new('%:attributes'))->exists($label);                                 
            }
        )
    )
);

=pod

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('get_attribute_list'),
    # method body
        method->new(
            $::ENV,
            closure::params->new(
                symbol->new('$self:' => 'opaque'),  
            ),
            sub {
                my $e     = shift;
                my $self  = $e->get('$self:');
                $self->get_attr(symbol->new('%:attributes'))->keys();                                 
            }
        )
    )
);

__END__

# METHOD TEMPLATE

=pod

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new(''),
    # method body
        method->new(
            $::ENV,
            closure::params->new(
                symbol->new('$self:' => 'opaque'),                
            ),
            sub {
                my $e    = shift;
                my $self = $e->get('$self:');
            }
        )
    )
);
