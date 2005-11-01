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
    my @classes = ($inv);
    # we take the MRO first, however at some early
    # stages of the bootstrap, this is not yet 
    # populated with anything, so ....
    my $supers = $inv->get_attr(symbol->new('@:MRO'));
    # if nothing is in MRO, we take the superclasses
    # because we know that is there ...
    $supers = $inv->get_attr(symbol->new('@:superclasses')) 
        if $supers->length->equal_to(num->new(0)) == $bit::TRUE;    
        
    push @classes => $supers->to_native;    
        
    foreach my $class (@classes) {
        my $methods = $inv->get_attr(symbol->new('%:methods'));
        return $methods->fetch($label)->do($args) 
            if $methods->exists($label);             
    }
    confess "Method ($label) not found in \$::Class";             
}

$::ENV = Perl6::Runtime::get_top_level_env();

## ----------------------------------------------------------------------------
## some metamodel tools

=pod

sub *WALKMETH (closure &dispatcher, symbol $label, hash %opts) returns (method | nil) {
    while (my $current = &dispatcher.()) {
        return $current.get_method($label, %opts)
            if $current.has_method($label, %opts);
    }
}

=cut

$::ENV->create('WALKMETH' => closure->new(
        $::ENV,
        closure::params->new(
            symbol->new('&dispatcher' => 'closure'),
            symbol->new('$label'      => 'symbol'),
            symbol->new('%opts'       => 'hash'),                        
        ),
        sub {
            my $e = shift;
            my $dispatcher = $e->get('&dispatcher');
            my $label      = $e->get('$label');
            my $opts       = $e->get('%opts');  
            
            my $NIL     = nil->new();
            my $current = $dispatcher->do();                                  
            while ($current != $NIL) {
                if ($current->send('has_method' => ($label, $opts))) {
                    return $current->send('get_method' => ($label, $opts));
                }
                $current = $dispatcher->do();                                                  
            }
            return $NIL;        
        }
    )
);

=pod

sub *WALKCLASS (closure &dispatcher) returns opaque {
    &dispatcher.()
}

=cut

$::ENV->create('WALKCLASS' => closure->new(
        $::ENV,
        closure::params->new(
            symbol->new('&dispatcher' => 'closure')
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
$::Class->change_class(reference->new($::Class));

=pod

method add_method (opaque $self: str $label, method $method) returns nil {
    %:methods{$label} = $method;
}

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

method has_method (opaque $self: str $label) returns bit { 
    %:methods.exists($label);
}

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

method new (opaque $class: hash %params) returns opaque {
    $class.bless(undef, %params);
}

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('new'),
    # method body
        method->new(
            $::ENV,
            closure::params->new(
                symbol->new('$class:' => 'opaque'), 
                symbol->new('%params' => 'hash')
            ),
            sub {
                my $e      = shift;
                my $class  = $e->get('$class:');
                my $params = $e->get('%params');
                return $class->send('bless' => (nil->new(), $params));    
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
                $canidate = str->new('P6opaque') if $canidate == $nil::NIL; 
                
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
                        # NOTE: 
                        # store them all as NIL for now
                        $attrs->store($attr => nil->new());
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
    for WALKMETH($dispatcher, 'BUILDALL') -> $method {
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
                my $method = $WALKMETH->do($dispatcher, symbol->new('BUILD'), hash->new());
                
                while ($method != $nil::NIL) { 
                    $method->do(list->new($self, $params));                  
                    $method = $WALKMETH->do($dispatcher, symbol->new('BUILD'), hash->new());
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
                    $self->set_attr($symbol => $params->fetch($key))
                        if $self->class->send('find_attribute_spec' => ($symbol));
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
                my $method = $WALKMETH->do($dispatcher, symbol->new('DESTROY'), hash->new());

                while ($method != $nil::NIL) { 
                    $method->do(list->new($self));                  
                    $method = $WALKMETH->do($dispatcher, symbol->new('DESTROY'), hash->new());
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

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('superclasses'),
    # method body
        method->new(
            $::ENV,
            closure::params->new(
                symbol->new('$self:'        => 'opaque'),                
                symbol->new('@superclasses' => 'list'),                                
            ),
            sub {
                my $e            = shift;
                my $self         = $e->get('$self:');
                my $superclasses = $e->get('@superclasses');
                
                if ($superclasses != $nil::NIL) {
                    foreach my $super ($superclasses->get_native) {
                        $super->add_subclass($self);
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
                my $seqs = $e->get('@segs');
                
                # convert this to native
                my @seqs = $seqs->to_native;
                
                my @res;
                while (1) {
                    # remove all empty seqences
                    my @nonemptyseqs = (map { (@{$_} ? $_ : ()) } @seqs);
                    # return the list if we have no more no-empty sequences
                    return list->new(@res) if not @nonemptyseqs; 
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
                        [ $self ],                                         # the class we are linearizing
                        (map { [ $_->send('MRO')->to_native ] } @supers ), # the MRO of all the superclasses
                        [ @supers ]                                        # a list of all the superclasses                        
                    ));
                    $self->set_attr(symbol->new('@:MRO') => $MRO);
                }
                return $self->get_attr(symbol->new('@:MRO'));                
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
