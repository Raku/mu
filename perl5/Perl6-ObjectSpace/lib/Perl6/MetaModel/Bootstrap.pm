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
use Perl6::MM::C3;

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
    
    my $args = list->new($inv, @args);
    
    my $class = $inv->class;
    
    # gather all the classes to look through
    my @classes = ($class);
    
    #warn "inv: " . $inv->id->to_native . " => class: " . $inv->class->id->to_native;
    # we take the MRO first, however at some early
    # stages of the bootstrap, this is not yet 
    # populated with anything, so ....
    my $supers = $class->get_attr(symbol->new('@:MRO'));
    # if nothing is in MRO, we take the superclasses
    # because we know that is there ...
    $supers = $class->get_attr(symbol->new('@:superclasses')) 
        if $supers->is_empty == $bit::TRUE;    
        
    push @classes => $supers->to_native;    
        
    foreach my $class (@classes) {
        my $methods = $class->get_attr(symbol->new('%:methods'));
        #warn "looking for method (" . $label->to_native . ")";
        if ($methods->exists($label) == $bit::TRUE) {
            #warn "calling method (" . $label->to_native . ")";
            return $methods->fetch($label)->do($args);
        }
    }
    confess "Method (" . $label->to_native . ") not found in \$::Class " . $class->id->to_native;             
}

$::ENV = Perl6::Runtime::get_top_level_env();

## ----------------------------------------------------------------------------
## some metamodel tools

=pod

sub *WALKMETH (block &dispatcher, symbol $label, hash %opts) returns method {
    while (my $current = &dispatcher.()) {
        return $current.get_method($label, %opts)
            if $current.has_method($label, %opts);
    }
}

# alternate p6 version
sub *WALKMETH (block &dispatcher, symbol $label, hash %opts) returns method {
    { (&dispatcher.() // return).get_method($label, %opts) || redo }       
}



(closure *WALKMETH ((block &dispatcher) (symbol $label) (hash ?%opts)) returns method 
    ((= $method NIL)
     (= $current (&dispatcher do))
     (while (($current is_not_nil) and ($method is_nil))
        (($current send (has_method $label)) and (= $method ($current send (get_method $label)))
         (= $current (&dispatcher do))))
     ($method))

=cut

$::ENV->create('WALKMETH' => closure->new(
        $::ENV,
        closure::signature->new(
            params => closure::params->new(
                           symbol->new('&dispatcher' => 'block'),
                           symbol->new('$label'      => 'symbol'),
                           symbol->new('?%opts'      => 'hash'),                        
                       ),
            returns => 'method'
        ),
        sub {
            my $e = shift;
            $e->create('$method' => $nil::NIL);
            $e->create('$current' => $e->get('&dispatcher')->do());                                  
            block->new($e, sub {
                my $e = shift;
                $e->get('$current')
                  ->send('has_method' => $e->get('$label'))
                  ->and(
                      block->new($e, sub {
                          my $e = shift;
                          $e->set('$method' => $e->get('$current')->send('get_method' => $e->get('$label')));                    
                      })
                  );    
                $e->set('$current' => $e->get('&dispatcher')->do());
            })->do_while(
                block->new($e, sub {
                    my $e = shift;
                    $e->get('$current')
                      ->is_not_nil
                      ->and(
                          block->new($e, sub {
                              my $e = shift;
                              $e->get('$method')->is_nil
                          })
                      ); 
                })
            );
            return $e->get('$method');        
        }
    )
);

=pod

sub *WALKCLASS (block &dispatcher) returns opaque {
    &dispatcher.()
}

(closure *WALKCLASS ((block &dispatcher)) returns opaque
    (&dispatcher do)

=cut

$::ENV->create('WALKCLASS' => closure->new(
        $::ENV,
        closure::signature->new(
            params  => closure::params->new(symbol->new('&dispatcher' => 'block')),
            returns => 'opaque' 
        ),
        sub {
            my $e = shift;
            return $e->get('&dispatcher')->do();        
        }
    )
);

## ----------------------------------------------------------------------------
## now begin creating the metamodel, starting with Class

=pod

class Class {
    has @:MRO;
    has @:subclasses;
    has @:superclasses;
    has %:private_methods;
    has %:attributes;
    has %:methods;
}

(class (Class)
    ((attribute new (@:MRO))             (list)
     (attribute new (@:subclasses))      (list)
     (attribute new (@:superclasses))    (list)
     (attribute new (%:private_methods)) (hash)
     (attribute new (%:attributes))      (hash)
     (attribute new (%:methods))         (hash)))    

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

(method add_method ((opaque $self:) (symbol $label) (method $method)) returns nil
    ((($self: get_attr (symbol new (%:methods))) store ($label $method))))

=cut

{
    # add the first method
    my $_add_method = method->new(
            $::ENV,
            closure::signature->new(
                params => closure::params->new(
                              symbol->new('$self:'  => 'opaque'), 
                              symbol->new('$label'  => 'symbol'), 
                              symbol->new('$method' => 'method')
                          ),
                returns => 'nil'
            ),
            sub {
                my $e = shift;
                $e->get('$self:')
                  ->get_attr(symbol->new('%:methods'))
                  ->store(
                      $e->get('$label'), 
                      $e->get('$method')
                  );
            }
        );

    # and use it to add itself to the $::Class
    $_add_method->do(list->new($::Class, symbol->new('add_method'), $_add_method));
}

=pod

method has_method (opaque $self: symbol $label) returns bit { 
    %:methods.exists($label);
}

(method has_method ((opaque $self:) (symbol $label)) returns bit
    ((($self: get_attr (symbol new (%:methods))) exists ($label))))

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('has_method'), 
    # method body
        method->new(
            $::ENV,
            closure::signature->new(
                params => closure::params->new(
                              symbol->new('$self:' => 'opaque'), 
                              symbol->new('$label' => 'symbol')
                          ),
                returns => 'bit'
            ),
            sub {
                my $e = shift;
                $e->get('$self:')
                  ->get_attr(symbol->new('%:methods'))
                  ->exists($e->get('$label'));                                                  
            }
        )
    )
);

=pod

method get_method (opaque $self: symbol $label) returns method { 
    %:methods{$label};
}


(method get_method ((opaque $self:) (symbol $label)) returns method
    ((($self: get_attr (symbol new (%:methods))) fetch ($label))))

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('get_method'), 
    # method body
        method->new(
            $::ENV,
            closure::signature->new(
                params => closure::params->new(
                              symbol->new('$self:' => 'opaque'), 
                              symbol->new('$label' => 'symbol')
                          ),
                returns => 'method'
            ),
            sub {
                my $e = shift;
                $e->get('$self:')
                  ->get_attr(symbol->new('%:methods'))
                  ->fetch($e->get('$label'));                                                  
            }
        )
    )
);

=pod

method remove_method (opaque $self: symbol $label) returns nil { 
    %:methods.delete($label);
}

(method remove_method ((opaque $self:) (symbol $label)) returns nil
    ((($self: get_attr (symbol new (%:methods))) delete ($label))))

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('remove_method'), 
    # method body
        method->new(
            $::ENV,
            closure::signature->new(
                params => closure::params->new(
                              symbol->new('$self:' => 'opaque'),
                              symbol->new('$label' => 'symbol')                
                          ),
                returns => 'nil'
            ),
            sub {
                my $e = shift;               
                $e->get('$self:')
                  ->get_attr(symbol->new('%:methods'))
                  ->remove($e->get('$label'));                                                  
            }
        )
    )
);

=pod

method get_method_list (opaque $self:) returns list { 
    %:methods.keys;
}

(method get_method_list ((opaque $self:)) returns list
    ((($self: get_attr (symbol new (%:methods))) keys)))

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('get_method_list'), 
    # method body
        method->new(
            $::ENV,
            closure::signature->new(
                params  => closure::params->new(symbol->new('$self:' => 'opaque')),
                returns => 'list'
            ),
            sub {
                my $e = shift;
                $e->get('$self:')
                  ->get_attr(symbol->new('%:methods'))
                  ->keys();                                                  
            }
        )
    )
);

=pod

method new (opaque $class: hash %params) returns opaque {
    $class.bless(undef, %params // hash());
}

(method new ((opaque $class:) (hash ?%params)) returns opaque
    (((?%params is_nil) and (= ?%params (hash new)))
     ($class: send (bless (NIL ?%params)))))

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('new'),
    # method body
        method->new(
            $::ENV,
            closure::signature->new(
                params => closure::params->new(
                              symbol->new('$class:' => 'opaque'), 
                              symbol->new('?%params' => 'hash')
                          ),
                returns => 'opaque'
            ),
            sub {
                my $e = shift;
                $e->get('?%params')
                  ->is_nil
                  ->and(block->new($e, sub {
                      my $e = shift;
                      $e->set('?%params' => hash->new()); 
                  }));
                $e->get('$class:')->send('bless' => (str->new(), $e->get('?%params')));    
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

(method bless ((opaque $class:) (str $canidate) (hash %params)) returns opaque
    ((($canidate to_bit) or (= $canidate (str new ('P6opaque'))))
     (= ($self ($class: send (CREATE ($canidate %params)))))
     ($self send (BUILDALL %params))
     ($self)))

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('bless'),
    # method body
        method->new(
            $::ENV,
            closure::signature->new(
                params => closure::params->new(
                              symbol->new('$class:'   => 'opaque'), 
                              symbol->new('$canidate' => 'str'), 
                              symbol->new('%params'   => 'hash')
                          ),
                returns => 'opaque'
            ),            
            sub {
                my $e = shift;
                # p6opaque is our default
                $e->get('$canidate')
                  ->to_bit
                  ->or(block->new($e, sub {
                      my $e = shift;                      
                      $e->set('$canidate' => str->new('P6opaque'))
                  })); 
                # create and init the object
                $e->create('$self' => $e->get('$class:')->send('CREATE' => ($e->get('$canidate'), $e->get('%params'))));
                $e->get('$self')->send('BUILDALL' => ($e->get('%params')));
                return $e->get('$self');                  
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

method CREATE (opaque $class: str $repr, hash %params) returns opaque {
    my %attrs = hash();
    my &dispatcher = $class.dispatcher(:descendent);
    my $c = WALKCLASS(&dispatcher);
    while (!$c.defined) {
        $c.get_attributes().map:{
            %attrs{$_.meta.name()} = $_.meta.instantite_container();
        };
        $c = WALKCLASS(&dispatcher);    
    }
    return opaque.new(\$class, %attrs)
}

(method CREATE ((opaque $class:) (str $repr) (hash %params)) returns opaque
    ((= %attrs (hash new))
     (= &dispatcher ($class: send (dispatcher :descendent)))
     (= $c (WALKCLASS (&dispatcher)))
     (while ($c is_not_nil)
        ((($c send (get_attributes)) apply 
            ((= %attrs store (($_ name) ($_ instantite_container)))
             (= $c (WALKCLASS (&dispatcher)))))))
     (opaque new (\$class: %attrs)))

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('CREATE'),
    # method body
        method->new(
            $::ENV,
            closure::signature->new(
                params => closure::params->new(
                              symbol->new('$class:' => 'opaque'), 
                              symbol->new('$repr'   => 'str'), 
                              symbol->new('%params' => 'hash')
                          ),
                returns => 'opaque'
            ),               
            sub {
                my $e = shift;
                $e->create('$attrs' => hash->new());
                $e->create('&dispatcher' => $e->get('$class:')->send('dispatcher' => (symbol->new(':descendant'))));
                $e->create('$c' => $e->get('WALKCLASS')->do(list->new($e->get('&dispatcher'))));            
                block->new($e, sub {
                    my $e = shift;
                    $e->get('$c')
                      ->send('get_attributes')
                      ->apply(
                          block->new($e, sub {
                                my $e = shift;
                                $e->get('$attrs')
                                  ->store(
                                      $e->get('$_')->name => $e->get('$_')->instantiate_container
                                  );
                          })
                      );
                    $e->set('$c' => $e->get('WALKCLASS')->do(list->new($e->get('&dispatcher'))));
                })->do_while(
                    block->new($e, sub {
                        my $e = shift;
                        $e->get('$c')->is_not_nil;
                    })
                );
                return opaque->new(reference->new($e->get('$class:')), $e->get('$attrs'));     
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

(method BUILDALL ((opaque $self:) (hash %params)) returns nil
    ((= &dispatcher (($self: class) send (dispatcher :descendant)))
     (= $method (WALKMETH (&dispatcher (symbol new BUILD))))
     (while ($method is_not_nil) 
        ($method do ($self %params))
        (= $method (WALKMETH (&dispatcher (symbol new BUILD)))))))

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('BUILDALL'),
    # method body
        method->new(
            $::ENV,
            closure::signature->new(
                params => closure::params->new(
                              symbol->new('$self:'  => 'opaque'), 
                              symbol->new('%params' => 'hash')
                          ),
                returns => 'nil'
            ),   
            sub {
                my $e = shift;
                $e->create('&dispatcher' => $e->get('$self:')->class->send('dispatcher' => symbol->new(':descendant')));
                $e->create('$method' => $e->get('WALKMETH')->do(list->new($e->get('&dispatcher'), symbol->new('BUILD'))));
                block->new($e, sub {
                    my $e = shift;
                    $e->get('$method')->do(list->new($e->get('$self:'), $e->get('%params')));                  
                    $e->set('$method' => $e->get('WALKMETH')->do(list->new($e->get('&dispatcher'), symbol->new('BUILD'))));
                })->do_while(
                    block->new($e, sub {
                        my $e = shift;
                        $e->get('$method')->is_not_nil;
                    })
                );
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

(method BUILD ((opaque $self:) (hash %params)) returns nil 
    ((%params keys) apply 
        ($self: set_attrs ((symbol new ($_)) (%params fetch ($_)))))
    (nil))

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('BUILD'),
    # method body
        method->new(
            $::ENV,
            closure::signature->new(
                params => closure::params->new(
                              symbol->new('$self:'  => 'opaque'), 
                              symbol->new('%params' => 'hash')
                          ),
                returns => 'nil'
            ), 
            sub {
                my $e = shift;
                $e->get('%params')
                  ->keys
                  ->apply(
                    block->new($e, sub {
                        my $e = shift;                        
                        $e->get('$self:')
                          ->set_attr(
                              symbol->new($e->get('$_')->to_native), 
                              $e->get('%params')->fetch($e->get('$_'))
                          );
                    })
                );
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

(method DESTROYALL ((opaque $self:) (hash %params)) returns nil
    ((= &dispatcher (($self: class) send (dispatcher :ascendant)))
     (= $method (WALKMETH (&dispatcher (symbol new DESTROY))))
     (while ($method is_not_nil) 
        ($method do ($self %params))
        (= $method (WALKMETH (&dispatcher (symbol new DESTROY)))))))

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('DESTROYALL'),
    # method body
        method->new(
            $::ENV,
            closure::signature->new(
                params => closure::params->new(symbol->new('$self:' => 'opaque')),
                returns => 'nil'
            ), 
            sub {                
                my $e = shift;
                $e->create('&dispatcher' => $e->get('$self:')->class->send('dispatcher' => symbol->new(':ascendant')));
                $e->create('$method' => $e->get('WALKMETH')->do(list->new($e->get('&dispatcher'), symbol->new('DESTROY'))));
                block->new($e, sub {
                    my $e = shift;
                    $e->get('$method')->do(list->new($e->get('$self:'), $e->get('%params')));                  
                    $e->set('$method' => $e->get('WALKMETH')->do(list->new($e->get('&dispatcher'), symbol->new('DESTROY'))));
                })->do_until(
                    block->new($e, sub {
                        my $e = shift;
                        $e->get('$method')->is_nil;
                    })
                );
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

(method id ((opaque $self:)) returns num 
    ($self: id))

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('id'),
    # method body
        method->new(
            $::ENV,
            closure::signature->new(
                params => closure::params->new(symbol->new('$self:' => 'opaque')),
                returns => 'num'
            ), 
            sub {
                my $e = shift;
                $e->get('$self:')->id;
            }
        )
    )
);

=pod

method class (opaque $self:) returns opaque {
    # speculative syntax here :)
    unbox($self).class
}

(method class ((opaque $self:)) returns opaque 
    ($self: class))

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('class'),
    # method body
        method->new(
            $::ENV,
            closure::signature->new(
                params => closure::params->new(symbol->new('$self:' => 'opaque')),
                returns => 'opaque'
            ), 
            sub {
                my $e = shift;
                return $e->get('$self:')->class;                
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

(method superclasses ((opaque $self) (list ?@superclasses)) returns list 
    ((?@superclasses is_nil) or 
     (?@superclasses apply 
          ($_ send (add_subclass $self:))
          ($self: set_attr ((symbol new @:superclasses) (?@superclasses)))
          ($self: set_attr ((symbol new @:MRO) (list new)))          
          ($self: send (MRO))))
     ($self: get_attr (symbol new @:superclasses)))

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('superclasses'),
    # method body
        method->new(
            $::ENV,
            closure::signature->new(
                params => closure::params->new(
                              symbol->new('$self:'         => 'opaque'),
                              symbol->new('?@superclasses' => 'list'),                              
                          ),
                returns => 'list'
            ),             
            sub {
                my $e = shift;
                $e->get('?@superclasses')
                  ->is_nil
                  ->or(
                      block->new($e, sub {
                          $e->get('?@superclasses')
                            ->apply(
                              block->new($e, sub {
                                  my $e = shift;
                                  $e->get('$_')->send('add_subclass' => $e->get('$self:'));
                              })
                          );
                          $e->get('$self:')->set_attr(symbol->new('@:superclasses') => $e->get('?@superclasses')); 
                          # clear the MRO now
                          $e->get('$self:')->set_attr(symbol->new('@:MRO') => list->new()); 
                          # and recalculate it ..
                          $e->get('$self:')->send('MRO');                          
                      })
                  );
                $e->get('$self:')->get_attr(symbol->new('@:superclasses'));                
            }
        )
    )
);

=pod

method subclasses (opaque $self:) returns list {
    @:subclasses;
}

(method subclasses ((opaque $self:)) returns list 
    ($self: get_attr (symbol new @:subclasses)))

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('subclasses'),
    # method body
        method->new(
            $::ENV,
            closure::signature->new(
                params => closure::params->new(symbol->new('$self:' => 'opaque')),
                returns => 'list'
            ), 
            sub {
                my $e = shift;
                $e->get('$self:')->get_attr(symbol->new('@:subclasses'));                
            }
        )
    )
);

=pod

method add_subclass (opaque $self: opaque $subclass) returns nil {
    @:subclasses.push($subclass);
}

(method add_subclass ((opaque $self:) (opaque $subclass)) returns nil 
    (($self: get_attr (symbol new @:subclasses)) push ($subclass)))

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('add_subclass'),
    # method body
        method->new(
            $::ENV,
            closure::signature->new(
                params => closure::params->new(
                            symbol->new('$self:'    => 'opaque'),
                            symbol->new('$subclass' => 'opaque'),           
                          ),
                returns => 'nil'
            ),             
            sub {
                my $e = shift;        
                $e->get('$self:')->get_attr(symbol->new('@:subclasses'))->push($e->get('$subclass'));                  
            }
        )
    )
);

=pod

method _merge (opaque $self:) returns list {
    ...
}

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('_merge'),
    # method body
        method->new(
            $::ENV,
            closure::signature->new(
                params => closure::params->new(symbol->new('$self:' => 'opaque')),
                returns => 'list'
            ), 
            sub {
                my $e = shift;
                C3->new($e->get('$self:'))->linearize;
            }
        )
    )
);

=pod

method MRO (opaque $self:) returns list {
    unless (@:MRO) {
        @:MRO = $self._merge();
    }
    @:MRO;
}

(method MRO ((opaque $self:)) returns list 
    (($self: get_attr (symbol new @:MRO)) is_empty) and 
     ($self: set_attr ((symbol new @:MRO) ($self: send (_merge)))) 
    ($self: get_attr (symbol new @:MRO)))

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('MRO'),
    # method body
        method->new(
            $::ENV,
            closure::signature->new(
                params => closure::params->new(symbol->new('$self:' => 'opaque')),
                returns => 'list'
            ), 
            sub {
                my $e = shift;
                # if it is empty ... 
                $e->get('$self:')
                  ->get_attr(symbol->new('@:MRO'))
                  ->is_empty
                  ->and(
                      block->new($e, sub {
                          my $e = shift;
                          $e->get('$self:')
                            ->set_attr(
                                symbol->new('@:MRO'),
                                $e->get('$self:')->send('_merge')
                            );                          
                      })
                  );
                return $e->get('$self:')->get_attr(symbol->new('@:MRO'));                
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
            closure::signature->new(
                params => closure::params->new(
                              symbol->new('$self:' => 'opaque'),
                              symbol->new('?$order' => 'symbol')                                                                
                          ),
                returns => 'block'
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
            closure::signature->new(
                params => closure::params->new(
                              symbol->new('$self:'  => 'opaque'),
                              symbol->new('@values' => 'list')                                                                
                          ),
                returns => 'block'
            ),                
            sub {
                my $e = shift;
                $e->create('$counter' => num->new(0));
                return block->new($e, sub { 
                    my $e = shift;
                    $e->create('$temp' => $e->get('$counter'));
                    $e->set('$counter' => $e->get('$counter')->increment);                        
                    return $e->get('@values')->fetch($e->get('$temp')); 
                });
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
            closure::signature->new(
                params  => closure::params->new(symbol->new('$self:'  => 'opaque')),
                returns => 'block'
            ),  
            sub {
                my $e = shift;
                $e->get('$self:')->send('_make_dispatcher_iterator' => $e->get('$self:')->send('MRO')->reverse);
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
            closure::signature->new(
                params  => closure::params->new(symbol->new('$self:'  => 'opaque')),
                returns => 'block'
            ),  
            sub {
                my $e = shift;
                $e->get('$self:')->send('_make_dispatcher_iterator' => $e->get('$self:')->send('MRO'));
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
            closure::signature->new(
                params => closure::params->new(
                              symbol->new('$self:' => 'opaque'), 
                              symbol->new('$class' => 'opaque'),                
                          ),
                returns => 'bit'
            ),
            sub {
                my $e = shift;               
                $e->create('$return' => bit->new(0));
                $e->get('$self:')
                  ->id
                  ->equal_to(
                     $e->get('$class')->id
                  )->and(
                      block->new($e, sub {
                          my $e = shift;
                          $e->set('$return' => bit->new(1))
                      })
                  );
                $e->create('$dispatcher' => $e->get('$self:')->send('dispatcher', symbol->new(':canonical')));
                $e->create('$next' => $e->get('$dispatcher')->do());             
                block->new($e, sub {
                    $e->get('$next')
                      ->id
                      ->equal_to(
                          $e->get('$class')->id
                      )->and(
                          block->new($e, sub {
                              my $e = shift;  
                              $e->set('$return' => bit->new(1))
                          })
                      );
                    $e->set('$next' => $e->get('$dispatcher')->do());
                })->do_while(
                    block->new($e, sub {
                       my $e = shift;
                       ($e->get('$return') == $bit::FALSE && $e->get('$next') != $nil::NIL) ? $bit::TRUE : $bit::FALSE 
                    })
                );
                return $e->get('$return');                 
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
            closure::signature->new(
                params => closure::params->new(
                              symbol->new('$self:'     => 'opaque'), 
                              symbol->new('$label'     => 'symbol'), 
                              symbol->new('$attribute' => 'attribute'),                                 
                          ),
                returns => 'nil'
            ),
            sub {
                my $e = shift;                               
                $e->get('$self:')
                  ->get_attr(symbol->new('%:attributes'))
                  ->store(
                      $e->get('$label'), 
                      $e->get('$attribute')
                  );                   
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
            closure::signature->new(
                params => closure::params->new(
                              symbol->new('$self:' => 'opaque'),  
                              symbol->new('$label' => 'symbol'),                                
                          ),
                returns => 'attribute'
            ),
            sub {
                my $e = shift; 
                $e->get('$self:')
                  ->get_attr(symbol->new('%:attributes'))
                  ->fetch($e->get('$label'));                                 
            }
        )
    )
);

=pod

=cut


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
            closure::signature->new(
                params => closure::params->new(
                              symbol->new('$self:' => 'opaque'),  
                              symbol->new('$label' => 'symbol'),                                
                          ),
                returns => 'bit'
            ),
            sub {
                my $e = shift;
                $e->get('$self:')
                  ->get_attr(symbol->new('%:attributes'))
                  ->exists($e->get('$label'));                                 
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
            closure::signature->new(
                params => closure::params->new(symbol->new('$self:' => 'opaque')),
                returns => 'list'
            ),
            sub {
                my $e = shift;
                $e->get('$self:')->get_attr(symbol->new('%:attributes'))->keys();                                 
            }
        )
    )
);

=pod

=cut

$::Class->send('add_method' => (
    # method label
        symbol->new('get_attributes'),
    # method body
        method->new(
            $::ENV,
            closure::signature->new(
                params => closure::params->new(symbol->new('$self:' => 'opaque')),
                returns => 'list'
            ),
            sub {
                my $e = shift;
                $e->get('$self:')->get_attr(symbol->new('%:attributes'))->values();                                 
            }
        )
    )
);

## ----------------------------------------------------------------------------
## now build Object, so we can create something here

$::Object = $::Class->send('new');

$::Object->send('add_method' => (
    # method label
        symbol->new('BUILD'),
    # method body
        method->new(
            $::ENV,
            closure::signature->new(
                params => closure::params->new(
                              symbol->new('$self:' => 'opaque'), 
                              symbol->new('%params' => 'hash')
                          ),
                returns => 'nil'
            ),          
            sub {
                my $e = shift;
                $e->get('%params')
                  ->keys
                  ->apply(
                    block->new($e, sub {
                        my $e = shift;                        
                        $e->get('$self:')
                          ->set_attr(
                              symbol->new($e->get('$_')->to_native), 
                              $e->get('%params')->fetch($e->get('$_'))
                          );
                    })
                );
                return $nil::NIL;               
            }
        )
    )
);

$::Object->send('add_method' => (
    # method label
        symbol->new('BUILDALL'),
    # method body
        method->new(
            $::ENV,
            closure::signature->new(
                params => closure::params->new(
                              symbol->new('$self:' => 'opaque'), 
                              symbol->new('%params' => 'hash')
                          ),
                returns => 'nil'
            ),
            sub {
                my $e = shift;
                $e->create('&dispatcher' => $e->get('$self:')->class->send('dispatcher' => symbol->new(':descendant')));
                $e->create('$method' => $e->get('WALKMETH')->do(list->new($e->get('&dispatcher'), symbol->new('BUILD'))));
                block->new($e, sub {
                    my $e = shift;
                    $e->get('$method')->do(list->new($e->get('$self:'), $e->get('%params')));                  
                    $e->set('$method' => $e->get('WALKMETH')->do(list->new($e->get('&dispatcher'), symbol->new('BUILD'))));
                })->do_until(
                    block->new($e, sub {
                        my $e = shift;
                        $e->get('$method')->is_nil;
                    })
                );
                return $nil::NIL;                 
            }
        )
    )
);

$::Object->send('add_method' => (
    # method label
        symbol->new('id'),
    # method body
        method->new(
            $::ENV,
            closure::signature->new(
                params => closure::params->new(symbol->new('$self:' => 'opaque')),
                returns => 'num'
            ),
            sub {
                my $e = shift;
                $e->get('$self:')->id;
            }
        )
    )
);

$::Object->send('add_method' => (
    # method label
        symbol->new('class'),
    # method body
        method->new(
            $::ENV,
            closure::signature->new(
                params => closure::params->new(symbol->new('$self:' => 'opaque')),
                returns => 'opaque'
            ),
            sub {
                my $e = shift;
                return $e->get('$self:')->class;                
            }
        )
    )
);

$::Object->send('add_method' => (
    # method label
        symbol->new('can'),
    # method body
        method->new(
            $::ENV,
            closure::signature->new(
                params => closure::params->new(
                              symbol->new('$self:' => 'opaque'),                
                              symbol->new('$label' => 'symbol'),                                
                          ),
                returns => 'method'
            ),
            sub {
                my $e = shift;
                $e->get('WALKMETH')
                  ->do(list->new(
                      $e->get('$self:')->class->send('dispatcher' => symbol->new(':canonical')), 
                      $e->get('$label')
                ));                   
            }
        )
    )
);

$::Object->send('add_method' => (
    # method label
        symbol->new('DESTROYALL'),
    # method body
        method->new(
            $::ENV,
            closure::signature->new(
                params => closure::params->new(symbol->new('$self:' => 'opaque')),
                returns => 'nil'
            ),
            sub {
                my $e = shift;
                $e->create('&dispatcher' => $e->get('$self:')->class->send('dispatcher' => symbol->new(':ascendant')));
                $e->create('$method' => $e->get('WALKMETH')->do(list->new($e->get('&dispatcher'), symbol->new('DESTROY'))));
                block->new($e, sub {
                    my $e = shift;
                    $e->get('$method')->do(list->new($e->get('$self:'), $e->get('%params')));                  
                    $e->set('$method' => $e->get('WALKMETH')->do(list->new($e->get('&dispatcher'), symbol->new('DESTROY'))));
                })->do_until(
                    block->new($e, sub {
                        my $e = shift;
                        $e->get('$method')->is_nil;
                    })
                );
                return $nil::NIL;                                          
            }
        )
    )
);

## ----------------------------------------------------------------------------
## now build Package & Module, so we can create something here

$::Package = $::Class->send('new');
$::Package->send('superclasses' => list->new($::Object));

$::Package->send('add_attribute', (symbol->new('$:name')      => attribute->new('$:name'      => 'str')));
$::Package->send('add_attribute', (symbol->new('%:namespace') => attribute->new('%:namespace' => 'hash')));


$::Package->send('add_method' => (
    # method label
        symbol->new('name'),
    # method body
        method->new(
            $::ENV,
            closure::signature->new(
                params => closure::params->new(
                              symbol->new('$self:' => 'opaque'),
                              symbol->new('?$name' => 'str'),
                          ),
                returns => 'str'
            ),
            sub {
                my $e = shift;
                $e->get('?$name')
                  ->is_not_nil
                  ->and(block->new($e, sub {
                      my $e = shift;
                      $e->get('$self:')->set_attr(symbol->new('$:name'), $e->get('?$name'));
                  }));
                $e->get('$self:')->get_attr(symbol->new('$:name'));
            }
        )
    )
);

$::Package->send('add_method' => (
    # method label
        symbol->new('FETCH'),
    # method body
        method->new(
            $::ENV,
            closure::signature->new(
                params => closure::params->new(
                              symbol->new('$self:' => 'opaque'),                
                              symbol->new('$label' => 'symbol'),                                
                          ),
                returns => 'type'
            ),
            sub {
                my $e = shift;
                $e->create('$return' => $nil::NIL);
                $e->get('$label')
                  ->to_str
                  ->is_equal(str->new('{}'))
                  ->and(block->new($e, sub {
                      $e->set('$return' => $e->get('$self:')->get_attr(symbol->new('%:namespace')));
                  }))
                  ->or(block->new($e, sub {
                      $e->set('$return' => $e->get('$self:')->get_attr(symbol->new('%:namespace'))->fetch($e->get('$label')));
                  }));
                $e->get('$return');
            }
        )
    )
);

$::Package->send('add_method' => (
    # method label
        symbol->new('STORE'),
    # method body
        method->new(
            $::ENV,
            closure::signature->new(
                params => closure::params->new(
                              symbol->new('$self:' => 'opaque'),                
                              symbol->new('$label' => 'symbol'), 
                              symbol->new('$value' => 'type'),                               
                          ),
                returns => 'nil'
            ),            
            sub {
                my $e = shift;
                $e->get('$self:')
                  ->get_attr(symbol->new('%:namespace'))
                  ->store(
                      $e->get('$label'),
                      $e->get('$value')
                  );
            }
        )
    )
);

## ----------------------------------------------------------------------------
## add Module

$::Module = $::Class->send('new');
$::Module->send('superclasses' => list->new($::Package));


$::Module->send('add_attribute', (symbol->new('$:version')   => attribute->new('$:version'   => 'str')));
$::Module->send('add_attribute', (symbol->new('$:authority') => attribute->new('$:authority' => 'str')));

$::Module->send('add_method' => (
    # method label
        symbol->new('version'),
    # method body
        method->new(
            $::ENV,
            closure::signature->new(
                params => closure::params->new(
                              symbol->new('$self:' => 'opaque'),
                              symbol->new('?$version' => 'str'),
                          ),
                returns => 'str'
            ),
            sub {
                my $e = shift;
                $e->get('?$version')
                  ->is_not_nil
                  ->and(block->new($e, sub {
                      my $e = shift;
                      $e->get('$self:')->set_attr(symbol->new('$:version'), $e->get('?$version'));
                  }));
                $e->get('$self:')->get_attr(symbol->new('$:version'));
            }
        )
    )
);

$::Module->send('add_method' => (
    # method label
        symbol->new('authority'),
    # method body
        method->new(
            $::ENV,
            closure::signature->new(
                params => closure::params->new(
                              symbol->new('$self:' => 'opaque'),
                              symbol->new('?$authority' => 'str'),
                          ),
                returns => 'str'
            ),            
            sub {
                my $e = shift;
                $e->get('?$authority')
                  ->is_not_nil
                  ->and(block->new($e, sub {
                      my $e = shift;
                      $e->get('$self:')->set_attr(symbol->new('$:authority'), $e->get('?$authority'));
                  }));
                $e->get('$self:')->get_attr(symbol->new('$:authority'));
            }
        )
    )
);

$::Module->send('add_method' => (
    # method label
        symbol->new('identifier'),
    # method body
        method->new(
            $::ENV,
            closure::signature->new(
                params => closure::params->new(symbol->new('$self:' => 'opaque')),
                returns => 'str'
            ),
            sub {
                my $e = shift;
                $e->create('$identifier' => $e->get('$self:')->get_attr(symbol->new('$:name')));
                $e->get('$self:')
                  ->get_attr(symbol->new('$:version'))
                  ->to_bit()
                  ->and(block->new($e, sub {
                      my $e = shift;
                      $e->set('$identifier' => 
                          $e->get('$identifier')
                            ->concat(
                              str->new('-')
                            )
                            ->concat(
                              $e->get('$self:')->get_attr(symbol->new('$:version'))
                            )
                      );
                  }));
                $e->get('$self:')
                  ->get_attr(symbol->new('$:authority'))
                  ->to_bit()
                  ->and(block->new($e, sub {
                      my $e = shift;
                      $e->set('$identifier' => 
                          $e->get('$identifier')
                             ->concat(
                                str->new('-')
                            )                          
                            ->concat(
                              $e->get('$self:')->get_attr(symbol->new('$:authority'))
                            )
                      );
                  }));
                $e->get('$identifier');
            }
        )
    )
);

## ----------------------------------------------------------------------------
## finishing up the boostrapping

# < Class is subclass of Object >
$::Class->set_attr(symbol->new('@:superclasses') => list->new($::Module));

$::Module->set_attr(symbol->new('@:subclasses') => list->new($::Class));

# NOTE:
# this is to avoid recursion
$::Class->set_attr(symbol->new('@:MRO') => list->new($::Class, $::Module, $::Package, $::Object));
$::Object->set_attr(symbol->new('@:MRO') => list->new($::Object));

foreach my $meta ($::Class, $::Object, $::Package, $::Module) {
    $meta->add_attr(symbol->new('$:name'      => 'str'));
    $meta->add_attr(symbol->new('%:namespace' => 'hash'));    
    $meta->add_attr(symbol->new('$:version'   => 'str'));        
    $meta->add_attr(symbol->new('$:authority' => 'str'));            
}

# add their names in
$::Class->send('name'   => str->new('Class'));
$::Object->send('name'  => str->new('Object'));
$::Package->send('name' => str->new('Package'));
$::Module->send('name'  => str->new('Module'));

1;

__END__
