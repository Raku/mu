
package Perl6::MetaClass;

use strict;
use warnings;

use Scalar::Util 'blessed';
use Hash::Util 'lock_keys';
use Carp 'confess';

use Perl6::MetaClass::Dispatcher;

sub new {
    my (%params) = @_;
    my $meta = bless {
        class => 'Perl6::MetaClass',
        instance_data => { 
            # meta-information
            '$.name'         => $params{'$.name'}      || undef,
            '$.version'      => $params{'$.version'}   || '0.0.0',
            '$.authority'    => $params{'$.authority'} || undef,
            # the guts of the metaclass
            '@:MRO'          => [],
            '@:superclasses' => $params{'@:superclasses'} || [],
            '%:private' => {
                # only methods for now
                methods => {}                
            },
            '%:class_definition' => {
                methods      => {},
                attributes   => {},
            },
            '%:class_data' => {
                methods      => {},
                attributes   => {},
            },        
        }
    }, 'Perl6::MetaClass';
    # lock the keys for safe keeping 
    lock_keys(%{$meta});
    lock_keys(%{$meta->{instance_data}});
    
    _build_meta() unless $params{'$.name'} eq 'Perl6::MetaClass';
     
    return $meta;
}


###############################################################################
## BOOTSTRAP METHODS

# we use a sub-reference of this sub to define the 'add_method' method. 
# but in order to actually "add" this method, we need to call it 
# something else in the bootstrap
sub _add_method {
    my ($self, $label, $method) = @_;
    (defined $label && defined $method)
        || confess "InsufficientArguments : you must provide a method and a label";
    (blessed($method) && $method->isa('Perl6::Method'))
        || confess "IncorrectObjectType : Method must be a Perl6::Method object got($method)";
    my $method_table;
    if ($method->isa('Perl6::Instance::Method')) {
        $method_table = '%:class_definition';
    }
    elsif ($method->isa('Perl6::Class::Method')) {
        $method_table = '%:class_data';
    }
    elsif ($method->isa('Perl6::PrivateMethod')) {
        $method_table = '%:private';
    }    
    elsif ($method->isa('Perl6::SubMethod')) {
        # XXX - this is probably wrong ... 
        # can submethods be called by a class too?
        $method_table = '%:class_definition'; 
    }    
    else {
        confess "Incorrect Object Type : I dont know what to do with ($method)";
    }   
    # NOTE: we need to break the opaque instance structure
    # here so that we can bootstrap this method, otherwise
    # using the _($method_table) version here would result 
    # in calls to other methods and those methods don't 
    # actually exists yet 
    $self->{instance_data}->{$method_table}->{methods}->{$label} = $method;
}

our $META;
sub _build_meta {
    $META = new('$.name' => 'Perl6::MetaClass', '$.version' => '0.0.1');

    ## BOOTSTRAPPING ...
    # We have to add the method 'add_method' first, and 
    # we need to use the &_add_method function to do it
    # with. After this, we can add any method we want :)
    _add_method($META, 'add_method' => Perl6::Method->create_instance_method('Perl6::MetaClass' => \&_add_method));    

    # BUILD submethod
    ::dispatch($META, 'add_method', 
        'BUILD' => Perl6::Method->create_submethod('Perl6::MetaClass' => sub {
            my ($self, %params) = @_;
            # XXX - this is just how we choose to deal with this now
            # there is no reason if cannot be done in some other
            # manner, and not stored in the package symbol table. 
            # However, if you change this, also change the Perl6::Object
            # bootstrapping stuff in Perl6::Class::_apply_class_to_environment
            # as well as how $META is stored in this module, and how
            # ::meta() accesses all this in Perl6::MetaModel
            no strict 'refs';
            ${$params{'$.name'} . '::META'} = $self;
        })
    );

    # methods ...
    ::dispatch($META, 'add_method', 
        'version' => Perl6::Method->create_instance_method('Perl6::MetaClass' => sub {
            my ($self, $version) = @_;
            if (defined $version) {
                ($version =~ /^\d+\.\d+\.\d+$/)
                    || confess "The version ($version) is not in the correct format '0.0.0'";
                _('$.version' => $version);
            }
            _('$.version');
        })
    );
    ::dispatch($META, 'add_method',
        'identifier' => Perl6::Method->create_instance_method('Perl6::MetaClass' => sub {        
            return join '-' => (_('$.name'), _('$.version'), (_('$.authority') || ()));
        })
    );
          
    ::dispatch($META, 'add_method',
        'is_a' => Perl6::Method->create_instance_method('Perl6::MetaClass' => sub {        
            my ($self, $class) = @_;
            $class = ::dispatch($class, 'name') if blessed($class) && ::dispatch($class, 'isa', ('Perl6::MetaClass'));
            return 1 if ::dispatch($self, 'name') eq $class;
            my $dispatcher = ::dispatch($self, 'dispatcher', (':canonical'));
            while (my $next = $dispatcher->next) {    
                return 1 if ::dispatch($next, 'name') eq $class;
            }
            return 0; 
        })
    );
    ::dispatch($META, 'add_method',
        'superclasses' => Perl6::Method->create_instance_method('Perl6::MetaClass' => sub {        
            my ($self, $superclasses) = @_;
            if (defined $superclasses) {
                (ref($superclasses) eq 'ARRAY')
                    || confess "BadType : You must pass the superclasses as an ARRAY ref";
                (blessed($_) && blessed($_) eq 'Perl6::MetaClass')
                    || confess "IncorrectObjectType : A superclass must be a Perl6::MetaClass instance got($_)"
                        foreach @{$superclasses};
                _('@:superclasses' => $superclasses);    
                # since the superclasses changed, 
                # we need to calc the MRO again
                _('@:MRO' => []);
                ::dispatch($self, 'MRO');
            }
            _('@:superclasses');
        })
    ); 

    ::dispatch($META, 'add_method',
        '_merge' => Perl6::Method->create_private_method('Perl6::MetaClass' => sub {                
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
        })
    );        
    
    ::dispatch($META, 'add_method',
        'MRO' => Perl6::Method->create_instance_method('Perl6::MetaClass' => sub { 
            my ($self) = @_;
            # XXX - if you need to force a recalc of the 
            # MRO, you just remove all the values from 
            # @:MRO (set it to an empty array), and it 
            # will recalc, this is what the superclasses 
            # method does when you change the value of
            # @:superclasses
            if (scalar(@{_('@:MRO')}) == 0) {
                # NOTE: we need to access this through the
                # opaque instance structure here because
                # setting it with the _('@:MRO' => \@MRO)
                # results in a call to find_attribute_spec
                # which will then in turn call dispatcher
                # which then calls MRO which then throws
                # this into an infinite loop. So the compromise
                # here is to break the opaque structure. 
                $self->{instance_data}->{'@:MRO'} = [ 
                    ::dispatch($self, '_merge', (
                        [ $self ],                                                  # the class we are linearizing
                        (map { [ ::dispatch($_, 'MRO') ] } @{_('@:superclasses')}), # the MRO of all the superclasses
                        [ @{_('@:superclasses')} ]                                  # a list of all the superclasses
                    ))
                ];
            }
            @{_('@:MRO')};
        })
    );        
           
    ::dispatch($META, 'add_method',
        'class_precedence_list' => Perl6::Method->create_instance_method('Perl6::MetaClass' => sub {        
            my ($self, $order) = @_;
            my $seen = {};
            my @class_precedence_list;    
            my $dispatcher = ::dispatch($self, 'dispatcher', ($order));
            while (my $next = $dispatcher->next) {
                my $name = ::dispatch($next, 'name');
                unless ($seen->{$name}) {
                    $seen->{$name}++;
                    push @class_precedence_list => $next;              
                }
            }
            return @class_precedence_list;
        })
    );   
    ::dispatch($META, 'add_method',
        'dispatcher' => Perl6::Method->create_instance_method('Perl6::MetaClass' => sub {   
            my ($self, $order) = @_;
            Perl6::MetaClass::Dispatcher->new($self, $order);
        })
    );            
    ::dispatch($META, 'add_method',
        'has_method' => Perl6::Method->create_instance_method('Perl6::MetaClass' => sub {
            my ($self, $label, %params) = @_;
            ::dispatch($self, 'get_method', ($label, %params)) ? 1 : 0;                    
        })
    );  
    ::dispatch($META, 'add_method',
        'get_method' => Perl6::Method->create_instance_method('Perl6::MetaClass' => sub {
            my ($self, $label, %params) = @_;
            (defined $label)
                || confess "InsufficientArguments : you must provide a label";
            my $table = ::dispatch($self, '_which_table', (\%params));
            _($table)->{methods}->{$label};                
        })
    );        
    # This is from A12, but I think the API needs work ...
    ::dispatch($META, 'add_method',
        'getmethods' => Perl6::Method->create_instance_method('Perl6::MetaClass' => sub {        
            my ($self, %params) = @_;
            my $table = ::dispatch($self, '_which_table', (\%params));                
            my $methods = _($table)->{methods};
            return values %$methods;
        })
    );        
    
    # attribute methods
    ::dispatch($META, 'add_method', 
        'add_attribute' => Perl6::Method->create_instance_method('Perl6::MetaClass' => sub {                         
            my ($self, $label, $attribute) = @_;
            (defined $label && defined $attribute)
                || confess "InsufficientArguments : you must provide an attribute and a label";
            (blessed($attribute) && $attribute->isa('Perl6::Attribute'))
                || confess "IncorrectObjectType : Attributes must be a Perl6::Attribute instance got($attribute)";
            ::dispatch($self, '_create_accessor', ($attribute));         

            my $method_table;
            if ($attribute->isa('Perl6::Instance::Attribute')) {
                $method_table = '%:class_definition';
            }
            elsif ($attribute->isa('Perl6::Class::Attribute')) {
                $method_table = '%:class_data';
            }

            _($method_table)->{attributes}->{$label} = $attribute;
        })
    );                                  
    ::dispatch($META, 'add_method', 
        'get_attribute' => Perl6::Method->create_instance_method('Perl6::MetaClass' => sub {                                     
            my ($self, $label, %params) = @_;
            (defined $label)
                || confess "InsufficientArguments : you must provide a label";
            my $table = ::dispatch($self, '_which_table', (\%params));                    
            _($table)->{attributes}->{$label};
        })
    );
    ::dispatch($META, 'add_method',
        'has_attribute' => Perl6::Method->create_instance_method('Perl6::MetaClass' => sub {          
            my ($self, $label, %params) = @_;
            ::dispatch($self, 'get_attribute', ($label, %params)) ? 1 : 0;
        })
    );
    ::dispatch($META, 'add_method',
        'get_attribute_list' => Perl6::Method->create_instance_method('Perl6::MetaClass' => sub {          
            my ($self, %params) = @_;
            my $table = ::dispatch($self, '_which_table', (\%params));                
            keys %{_($table)->{attributes}};
        })
    );
    # "spec" here means "whatever annotation went with this attribute when it's declared"
    ::dispatch($META, 'add_method',
        'find_attribute_spec' => Perl6::Method->create_instance_method('Perl6::MetaClass' => sub {                  
            my ($self, $label, %params) = @_;
            # go in BUILD order
            my $dispatcher = ::dispatch($self, 'dispatcher', (':descendant'));
            while (my $next = $dispatcher->next) {   
                return ::dispatch($next, 'get_attribute', ($label, %params))
                    if ::dispatch($next, 'has_attribute', ($label, %params))
            } 
            return undef;
        })
    );       
    
    # private methods
    ::dispatch($META, 'add_method',
        '_which_table' => Perl6::Method->create_private_method('Perl6::MetaClass' => sub {         
            my ($self, $params) = @_;
            my $method_table;
            if (not exists $params->{for} || lc($params->{for}) eq 'instance') {
                return '%:class_definition';
            }
            elsif (lc($params->{for}) eq 'class') {
                return '%:class_data';
            }
            elsif (lc($params->{for}) eq 'submethod') {
                return '%:class_definition'; 
            }  
            elsif (lc($params->{for}) eq 'private') {
                return '%:private'; 
            }        
            else {
                confess "Incorrect Parameter : methods cannot be found for " . $params->{for};
            }
        })
    );
    
    ::dispatch($META, 'add_method',
        '_create_accessor' => Perl6::Method->create_private_method('Perl6::MetaClass' => sub {                         
            my ($self, $attribute) = @_;
            # no accessors if it's not public ...
            return unless $attribute->is_public();
            # do not overwrite already defined methods ...
            return if ::dispatch($self, 'has_method', ($attribute->accessor_name()));

            # otherwise ...
            my $label = $attribute->label();
            my ($method_type, $method_code);
            if ($attribute->isa('Perl6::Instance::Attribute')) {
                $method_type = 'create_instance_method';
                $method_code = sub {
                    my ($i, $value) = @_;
                    _($label => $value) if defined $value;
                    _($label);
                } if $attribute->is_rw;
                $method_code = sub {
                    my $i = shift;
                    (@_) && confess "the attribute '$label' is read-only";
                    _($label);
                } if $attribute->is_ro;        
            }
            elsif ($attribute->isa('Perl6::Class::Attribute')) {
                $method_type = 'create_class_method';
                $method_code = sub {
                    my (undef, $value) = @_;
                    $attribute->set_value($label => $value) if defined $value;
                    $attribute->get_value($label);
                } if $attribute->is_rw;
                $method_code = sub {
                    (@_) && confess "the attribute '$label' is read-only";            
                    $attribute->get_value($label);
                } if $attribute->is_ro;                
            }
            else {
                confess "Incorrect Object Type : I do not understand the attribute class ($attribute)";
            }

            ::dispatch($self, 'add_method', (
                $attribute->accessor_name(),
                Perl6::Method->$method_type(_('$.name'), $method_code)
            )); 
        })            
    );
                       
                           
    # attributes ...
    ::dispatch($META, 'add_attribute',
        '$.name' => Perl6::Instance::Attribute->new('Perl6::MetaClass' => '$.name', { access => 'rw' })
    );
    ::dispatch($META, 'add_attribute',
        '$.version' => Perl6::Instance::Attribute->new('Perl6::MetaClass' => '$.version', { access => 'rw', build => '0.0.0' })
    ); 
    ::dispatch($META, 'add_attribute',
        '$.authority' => Perl6::Instance::Attribute->new('Perl6::MetaClass' => '$.authority', { access => 'rw' })
    );  
    ::dispatch($META, 'add_attribute',
        '@:superclasses' => Perl6::Instance::Attribute->new('Perl6::MetaClass' => '@:superclasses')
    );    
    ::dispatch($META, 'add_attribute',
        '@:MRO' => Perl6::Instance::Attribute->new('Perl6::MetaClass' => '@:MRO')
    ); 
    ::dispatch($META, 'add_attribute',
        '%:private' => Perl6::Instance::Attribute->new('Perl6::MetaClass' => '%:private', {
            build => sub { { methods => {} } } # use a sub to ensure unique refs each time
        })
    );         
    ::dispatch($META, 'add_attribute',
        '%:class_definition' => Perl6::Instance::Attribute->new('Perl6::MetaClass' => '%:class_definition', {
            build => sub { { methods => {}, attributes => {} } } # use a sub to ensure unique refs each time
        })
    );                                
    ::dispatch($META, 'add_attribute',
        '%:class_data' => Perl6::Instance::Attribute->new('Perl6::MetaClass' => '%:class_data', {
            build => sub { { methods => {}, attributes => {} } } # use a sub to ensure unique refs each time
        })
    );
}


1;

__END__

=pod

=head1 NAME 

Perl6::MetaClass - Metaclass in the Perl 6 Meta Model

=head1 DESCRIPTION

=head1 METHODS 

=over 4

=item B<new>

=item B<name>

=item B<version>

=item B<authority>

=item B<identifier>

=item B<superclasses>

=item B<is_a>

=item B<MRO>

=item B<dispatcher>

=item B<add_method>

=item B<get_method>

=item B<has_method>

=item B<add_attribute>

=item B<get_attribute>

=item B<has_attribute>

=item B<get_attribute_list>

=item B<find_attribute_spec>

=back

=head1 AUTHOR

Stevan Little E<lt>stevan@iinteractive.comE<gt>

Autrijus Tang E<lt>autrijus@autrijus.orgE<gt>

=cut
