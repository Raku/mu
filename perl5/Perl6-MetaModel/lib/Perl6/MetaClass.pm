
package Perl6::MetaClass;

use strict;
use warnings;

use Scalar::Util 'blessed';
use Hash::Util 'lock_keys';
use Carp 'confess';

use Perl6::MetaModel;
use Perl6::MetaClass::Dispatcher;

sub new {
    my ($class, %params) = @_;
    my $meta = bless {
        class => 'Perl6::MetaClass',
        instance_data => { 
            # meta-information
            '$.name'         => $params{name}      || undef,
            '$.version'      => $params{version}   || '0.0.0',
            '$.authority'    => $params{authority} || undef,
            # the guts of the metaclass
            '@:MRO'          => undef,
            '@:superclasses' => $params{superclasses} || [],
            '%:class_definition' => {
                methods      => {},
                attributes   => {},
            },
            '%:class_data' => {
                methods      => {},
                attributes   => {},
            },        
        }
    }, $class;
    # lock the keys for safe keeping 
    lock_keys(%{$meta});
    lock_keys(%{$meta->{instance_data}}); 
    return $meta;
}

our $META;
sub meta {
    unless (defined $META) {
        $META = Perl6::MetaClass->new(name => 'Perl6::MetaClass', version => '0.0.1');
        # methods ...
        $META->add_method(
            'version' => Perl6::Instance::Method->new('Perl6::MetaClass' => sub {
                my ($self, $version) = @_;
                if (defined $version) {
                    ($version =~ /^\d+\.\d+\.\d+$/)
                        || confess "The version ($version) is not in the correct format '0.0.0'";
                    _('$.version' => $version);
                }
                _('$.version');
            })
        );
        $META->add_method(
            'identifier' => Perl6::Instance::Method->new('Perl6::MetaClass' => sub {        
                return join '-' => (_('$.name'), _('$.version'), (_('$.authority') || ()));
            })
        );
        $META->add_method(
            'is_a' => Perl6::Instance::Method->new('Perl6::MetaClass' => sub {        
                my ($self, $class) = @_;
                $class = $class->name if blessed($class) && $class->isa('Perl6::MetaClass');
                return 1 if $self->name eq $class;
                my $dispatcher = $self->dispatcher(':canonical');
                while (my $next = $dispatcher->next) {    
                    return 1 if $next->name eq $class;
                }
                return 0; 
            })
        );
        $META->add_method(
            'superclasses' => Perl6::Instance::Method->new('Perl6::MetaClass' => sub {        
                my ($self, $superclasses) = @_;
                if (defined $superclasses) {
                    (ref($superclasses) eq 'ARRAY')
                        || confess "BadType : You must pass the superclasses as an ARRAY ref";
                    (blessed($_) && $_->isa('Perl6::MetaClass'))
                        || confess "IncorrectObjectType : A superclass must be a Perl6::MetaClass instance got($_)"
                            foreach @{$superclasses};
                    _('@:superclasses' => $superclasses);    
                    # since the superclasses changed, 
                    # we need to calc the MRO again
                    _('@:MRO' => undef);
                    $self->MRO();
                }
                _('@:superclasses');
            })
        );        
        $META->add_method(
            'class_precedence_list' => Perl6::Instance::Method->new('Perl6::MetaClass' => sub {        
                my ($self, $order) = @_;
                my $seen = {};
                my @class_precedence_list;    
                my $dispatcher = $self->dispatcher($order);
                while (my $next = $dispatcher->next) {
                    unless ($seen->{$next->name}) {
                        $seen->{$next->name}++;
                        push @class_precedence_list => $next;              
                    }
                }
                return @class_precedence_list;
            })
        );   
        $META->add_method(
            'dispatcher' => Perl6::Instance::Method->new('Perl6::MetaClass' => sub {   
                my ($self, $order) = @_;
                Perl6::MetaClass::Dispatcher->new($self, $order);
            })
        );             
        # attributes ...
        $META->add_attribute(
            '$.name' => Perl6::Instance::Attribute->new('Perl6::MetaClass' => '$.name', { access => 'rw' })
        );
        $META->add_attribute(
            '$.version' => Perl6::Instance::Attribute->new('Perl6::MetaClass' => '$.version', { access => 'rw', build => '0.0.0' })
        ); 
        $META->add_attribute(
            '$.authority' => Perl6::Instance::Attribute->new('Perl6::MetaClass' => '$.authority', { access => 'rw' })
        );  
        $META->add_attribute(
            '@:superclasses' => Perl6::Instance::Attribute->new('Perl6::MetaClass' => '@:superclasses')
        );    
        $META->add_attribute(
            '@:MRO' => Perl6::Instance::Attribute->new('Perl6::MetaClass' => '@:MRO')
        ); 
        $META->add_attribute(
            '%:class_definition' => Perl6::Instance::Attribute->new('Perl6::MetaClass' => '%:class_definition', {
                build => { methods => {}, attributes => {} }
            })
        );                                
        $META->add_attribute(
            '%:class_data' => Perl6::Instance::Attribute->new('Perl6::MetaClass' => '%:class_data', {
                build => { methods => {}, attributes => {} }
            })
        );                                        
    }
    return $META;
}

sub can {
    my ($self, $label) = @_;
    return 1 if $self->meta->has_method($label);
    return $self->UNIVERSAL::can($label);
}

# XXX - in order to allow for metaclass subclassing
# we need to make this use the MRO, and not
# just check the local method table. 
sub AUTOLOAD {
    my @AUTOLOAD = split '::', our $AUTOLOAD;
    my $label = $AUTOLOAD[-1];
    my $self = shift;    
    return if ($label =~ /DESTROY/);

    my $meta = $self->meta();    
    ($meta->has_method($label))
        || confess "Method ($label) not found for instance ($self)";        
    my $method = $meta->get_method($label);

    return $method->call($self, @_);     
}

sub _merge {
    my (@seqs) = @_;
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
}

sub MRO {
    my ($self) = @_;
    $self->{instance_data}->{'@:MRO'} = [ 
        _merge(
            [ $self ],                                      # the class we are linearizing
            (map { [ $_->MRO() ] } @{$self->superclasses}), # the MRO of all the superclasses
            [ @{$self->superclasses} ]                      # a list of all the superclasses
        )
    ] unless defined $self->{instance_data}->{'@:MRO'};
    @{$self->{instance_data}->{'@:MRO'}};
}

## METHODS

# Instance Methods

sub add_method {
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
    elsif ($method->isa('Perl6::SubMethod')) {
        # XXX - this is probably wrong ... 
        # can submethods be called by a class too?
        $method_table = '%:class_definition'; 
    }    
    else {
        confess "Incorrect Object Type : I dont know what to do with ($method)";
    }    
    $self->{instance_data}->{$method_table}->{methods}->{$label} = $method;
}

sub getmethods {
    my ($self, %params) = @_;
    my $methods         = $self->{instance_data}->{$self->_which_table(\%params)}->{methods};
    return values %$methods;
}

sub get_method {
    my ($self, $label, %params) = @_;
    (defined $label)
        || confess "InsufficientArguments : you must provide a label";
    $self->{instance_data}->{$self->_which_table(\%params)}->{methods}->{$label};
}

sub has_method {
    my ($self, $label, %params) = @_;
    $self->get_method($label, %params) ? 1 : 0;    
}

## ATTRIBUTES

# Instance Attributes

sub add_attribute {
    my ($self, $label, $attribute) = @_;
    (defined $label && defined $attribute)
        || confess "InsufficientArguments : you must provide an attribute and a label";
    (blessed($attribute) && $attribute->isa('Perl6::Attribute'))
        || confess "IncorrectObjectType : Attributes must be a Perl6::Attribute instance got($attribute)";
    $self->_create_accessor($attribute);         
    
    my $method_table;
    if ($attribute->isa('Perl6::Instance::Attribute')) {
        $method_table = '%:class_definition';
    }
    elsif ($attribute->isa('Perl6::Class::Attribute')) {
        $method_table = '%:class_data';
    }

    $self->{instance_data}->{$method_table}->{attributes}->{$label} = $attribute;
}

sub get_attribute {
    my ($self, $label, %params) = @_;
    (defined $label)
        || confess "InsufficientArguments : you must provide a label";
    $self->{instance_data}->{$self->_which_table(\%params)}->{attributes}->{$label};
}

sub has_attribute {
    my ($self, $label, %params) = @_;
    $self->get_attribute($label, %params) ? 1 : 0;
}

sub get_attribute_list {
    my ($self, %params) = @_;
    keys %{$self->{instance_data}->{$self->_which_table(\%params)}->{attributes}};
}

# "spec" here means "whatever annotation went with this attribute when it's declared"
sub find_attribute_spec {
    my ($self, $label, %params) = @_;
    # go in BUILD order
    my $dispatcher = $self->dispatcher(':descendant');
    while (my $next = $dispatcher->next) {   
        return $next->get_attribute($label, %params) 
            if $next->has_attribute($label, %params)
    } 
    return undef;
}

## PRIVATE METHODS

sub _create_accessor {
    my ($self, $attribute) = @_;
    # no accessors if it's not public ...
    return unless $attribute->is_public();
    # do not overwrite already defined methods ...
    return if $self->has_method($attribute->accessor_name());
    
    # otherwise ...
    my $label = $attribute->label();
    my ($method_type, $method_code);
    if ($attribute->isa('Perl6::Instance::Attribute')) {
        $method_type = 'Perl6::Instance::Method';
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
        $method_type = 'Perl6::Class::Method';
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
    
    $self->add_method(
        $attribute->accessor_name(),
        $method_type->new($self->{instance_data}->{'$.name'}, $method_code)
    ); 
}

sub _which_table {
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
    else {
        confess "Incorrect Parameter : methods cannot be found for " . $params->{for};
    }
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
