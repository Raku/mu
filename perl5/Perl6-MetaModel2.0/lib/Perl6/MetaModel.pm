
# load the metamodel ....
do "lib/genesis.pl";

package Perl6::MetaModel;

use strict;
use warnings;

use Carp 'confess';

our $VERSION = '2.00';

sub import {
    no strict 'refs';
    my $pkg = caller;
    *{"${pkg}::class"} = \&class;
}

our %CLASSES_BY_NAME;

sub _ {
    my $attr = shift;
    ($::CLASS->find_attribute_spec($attr))
        || confess "Attribute ($attr) is not a valid attribute for $::SELF";
    ::opaque_instance_attrs($::SELF)->{$attr} = shift if @_;
    ::opaque_instance_attrs($::SELF)->{$attr};    
}

sub class {
    my ($full_name, $body) = @_;
    # support anon-classes here
    if (!defined($body) && ref($full_name) && ref($full_name) =~ /HASH|CODE/) {
        return _build_class(undef, undef, undef, $full_name);
    }
    my ($name, $version, $authority) = split '-' => $full_name;       
    my $new_class = _build_class($name, $version, $authority, $body);
    $CLASSES_BY_NAME{$new_class->name} = $new_class;
    return $new_class;
}

sub _build_class {
    my ($name, $version, $authority, $body) = @_;
    
    my $metaclass = $::Class;
    $metaclass = $body->{metaclass} 
        if ref($body) eq 'HASH' && exists $body->{metaclass};
    
    my $new_class = $metaclass->new();    
    
    $new_class->name($name)           if defined $name;
    $new_class->version($version)     if defined $version;
    $new_class->authority($authority) if defined $authority;     
    
    if (ref($body) eq 'CODE') {
        local $::CLASS = $new_class;
        $body->();
    }
    elsif (ref($body) eq 'HASH') {
        $new_class->superclasses($body->{is}) if exists $body->{is};
        if ($body->{attributes}) {
            foreach my $attribute_name (@{$body->{attributes}}) {
                $new_class->add_attribute($attribute_name => ::make_attribute($attribute_name));
            }
        }
        if ($body->{class_attributes}) {
            foreach my $attr_name (@{$body->{class_attributes}}) {
                $new_class->STORE($attr_name => ($attr_name =~ /^@/ ? [] : $attr_name =~ /^%/ ? {} : undef));
            }
        }        
        if ($body->{methods}) {
            foreach my $method_name (keys %{$body->{methods}}) {
                if ($method_name =~ /^_/) {
                    $new_class->add_method($method_name => ::make_private_method(
                        $body->{methods}->{$method_name}
                    ));                    
                }
                else {
                    $new_class->add_method($method_name => ::make_method(
                        $body->{methods}->{$method_name}
                    ));
                }
            }
        }
        if ($body->{submethods}) {
            foreach my $method_name (keys %{$body->{submethods}}) {
                $new_class->add_method($method_name => ::make_submethod(
                    $body->{submethods}->{$method_name}
                ));
            }
        }                
        if ($body->{class_methods}) {
            foreach my $method_name (keys %{$body->{class_methods}}) {
                if ($method_name =~ /^_/) {
                    # we just add a private method in here...
                    # there is no real distinction between the
                    # private class method or the private instance 
                    # method, maybe there should be, but I am not sure
                    $new_class->add_method($method_name => ::make_private_method(
                        $body->{class_methods}->{$method_name}
                    ));
                }
                else {
                    $new_class->add_method($method_name => ::make_class_method(
                        $body->{class_methods}->{$method_name}
                    ));                    
                }
            }
        }        
    }
    
    return $new_class;
}

1;

__END__

=pod

=head1 NAME

Perl6::MetaModel - The Perl 6 Object Meta Model

=head1 SYNOPSIS

  # more Macro-ish form (less typing for you)
  class 'Foo-0.0.1-cpan:STEVAN' => {
      is => [ $::Object ],
      class_methods => {
          foo => sub { "Hello from foo" },
          bar => sub { "Hello from bar" },          
      }
  };

  # class-is-a-closure form (more typing 
  # for you, but more control)
  class 'Foo-0.0.1-cpan:STEVAN' => sub {
      $::CLASS->superclasses([ $::Object ]);
      foreach my $name (qw(foo bar)) {
          $::CLASS->add_method($name => ::make_class_method(sub { "Hello from $name" }, $::CLASS));
      }
  };

=head1 DESCRIPTION

=head1 SEE ALSO

=head1 AUTHOR

Stevan Little E<gt>stevan@iinteractive.comE<lt>

=cut
