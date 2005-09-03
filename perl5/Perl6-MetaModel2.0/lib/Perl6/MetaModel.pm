
# load the metamodel ....
do "lib/genesis.pl";

package Perl6::MetaModel;

use strict;
use warnings;

our $VERSION = '2.00';

sub import {
    no strict 'refs';
    my $pkg = caller;
    *{"${pkg}::class"} = \&class;
}

our %CLASSES_BY_NAME;

sub _ {
    my $attr = shift;
    ::opaque_instance_attrs($::SELF)->{$attr} = shift if @_;
    ::opaque_instance_attrs($::SELF)->{$attr};    
}

sub class {
    my ($full_name, $body) = @_;
    my $new_class = $::Class->class::new();
    my ($name, $version, $authority) = split '-' => $full_name;
    $new_class->name($name)           if defined $name;
    $new_class->version($version)     if defined $version;
    $new_class->authority($authority) if defined $authority;        
    _process_class_body($new_class, $body);
    $CLASSES_BY_NAME{$new_class->name} = $new_class;
    return $new_class;
}

sub _process_class_body {
    my ($new_class, $body) = @_;
    if (ref($body) eq 'CODE') {
        ::bind_CLASS($new_class);
        $body->();
        ::unbind_CLASS();
    }
    elsif (ref($body) eq 'HASH') {
        $new_class->superclasses($body->{is}) if exists $body->{is};
        if ($body->{attributes}) {
            foreach my $attribute_name (@{$body->{attributes}}) {
                $new_class->add_attribute($attribute_name => ::make_attribute($attribute_name));
            }
        }
        if ($body->{class_attributes}) {
            foreach my $attribute_name (@{$body->{class_attributes}}) {
                $new_class->add_attribute($attribute_name => ::make_class_attribute($attribute_name));
            }
        }        
        if ($body->{methods}) {
            foreach my $method_name (keys %{$body->{methods}}) {
                if ($method_name =~ /^_/) {
                    $new_class->add_method($method_name => ::make_private_method(
                        $body->{methods}->{$method_name}, $new_class
                    ));                    
                }
                else {
                    $new_class->add_method($method_name => ::make_method(
                        $body->{methods}->{$method_name}, $new_class
                    ));
                }
            }
        }
        if ($body->{sub_methods}) {
            foreach my $method_name (keys %{$body->{sub_methods}}) {
                $new_class->add_method($method_name => ::make_submethod(
                    $body->{sub_methods}->{$method_name}, $new_class
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
                        $body->{class_methods}->{$method_name}, $new_class
                    ));
                }
                else {
                    $new_class->add_method($method_name => ::make_class_method(
                        $body->{class_methods}->{$method_name}, $new_class
                    ));                    
                }
            }
        }        
    }
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
