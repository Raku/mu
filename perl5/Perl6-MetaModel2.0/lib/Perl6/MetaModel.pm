
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
        $body->($new_class);
    }
    elsif (ref($body) eq 'HASH') {
        ;
    }
}

1;

__END__

=pod

=head1 NAME

Perl6::MetaModel - The Perl 6 Object Meta Model

=head1 SYNOPOSIS

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
      my $class = shift;
      $class->superclasses([ $::Object ]);
      foreach my $name (qw(foo bar)) {
          $class->add_method($name => ::make_class_method(sub { "Hello from $name" }, $class));
      }
  };

=head1 DESCRIPTION

=head1 SEE ALSO

=head1 AUTHOR

Stevan Little E<gt>stevan@iinteractive.comE<lt>

=cut