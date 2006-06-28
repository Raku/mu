
package Pugs::Runtime::Grammar;

use strict;
use warnings;
use metaclass;

use Pugs::Compiler::Rule;

our $VERSION ='0.01';

# stash the metaclass for the package 
# that we store the grammer in
__PACKAGE__->meta->add_attribute('$:pkg_meta' => (
	reader   => '_pkg_meta',
	init_arg => 'pkg_meta'
));

# keep the rules handy in a local hash
__PACKAGE__->meta->add_attribute('%:rules' => (
	reader  => 'rules',
	default => sub { {} },
));

sub new {
	my ($class, $name, $version) = @_;
	# create the package to hold the 
	my $meta = Class::MOP::Class->create($name, $version);
	my $self = $class->meta->new_object(pkg_meta => $meta);
	$meta->add_method('grammar' => sub { $self });
	return $self;
}

sub add_rule {
	my ($self, $rule_name, $rule) = @_;
	# try to compile the rule first ...
	my $compiled_rule = eval { Pugs::Compiler::Rule->compile( $rule ) };
	die "Could not compile rule ($rule_name) because : $@" if $@;
	
	# add the compiled rule to our local stash
	$self->rules->{$rule_name} = $compiled_rule;
	# but put the .code version of it into the package
	$self->_pkg_meta->add_method($rule_name, sub { $compiled_rule });
}

1;

__END__

=pod

=head1 NAME 

Pugs::Runtime::Grammar

=head1 SYNOPSIS

  my $grammar = Pugs::Runtime::Grammar->new('Foo');
  
  $grammar->add_rule(bar => '((.).).');
  # or
  Foo->grammar->add_rule(bar => '((.).).');
  
  my $match = Foo->bar->match( 'abc' );
  
  if ($match) {               # true
      print $match;           # "abc"
      print $match->from;     # 0
      print $match->to;       # 3
      print $match->[0];      # "ab"
      print $match->[0][0];   # "a"
  }

=head1 DESCRIPTION

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 Rules Spec: L<http://dev.perl.org/perl6/doc/design/syn/S05.html>

=head1 COPYRIGHT

Copyright 2006 by Stevan Little and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
