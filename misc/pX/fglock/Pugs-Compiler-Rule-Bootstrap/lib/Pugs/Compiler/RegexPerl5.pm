package Pugs::Compiler::RegexPerl5;

# Version in Pugs::Compiler::Rule
# Documentation in the __END__
use 5.006;
use strict;
use warnings;

use base 'Pugs::Compiler::Regex';

sub compile {
    my ( $class, $rule_source, $param ) = @_;
    $param = ref $param ? { %$param } : {}; 
    delete $param->{P5};
    delete $param->{Perl5};
    warn "Error in rule: unknown parameter '$_'" 
        for keys %$param;
    my $self = { source => $rule_source };
    $self->{perl5} = 
q(sub {
  my $s = $_[1];
  pos $_[3]{p};
  my $bool = \( $s =~ /) . $rule_source . q(/ \) ? 1 : 0;
  my @match;
  for ( 1 .. $#+ ) {
      push @match, bless \\{
        str => \\$s, from => \\(0+$-[$_]), to => \\(0+$+[$_]),
        bool => \\1, match => [], named => {}, capture => \\undef,
      }, 'Pugs::Runtime::Match::Ratchet';
  }
  return bless \\{
    str => \\$s, from => \\(0+$-[0]), to => \\(0+$+[0]),
    bool => \\$bool, match => \\@match, named => {}, capture => \\undef,
  }, 'Pugs::Runtime::Match::Ratchet';
};
);
    #print 'rule perl5: ', do{use Data::Dumper; Dumper($self->{perl5})};

    local $@;
    $self->{code} = eval 
        $self->{perl5};
    die "Error in evaluation: $@\nSource:\n$self->{perl5}\n" if $@;

    bless $self, $class;
}

1;

__END__

=head1 NAME 

Pugs::Compiler::RegexPerl5 - Compiler for Perl 6 style "Perl5" regex

=head1 DESCRIPTION

This module provides an implementation for Perl 6 regexes that use the "Perl5" switch:

    :Perl5 /.*/

    :P5 /.*/

See L<Pugs::Compiler::Rule> for documentation.

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 Rules Spec: L<http://dev.perl.org/perl6/doc/design/syn/S05.html>

=head1 COPYRIGHT

Copyright 2006 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
