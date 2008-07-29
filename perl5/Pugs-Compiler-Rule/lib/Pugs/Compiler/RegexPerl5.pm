package Pugs::Compiler::RegexPerl5;

# Version in Pugs::Compiler::Rule
# Documentation in the __END__
use 5.006;
use strict;
use warnings;

#use base 'Pugs::Compiler::Regex';

#use Pugs::Compiler::Regex;
#sub code { (+shift)->Pugs::Compiler::Regex::code( @_ ) }

# http://www.foo.be/docs/tpj/issues/vol2_3/tpj0203-0002.html
# is a good reference on the use of pos()

sub _quote_rule {
    my $rule_source = shift;
    return 'm/' . $rule_source . '/' unless $rule_source =~ m{/};
    return 'm{' . $rule_source . '}' unless $rule_source =~ m/{/ || $rule_source =~ m/}/;
    return 'm!' . $rule_source . '!' unless $rule_source =~ m/!/;
    return 'm[' . $rule_source . ']' unless $rule_source =~ m/\[/ || $rule_source =~ m/\]/;
    return 'm^' . $rule_source . '^' unless $rule_source =~ m/^/;
}

sub compile {
    my ( $class, $rule_source, $param ) = @_;
    my $self = { source => $rule_source };
    $param = ref $param ? { %$param } : {}; 
    delete $param->{P5};
    delete $param->{Perl5};
    $self->{continue} = delete $param->{continue} ||
                        delete $param->{c}        || 
                        0;
    warn "Error in rule: unknown parameter '$_'" 
        for keys %$param;
        
    # TODO - set "prior"
        
    my $captures = q'
      for ( 1 .. $#+ ) {
        push @match, Pugs::Runtime::Match->new({
          str => $_[1], from => \\(0+$-[$_]), to => \\(0+$+[$_]),
          bool => \\1, match => [], named => {}, capture => undef,
        });
      }
      ' . 
      #print "POS $bool ",(0+$-[0]),"-",(0+$+[0]),"\n";select(undef, undef, undef, 0.1);
      'return Pugs::Runtime::Match->new({
        str => $_[1], from => \\(0+$-[0]), to => \\(0+$+[0]),
        bool => \\$bool, match => \\@match, named => {}, capture => undef,
      });
      ';
    $self->{perl5} = 
q!do {
  my $rule; 
  $rule = sub { # grammar, string, state, args
  no warnings 'uninitialized';
  my $bool;
  my @match;
  
  return $rule->($_[0], \\$_[1], $_[2], $_[3])
    unless ref( $_[1] );  # backwards compatibility
  
  #print "POS ${$_[1]} ",pos(${$_[1]}),"\n";
  #print "p5 $_[3]{p} \n";
  
  if( $_[3]{continue} ) {
    pos(${$_[1]}) = $_[3]{p}
      if defined $_[3]{p};
    $bool = ( ${$_[1]} =~ !
        . _quote_rule( $rule_source ) 
        . q(g \) ? 1 : 0; ) 
        . $captures 
        . q!
  }
  
  if ( defined $_[3]{p} ) {
      pos(${$_[1]}) = $_[3]{p};
      $bool = ( ${$_[1]} =~ ! 
        . _quote_rule( 
                q(\\G\(?:) . $rule_source . ')' 
          ) 
        . ' ) ? 1 : 0; ' 
        . $captures 
        . q!
  }
  else {
      $bool = ( ${$_[1]} =~ !
      . _quote_rule( $rule_source ) 
      . q( \) ? 1 : 0; ) 
      . $captures . q(
  }
};
}
);
    # print 'rule perl5: ', do{use Data::Dumper; Dumper($self->{perl5})};

    unless ( $param->{compile_only} ) {
        local $@;
        $self->{code} = eval 
            $self->{perl5};
        die "Error in evaluation: $@\nSource:\n$self->{perl5}\n" if $@;
    }
    
    bless $self, 'Pugs::Compiler::Regex';
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
