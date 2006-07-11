
package Pugs::Runtime::Perl6;

use strict;
use warnings;

use Data::Dumper;
use Data::Bind;
use Sub::Multi;

# TODO - see Pugs::Runtime::Grammar for metaclass stuff

use constant Inf => 100**100**100;
use constant NaN => Inf - Inf;

sub perl {
    local $Data::Dumper::Terse = 1;
    return join( ', ', Data::Dumper::Dumper( @_ ) );
}

sub eval {
    my ($string, $lang);
    Data::Bind->arg_bind(\@_);
    $lang ||= 'perl6';
    my $eval_string;
    Data::Bind::bind_op2(\$eval_string, \$string);
    if ($lang eq 'perl6') {
	require Pugs::Compiler::Perl6;
	my $p6 = Pugs::Compiler::Perl6->compile( $string );
	Data::Bind::bind_op2(\$eval_string, \$p6->{perl5});
    }
    elsif ($lang ne 'perl5') {
	die;
    }

    local $@;
    no warnings;
    my @result;
    if (wantarray) {
	@result = eval $eval_string;
    }
    else {
	$result[0] = eval $eval_string;
    }
    $::_EXCL__ = $@;
    return wantarray ? @result : $result[0];

}

Data::Bind->sub_signature(\&eval, { var => '$string' }, { var => '$lang', optional => 1});

package Pugs::Runtime::Perl6::Routine;
use B ();
use Devel::Caller ();

sub new {
    my ($class, $cv) = @_;
    bless { cv => B::svref_2object($cv) }, $class;
}

sub code {
    $_[0]->{cv}->object_2svref;
}

sub name {
    my $self = shift;
    my $cv = $self->{cv};
    return '&'.$cv->GV->STASH->NAME . '::' . $cv->GV->NAME;
}

sub package {
    $_[0]->{cv}->GV->STASH->NAME;
}

sub arity {
    my $cv = Data::Bind::_get_cv($_[0]->code);
    use Data::Dumper;
    return *$cv->{sig} ? *$cv->{sig}->arity : 0;
}

package Pugs::Runtime::Perl6::Scalar;

sub defined { CORE::defined(@_) }

sub isa {
    my $self = $_[0];
    return 1 if $_[1] eq 'Str'  && defined $_[0];
    return 1 if $_[1] eq 'Num'  && defined $_[0];
    return 1 if $_[1] eq 'Code' && ref($_[0]) eq 'CODE';
    return 0;
}

package Pugs::Runtime::Perl6::Scalar::Alias;

sub TIESCALAR {
    my $class = shift;
    my $var_ref = shift;
    return bless $var_ref, $class;
}
sub FETCH {
    my $self = shift;
    $$self;
}
sub STORE {
    my $self = shift;
    $$self = shift;
  }

1;

__END__

=pod

=head1 NAME 

Pugs::Runtime::Perl6

=head1 DESCRIPTION

Provides runtime routines for the Perl6-in-Perl5 compiled code

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 COPYRIGHT

Copyright 2006 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
