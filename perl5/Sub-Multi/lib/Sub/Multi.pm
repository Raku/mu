package Sub::Multi;
our $VERSION = '0.001_1';
use base 'Class::Multimethods::Pure';
use Data::Bind 0.26;

=head1 NAME

Sub::Multi - Data::Bind-based multi-sub dispatch

=head1 SYNOPSIS

 my $wrapper = Sub::Multi->new($sub1, $sub2, $sub3);

 $wrapper->([\(1, 2, 3)], { named => \'value' });

=head1 DESCRIPTION



=cut

sub new {
    my ($class, @subs) = @_;
    return sub { $class->dispatch(\@subs, @_) };
}

sub dispatch {
    my $class = shift;
    my $subs = shift;
    my @compat;
    for my $variant (@$subs) {
	my $cv = Data::Bind::_get_cv($variant);
	push @compat, $variant if *$cv->{sig}->is_compatible(@_);
    }
    die 'I hate vapour ware' unless @compat;
    while (@compat != 1) {
	die 'I hate ambiguous software';
    }
    goto $compat[0];
}

1;

=head1 AUTHORS

Chia-liang Kao E<lt>clkao@clkao.orgE<gt>

=head1 COPYRIGHT

Copyright 2006 by Chia-liang Kao and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
