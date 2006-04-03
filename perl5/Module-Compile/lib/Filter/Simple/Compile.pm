package Filter::Simple::Compile;

use strict;
use warnings;
use Module::Compile ();

sub import {
    my $class = shift;
    my $code = shift;
    my $pkg = caller;

    no strict 'refs';

    push @{"$pkg\::ISA"} => 'Module::Compile';
    *{"$pkg\::FILTER"} = \&FILTER;

    ($code and ref($code) eq 'CODE') or return;
    _setup_filter($pkg, $code);
}

sub unimport {
    my $pkg = caller;

    no strict 'refs';
    *{"$pkg\::pmc_use_means_no"} = sub { 1 };

    goto &{$_[0]->can('import')};
}

sub FILTER (&) {
    my $pkg = caller;
    my $code = shift;

    _setup_filter($pkg, $code);
}

sub _setup_filter {
    my ($pkg, $code) = @_;

    no strict 'refs';
    *{"$pkg\::pmc_compile"} = sub {
        local $_;
        (undef, $_, undef) = @_;
        $code->();
        return $_;
    };
}

1;

__END__

=head1 NAME

Filter::Simple::Compile - Drop-in replacement to Filter::Simple

=head1 SYNOPSIS

Drop-in replacement for L<Filter::Simple>:

    package MyFilter;
    use Filter::Simple::Compile;
    FILTER { ... };

This way also works:

    use Filter::Simple::Compile sub { ... };

=head1 DESCRIPTION

This module lets you write B<Module::Compile> extensions that
are compatible with B<Filter::Simple>'s API.

Additionally, C<no Filter::Simple::Compile> does the same thing
as C<use Filter::Simple::Compile>, except the meaning for C<use>
and C<no> will be reversed for your filter:

    package MyFilter;
    no Filter::Simple::Compile sub { ... }

    # "no MyFilter" begins filtering
    # "use MyFilter" terminates it

=head1 AUTHORS

Audrey Tang E<lt>cpan@audreyt.orgE<gt>

=head1 COPYRIGHT

Copyright 2006 by Audrey Tang E<lt>cpan@audreyt.orgE<gt>.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
