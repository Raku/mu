package Filter::Simple::Cached;

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

Filter::Simple::Cached - Cache Filter::Simple results

=head1 SYNOPSIS

Drop-in replacement for L<Filter::Simple>:

    package MyFilter;
    use Filter::Simple::Cached;
    FILTER { ... };

=head1 DESCRIPTION


=cut
