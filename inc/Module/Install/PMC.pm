package Module::Install::PMC;

use strict;
use Module::Install::Base 0.61;
use File::Basename ();

use vars qw{$VERSION @ISA};
BEGIN {
	$VERSION = '0.61';
	@ISA     = qw{Module::Install::Base};
}

# Add support on the installer's side to make sure all pmcs have mtime >
# mtime of .pms.
sub pmc_support {
    my $self = shift;
    require File::Find;

    my $postamble = '';

    # This will generate all the .pmcs on the author side.
    $self->admin->pmc_support
        if $self->is_admin;

    my @pmcs = glob('*.pmc');
    File::Find::find( sub {
        push @pmcs, $File::Find::name if /\.pmc$/i;
    }, 'lib', 't');

    $self->realclean_files("@pmcs");

    $postamble .= "\nconfig ::\n";

    for my $pmc (@pmcs) {
        $postamble .= <<".";
\t-\$(NOECHO) \$(CHMOD) 644 $pmc
\t-\$(NOECHO) \$(TOUCH) $pmc
.
    }

    $self->postamble($postamble)
        if @pmcs;
}

1;

__END__

=pod

=head1 NAME

Module::Install::PMC - Support for Perl Compilation (.pmc)

=head1 SYNOPSIS

  To be completed

=head1 DESCRIPTION

  To be completed

=head1 COMMANDS

To be completed

=head1 TO DO

To be completed

=head1 SEE ALSO

L<Module::Install>, L<Module::Compile>

=head1 AUTHORS

Ingy döt Net <ingy@cpan.org>

=head1 COPYRIGHT

Copyright (c) 2006. Ingy döt Net. All rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
