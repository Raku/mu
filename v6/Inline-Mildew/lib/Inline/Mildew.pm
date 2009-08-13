package Inline::Mildew;

use warnings;
use strict;
our @ISA = qw(Inline);
use Carp;

=head1 NAME

Inline::Mildew - The great new Inline::Mildew!

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';


=head1 SYNOPSIS

use Inline Mildew => <<'END_OF_PERL6_CODE';
say "hi from perl6";
END_OF_PERL6_CODE

=cut

sub register {
    return {
        language => 'Mildew',
        type => 'interpreted',
        suffix => 'foo',
       };
}
sub build {
    my $o = shift;
    my $obj = $o->{API}{location};
    my $path = "$o->{API}{install_lib}/auto/$o->{API}{modpname}";
    $o->mkpath($path) unless -d $path;
    my $code = $o->{API}{code};
    open my $fh, "> $obj"
      or croak "Can't open $obj for output\n$!";
    print $fh $code;
}
sub validate {
}
sub load {
    my $o = shift;
    my $obj = $o->{API}{location};
    system("cd ../re-mildew;perl mildew $obj");
#    open my $fh, "< $obj"
#      or croak "Can't open $obj for output\n$!";
#    my $code = join '', <$fh>;
#    print $code,"\n";
}
sub info {
}

=head1 AUTHOR

Paweł Murias, C<< <pawelmurias at gmail.com> >>

=head1 BUGS

Please report any bugs or feature requests to C<bug-inline-mildew at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Inline-Mildew>.  I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.




=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Inline::Mildew


You can also look for information at:

=over 4

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Inline-Mildew>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Inline-Mildew>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Inline-Mildew>

=item * Search CPAN

L<http://search.cpan.org/dist/Inline-Mildew/>

=back


=head1 ACKNOWLEDGEMENTS


=head1 COPYRIGHT & LICENSE

Copyright 2009 Paweł Murias.

This program is free software; you can redistribute it and/or modify it
under the terms of either: the GNU General Public License as published
by the Free Software Foundation; or the Artistic License.

See http://dev.perl.org/licenses/ for more information.


=cut

1; # End of Inline::Mildew
