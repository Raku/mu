package pugs;
use strict;
use Filter::Simple;

$pugs::VERSION = '0.02';

FILTER {
    my $marker = 'XXXXXXXX';
    $marker++ while /^$marker$/m;
    $_ = <<END;
use Inline Pugs => <<'$marker';
$_
$marker
END
};

1;

__DATA__

=head1 NAME 

pugs - Pragma for using Perl 6 inside Perl 5

=head1 SYNOPSIS

    #!/usr/bin/perl
    use pugs;   # Here is some Perl 6 code...
    sub postfix:<!> { [*] 1..$_ }
    sub sum_factorial { [+] 0..$_! }
    no pugs;    # Here is some Perl 5 code...
    print sum_factorial(3); # 21

=head1 DESCRIPTION

The B<pugs> module lets you put Perl 6 code in Perl 5 source code after
a line of:

    use pugs;

To switch back to Perl 5:

    no pugs;

=head1 SEE ALSO

L<Perl6::Pugs>

=head1 AUTHOR

Brian Ingerson C<E<lt>INGY@cpan.orgE<gt>>

=head1 COPYRIGHT

Copyright (c) 2005. Brian Ingerson. All rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>.

=cut
