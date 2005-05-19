package pugs;
use strict;
use Filter::Simple;

$pugs::VERSION = '0.01';

FILTER {
    $_ = <<END;
use Inline Pugs => <<'END_OF_INLINE_PUGS_SECTION';
$_
END_OF_INLINE_PUGS_SECTION
END
};

1;

__DATA__

=head1 NAME 

pugs - Pragma for using Perl 6 inside Perl 5

=head1 SYNOPSIS

    #!/usr/bin/perl
    use pugs;
    sub postfix:<!> { [*] 1..$_ }
    sub sum_factorial { [+] 0..$_! }
    no pugs;
    print sum_factorial(3); # 21

=head1 DESCRIPTION

The C<Pugs> module lets you put Perl 6 code in Perl 5 source code after
a line of:

    use pugs;

To switch back to Perl 5:

    no pugs;

=head1 AUTHOR

Brian Ingerson <INGY@cpan.org>

=head1 COPYRIGHT

Copyright (c) 2005. Brian Ingerson. All rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See http://www.perl.com/perl/misc/Artistic.html

=cut
