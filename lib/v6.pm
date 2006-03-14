package v6;

use strict;
use Inline; # this is going away soon
use Filter::Simple;

$v6::VERSION = '0.02';

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

v6 - Pragma for using Perl 6 inside Perl 5

=head1 SYNOPSIS

    #!/usr/bin/perl
    use v6-pugs; # Here is some Perl 6 code...
    sub postfix:<!> { [*] 1..$_ }
    sub sum_factorial { [+] 0..$_! }

    no v6;       # Here is some Perl 5 code...
    print sum_factorial(3); # 21

=head1 DESCRIPTION

The B<v6> pragma lets you put Perl 6 code in Perl 5 source code
after a line of:

    use v6-pugs;

To switch back to Perl 5:

    no v6;

=head1 NOTES

Whilst resembling C<use VERSION> syntax, Perl 5 actually (mis)interprets
C<use v6-pugs> as a request to use a module named C<v6.pm>, passing in C<'-pugs'>
as an argument list to C<import> after C<v6.pm> is C<require>d.  The lack of
whitespace between C<v6> and C<-pugs> is neccessary in this instance,
due to the bizarre interaction between the hybrid C<use VERSION> and
C<use Module VERSION> syntax.

Larry gave the following explanation of why it works on C<#perl6>:

    ...the interaction of v-syntax with 'use' versions is interesting, to say
    the least, as witnessed by 'v6-pugs'.

    In theory, Perl 5 oughta just blow up the moment it sees a 'use v6', saying 
    "I'm not version 6..." But there's chicanery in the lexer to rearrange 
    module versions, such that the parser sees the version first, and that's 
    getting fooled by 'v6-pugs' into thinking it's a module version when the 
    parser thinks otherwise.

=head1 SEE ALSO

L<Perl6::Pugs>

=head1 AUTHOR

Brian Ingerson C<E<lt>INGY@cpan.orgE<gt>>,
Audrey Tang C<E<lt>autrijus@autrijus.orgE<gt>>

=head1 COPYRIGHT

Copyright (c) 2005. Brian Ingerson. All rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>.

=cut

