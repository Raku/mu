module Sample::Module-0.0.1;
use v6;

sub greeting(Str $name) returns Str is export {
    "hello, $name";
}

=kwid

= NAME

Sample::Module - A Sample Module for Pugs/Perl6

= SYNOPSIS

    > pugs -e 'use Sample::Module'

= DESCRIPTION

This purpose of this module is simply to be an example Perl6 module that will
run and install and have all the distribution components you need.

= WHY

You should use this module as a starting point to base
a new module distribution on.

= AUTHORS

Brian Ingerson <ingy@cpan.org>

= COPYRIGHT

Copyright (c) 2005. Brian Ingerson. All rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See http://www.perl.com/perl/misc/Artistic.html

=cut
