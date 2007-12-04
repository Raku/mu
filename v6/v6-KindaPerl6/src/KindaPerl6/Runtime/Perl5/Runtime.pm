
use v5;
use strict;
#use FindBin;

use KindaPerl6::Runtime::Perl5::MOP;

use KindaPerl6::Runtime::Perl5::Value;
# Value also contains $::List, $::Subset, $::Code, $::Multi

use KindaPerl6::Runtime::Perl5::Container;

use KindaPerl6::Runtime::Perl6::Pair;
use KindaPerl6::Runtime::Perl5::Pair; # Must be after Runtime::Perl5::Value

use KindaPerl6::Runtime::Perl6::NamedArgument;

use KindaPerl6::Runtime::Perl6::Hash;
use KindaPerl6::Runtime::Perl5::Hash;

use KindaPerl6::Runtime::Perl6::Array;
use KindaPerl6::Runtime::Perl5::Array;

use KindaPerl6::Runtime::Perl6::List;
use KindaPerl6::Runtime::Perl5::List;

use KindaPerl6::Runtime::Perl6::Capture;
use KindaPerl6::Runtime::Perl6::Signature;

use KindaPerl6::Runtime::Perl6::Match;
use KindaPerl6::Runtime::Perl5::Match;

use KindaPerl6::Runtime::Perl6::Int;

# try to load gather/take (depends on 'Coro')
eval {
    require KindaPerl6::Runtime::Perl6::Gather;
    require KindaPerl6::Runtime::Perl5::Gather; # Must be after Runtime::Perl5::Array
};

# load the runtime

for ( qw( IO Math Kp6Security Range ) ) {
    eval "require KindaPerl6::Runtime::Perl5::$_";
    warn "*** Could not load runtime class $_: $@" if $@;
}
for ( qw( IO Math Multi Junction Range ) ) {
    eval "require KindaPerl6::Runtime::Perl6::$_";
    warn "*** Could not load runtime class $_: $@" if $@;
}

require KindaPerl6::Runtime::Perl6::Prelude;

use KindaPerl6::Runtime::Perl5::MP6Runtime;
use KindaPerl6::Runtime::Perl5::Pad;
use KindaPerl6::Runtime::Perl5::Wrap;
use KindaPerl6::Runtime::Perl5::GLOBAL;
use KindaPerl6::Runtime::Perl5::Grammar;
use KindaPerl6::Runtime::Perl6::Grammar;

use KindaPerl6::Grammar;
use KindaPerl6::Ast;
use KindaPerl6::Runtime::Perl5::Compiler;

1;

__END__

=head1 NAME

KindaPerl6::Perl5::Runtime

=head1 DESCRIPTION

Provides runtime routines for the KindaPerl6-in-Perl5 compiled code

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 COPYRIGHT

Copyright 2007 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
