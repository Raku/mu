
use v5;
use strict;
#use FindBin;

use KindaPerl6::Runtime::Perl5::MOP;
use KindaPerl6::Runtime::Perl5::Match;

# try to load gather/take (depends on 'Coro')
eval {
    require KindaPerl6::Runtime::Perl6::Gather;
    require KindaPerl6::Runtime::Perl5::Gather;
};

# load the runtime

for ( qw( IO Math Kp6Security ) ) {
    eval "require KindaPerl6::Runtime::Perl5::$_";
    warn "*** Could not load runtime class $_: $@" if $@;
}
for ( qw( IO Math Multi Junction Range ) ) {
    eval "require KindaPerl6::Runtime::Perl6::$_";
    warn "*** Could not load runtime class $_: $@" if $@;
}

#my $libpath  = $FindBin::Bin."/lib";
#my $runtime5 = 'KindaPerl6/Runtime/Perl5';
#my $runtime6 = 'KindaPerl6/Runtime/Perl6';
#my @runtime5 = <$libpath/$runtime5/{IO,Math,Kp6Security}.pm>;
#my @runtime6 = <$libpath/$runtime6/{IO,Math,Multi,Junction,Range}.pm>;

#foreach (map { s,^.*($runtime5/.*)\.pm,$1,; s,/,::,g; $_ } @runtime5) {
#    eval "require $_";
#    warn "*** Could not load runtime class $_" if $@;
#}
#foreach (map { s,^.*($runtime6/.*)\.pm,$1,; s,/,::,g; $_ } @runtime6) {
#    eval "require $_";
#    warn "*** Could not load runtime class $_" if $@;
#}

require KindaPerl6::Runtime::Perl6::Prelude;

use KindaPerl6::Runtime::Perl5::MP6Runtime;
use KindaPerl6::Runtime::Perl5::Pad;
use KindaPerl6::Runtime::Perl5::Wrap;
use KindaPerl6::Runtime::Perl5::GLOBAL;
use KindaPerl6::Runtime::Perl5::Grammar;

1;

__END__

=pod

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
