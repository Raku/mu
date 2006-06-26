package v6;
$v6::VERSION = '0.01_001';

# Documentation in the __END__
use 5.006;
use strict;
use warnings;
use Module::Compile-base;
use File::Basename;
use Pugs::Runtime::Perl6;

my $bin;
BEGIN { $bin = ((dirname(__FILE__) || '.') . "/..") };
use lib (
    "$bin/lib",
    "$bin/../Pugs-Compiler-Rule/lib",
);

sub pmc_can_output { 1 }

sub pmc_compile {
    my ($class, $source) = @_;

    my $file = (caller(4))[1];
    if (defined $file and $file !~ /\.pm$/i) {
        # Do the freshness check ourselves
        my $pmc = $file.'c';
        my $pmc_is_uptodate = (-s $pmc and (-M $pmc <= -M $file));
        if ($pmc_is_uptodate) {
            local $@; do $pmc; die $@ if $@; exit 0;
        }
    }

    require Pugs::Compiler::Perl6;

    my $p6 = Pugs::Compiler::Perl6->compile( $source );
    my $perl5 = $p6->{perl5};

    # $perl5 =~ s/do\{(.*)\}/$1/s;
    $perl5 = 
        "use Pugs::Runtime::Perl6;\n" . 
        "use strict;\n" . 
        #"no warnings 'array';\n" . 
        $perl5 . "\n";

    {
      # Perl::Tidy is used if available
      local $@;   # don't care if there are errors here
      local @ARGV = ();  # "You may not specify any filenames ... - Perl::Tidy.pm
      eval {
        require Perl::Tidy;
        my $perl5_tidy;
        Perl::Tidy::perltidy( source => \$perl5, destination => \$perl5_tidy );
        $perl5 = $perl5_tidy;
      }
    }

    return $perl5;
}

if (@ARGV and !caller) {
    # We are the main program here
    my ($compile_only, $code);

    if ($ARGV[0] eq '--compile-only') {
        shift(@ARGV);
        $compile_only++;
    }

    shift(@ARGV) if $ARGV[0] =~ /^--pugs/;
    shift(@ARGV) if $ARGV[0] =~ /^-Bperl5$/i;
    splice(@ARGV, 0, 2) if $ARGV[0] =~ /^-B$/;

    if (@ARGV and $ARGV[0] =~ s/^-e//) {
        $code = (length($ARGV[0]) ? $ARGV[0] : $ARGV[1]);
    }
    else {
        local $/;
        $code = <>;
    }

    if ($compile_only) {
        print __PACKAGE__->pmc_compile($code);
    }
    else {
        local $@;
        eval __PACKAGE__->pmc_compile($code);
        die $@ if $@;
        exit 0;
    }
}

1;

__END__

=head1 NAME 

v6 - an experimental Perl 6 implementation (under way)

=head1 SYNOPSIS

Command line:

    $ perl -Ilib lib/v6.pm -e ' for 1,2,3 -> $x { say $x }'

Compile-only:

    $ perl -Ilib lib/v6.pm --compile-only -e ' <hello>.say; '

Script or module:

    # file: hello_world.pl
    use v6-pugs;
    "hello, world".say;

    $ perl hello_world.pl

=head1 DESCRIPTION

The C<v6> module is a front-end to the experimental Perl6-to-Perl5 compiler.

The current state of this compiler implementation only provide a small sample of
Perl 6 syntax and semantics.

=head2 Other Perl 6 implementations

The Pugs/Haskell Perl 6 is currently the most complete implementation. 
Pugs currently has some issues with Perl 5 interoperability.

Parrot Perl 6 is the best performing implementation by far.
The Parrot implementation is currently at a comparable state as v6.pm.

Although running C<v6> requires the installation of a lot of Perl 5 modules,
it is completely independent of Pugs or Parrot. 

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 Synopsis: L<http://dev.perl.org/perl6/doc/synopsis.html>

=head1 COPYRIGHT

Copyright 2006 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
