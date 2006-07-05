package v6;
$v6::VERSION = '0.005';

# Documentation in the __END__
use 5.006;
use strict;
use warnings;
use Module::Compile-base;
use File::Basename;
use Pugs::Runtime::Perl6;

my $bin;
BEGIN { $bin = ((dirname(__FILE__) || '.') . "/..") };

sub pmc_can_output { 1 }

sub pmc_parse_blocks {
    my $class = shift;
    my $text  = shift;
    return [$text, {$class => { use => 'dummy' }}, [$class]]
}

sub pmc_filter {
    my ($class, $module, $line_number, $post_process) = @_;
    $class->SUPER::pmc_filter($module, 0, $post_process);
}

sub pmc_compile {
    my ($class, $source) = @_;

    require Pugs::Compiler::Perl6;

    my $p6 = Pugs::Compiler::Perl6->compile( $source );
    my $perl5 = $p6->{perl5};

    # Don't write when we failed to compile, otherwise it never recompiles!
    die unless length $perl5;

    # $perl5 =~ s/do\{(.*)\}/$1/s;
    my ($package, $file) = caller(4);
    $perl5 = 
        ( $package ? "package $package;\n" : "# no package name\n" ).
        ((!$package or ($package eq 'main')) ? (
            "use Config;\n".
            "use lib split(/\\Q\$Config{path_sep}/, \$ENV{PERL6LIB} || '');\n"
        ) : '').
        "use Scalar::Util;\n" .
        "use Pugs::Runtime::Perl6;\n" . 
        "use strict;\n" . 
        "no warnings 'void';\n" .   # t/07-try.t, t/07-ref.t
        $perl5 . "\n" .
        "; 1;\n";

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

v6-pugs - an experimental Perl 6 implementation

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

The C<v6-pugs> module is a front-end to the experimental Perl6-to-Perl5 compiler.

The current state of this compiler implementation only provides a small sample of
Perl 6 syntax and semantics.

=head2 Other Perl 6 implementations

The Pugs/Haskell Perl 6 is currently the most complete implementation. 
Pugs currently has some issues with Perl 5 interoperability.

Parrot Perl 6 is the best performing implementation by far.
The Parrot implementation is currently at a comparable state as v6.pm.

Although running C<v6-pugs> requires the installation of a lot of Perl 5 modules,
it is completely independent of Pugs or Parrot. 

=head1 REQUIREMENTS

- The source file header must be valid perl5 I<and> perl6 code.

This is a valid header:

    #!/usr/bin/perl
    use v6-pugs;

* it executes perl5

* perl5 will call the C<v6.pm> module.

This is an invalid header:

    #!/usr/bin/pugs
    use v6;

* it tells perl5 to execute C</usr/bin/pugs>.

* it tells perl5 that Perl v6.0.0 required.

- The C<Pugs::Compiler::Rule> module must be properly installed.

An improperly installed C<Pugs::Compiler::Rule> module would prevent the Perl 6 compiler
from bootstrapping. 

If that is the case, running C<Makefile.PL> and C<make> in
C<Pugs::Compiler::Rule> should fix the problem.

- The perl5 executable must have PMC support. 

PMC support is required for loading precompiled Perl 6 files.

If you see the error below, it may happen that your perl was compiled without PMC support.

  Can't locate object method "compile" via package "Pugs::Compiler::Perl6"

Please see L<http://rt.cpan.org/Public/Bug/Display.html?id=20152>

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

- the Perl 6 Synopsis: L<http://dev.perl.org/perl6/doc/synopsis.html>.

The Pugs homepage at L<http://pugscode.org/>.

The Parrot homepage at L<http://www.parrotcode.org>.

=head1 COPYRIGHT

Copyright 2006 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
