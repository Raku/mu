package v6;
$v6::VERSION = '0.014';

# Documentation in the __END__
use 5.006;
use strict;
use warnings;
use Module::Compile-base;
use File::Basename;
# use Pugs::Runtime::Perl6;

my $bin;
BEGIN { $bin = ((dirname(__FILE__) || '.') . "/..") };

INIT {
  if($ENV{V6_RESOURCE_GUARD}) {
    require BSD::Resource;
    import BSD::Resource;
    setrlimit(RLIMIT_CPU(), 30, 60) or die "Couldn't setrlimit: $!\n";
    setrlimit(RLIMIT_RSS(), 1048000, 1196000) or die "Couldn't setrlimit: $!\n";
  }
}

sub pmc_can_output { 1 }

sub pmc_parse_blocks {
    my $class = shift;
    my $text  = shift;
    return [$text, {$class => { use => 'dummy' }}, [$class]]
}

sub pmc_filter {
    my ($class, $module, $line_number, $post_process) = @_;
    return 
        if $module eq '-e';
    $class->SUPER::pmc_filter($module, 0, $post_process);
}

sub pmc_compile {
    my ($class, $source) = @_;

    require Pugs::Compiler::Perl6;

    my $p6 = Pugs::Compiler::Perl6->compile( $source );
    my $perl5 = $p6->{perl5};

    # Don't write when we failed to compile, otherwise it never recompiles!
    die unless defined $perl5 
            && length  $perl5;

    # $perl5 =~ s/do\{(.*)\}/$1/s;
    my ($package, $file) = caller(4);
    $perl5 = 
        ( $package 
            ? "package $package;\n" 
            : "package main; # no package name\n" ).
        ((!$package or ($package eq 'main')) ? (
            "use Config;\n".
            "use lib split(/\\Q\$Config{path_sep}/, \$ENV{PERL6LIB} || '');\n"
        ) : '').
        "use Scalar::Util;
         use Pugs::Runtime::Perl6;
         use Pugs::Runtime::Perl6Prelude;
         use strict;
         no strict 'refs';
         no warnings ('void', 'uninitialized');
         \$::_V6_COMPILER_OS      = '$^O';
         \$::_V6_COMPILER_NAME    = 'v6.pm';
         \$::_V6_COMPILER_VERSION = '$v6::VERSION';
         my \%_V6_PAD;
         our \%_V6_STATE;
        " .  
        # "Pugs::Runtime::Perl6Prelude->import();\n" .   # XXX - is import() needed?
        $perl5 . "\n" .
        "; 1;\n";

    unless ( $ENV{V6NOTIDY} )
    {
      # Perl::Tidy is used if available
      local $@;   # don't care if there are errors here
      local @ARGV = ();  # "You may not specify any filenames ... - Perl::Tidy.pm
      eval {
        require Perl::Tidy;
        my $perl5_tidy;
        Perl::Tidy::perltidy( 
            source => \$perl5, 
            destination => \$perl5_tidy,
            argv => [
                '--maximum-line-length' => 0,
                '--indent-columns'      => 2,
            ],
        );
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

    while (@ARGV and $ARGV[0] =~ /^-(\w)(.+)/) {
        use Config;
        $ENV{PERL6LIB} = "$2$Config{path_sep}$ENV{PERL6LIB}" if $1 eq 'I';
        shift @ARGV;
    }

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
elsif ( $0 eq '-e' ) {
    # perl -Ilib -e 'use v6-alpha' ' "hello world".say '
    # perl -Ilib -e 'use v6-alpha' - ' "hello world".say '
    # perl -Ilib -e 'use v6-alpha' - --compile-only ' "hello world".say '
    # perl -Ilib -e 'use v6-alpha' - -Ilib6 ' "hello world".say '
    # echo 42.say | perl -Ilib -e 'use v6-alpha' 
    
    #print "Compile [$0] [@ARGV]\n";
    my ($compile_only, $code);

    shift(@ARGV) if $ARGV[0] && $ARGV[0] eq '-';

    if ($ARGV[0] && $ARGV[0] eq '--compile-only') {
        shift(@ARGV);
        $compile_only++;
    }

    shift(@ARGV) if $ARGV[0] && $ARGV[0] =~ /^--pugs/;
    shift(@ARGV) if $ARGV[0] && $ARGV[0] =~ /^-Bperl5$/i;
    splice(@ARGV, 0, 2) if $ARGV[0] && $ARGV[0] =~ /^-B$/;

    while (@ARGV and $ARGV[0] =~ /^-(\w)(.+)/) {
        use Config;
        $ENV{PERL6LIB} = "$2$Config{path_sep}$ENV{PERL6LIB}" if $1 eq 'I';
        shift @ARGV;
    }

    if (@ARGV) {  # and $ARGV[0] =~ s/^-e//) {
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

v6 - An experimental Perl 6 implementation

=head1 SYNOPSIS

    # file: hello_world.pl
    use v6-alpha;
    "hello, world".say;

    $ perl hello_world.pl

C<v6-alpha> can be used in one-liners:

    $ perl -e 'use v6-alpha' ' 42.say '
    $ perl -e 'use v6-alpha' - ' 42.say '
    $ perl -e 'use v6-alpha' - --compile-only ' 42.say '
    $ perl -e 'use v6-alpha' - -Ilib6 ' 42.say '
    $ echo 42.say | perl -e 'use v6-alpha' 

C<v6.pm> can also be used as a plain program. 
This examples assume that v6.pm is in the C<./lib> directory:

    $ perl lib/v6.pm -e 'for 1,2,3 -> $x { say $x }'
    $ perl lib/v6.pm --compile-only -e '<hello>.say;'

=head1 DESCRIPTION

The C<v6> module is a front-end to the experimental Perl6-to-Perl5 compiler.

The current state of this compiler implementation only provides a small sample of
Perl 6 syntax and semantics.

=head2 Other Perl 6 implementations

The Pugs/Haskell Perl 6 is currently the most complete implementation. 
Pugs currently has some issues with Perl 5 interoperability.

Parrot Perl 6 aims to become the best performing implementation, but
currently is less complete than both C<v6.pm> and Pugs/Haskell.

Although running C<v6.pm> requires the installation of a lot of Perl 5 modules,
it is completely independent of Pugs or Parrot. 

=head1 REQUIREMENTS

- The source file header must be valid perl5 I<and> perl6 code.

This is a valid header:

    #!/usr/bin/perl
    use v6-alpha;

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

=head1 ENVIRONMENT VARIABLES

* PERL6LIB

Same usage as PERL5LIB - sets the search path for Perl 6 modules.

* V6DUMPAST

If set, the compiler will dump the syntax tree to C<STDOUT> just before emitting code, using C<Data::Dumper>.

* V6NOTIDY

If set, the compiler output will be much less readable, but there will
be some improvement in compiler speed.

=head1 COMMAND LINE SWITCHES

* --compile-only

When using v6.pm from the command line, dumps the emitted code
to C<STDOUT> and then exit:

    $ perl -Ilib lib/v6.pm --compile-only -e '<hello>.say;'

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
