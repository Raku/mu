package v6;
$v6::VERSION = '0.023';

# Documentation in the __END__
use 5.006;
use strict;
use warnings;
use Module::CompileV6-base;
use File::Basename;
# use Pugs::Runtime::Perl6;

binmode(STDOUT, ":utf8");
binmode(STDERR, ":utf8");
binmode(STDIN,  ":utf8");

my $backend_identifier = 'perl5';
my $grammar_identifier;

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

sub banner { 
q(         ___ 
        /  __\
 __    _\ \___      _____   _ ___ ___
/\ \  / /\  __ \   /\  __ \/\  __  __ \
\ \ \/ /\ \ \_\ \__\ \ \_\ \ \ \/\ \/\ \
 \ \__/  \ \____/\_).q(\\).q(\ \  __/\ \_\ \_\ \_\  Perl6-in-Perl5
  \/_/    \ ___/\/_/ \ \ \/  \/_/\/_/\/_/  
                      \ \_\              Version: ) . $v6::VERSION . q(
                       \/_/   Copyright 2006, The Pugs Contributors
--------------------------------------------------------------------
 Web: http://pugscode.org/           Email: perl6-compiler@perl.org

);
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

    my $p6 = Pugs::Compiler::Perl6->compile( $source, { backend => $backend_identifier, grammar => $grammar_identifier } );
    my $perl5 = $p6->{perl5};

    # Don't write when we failed to compile, otherwise it never recompiles!
    die unless defined $perl5 
            && length  $perl5;

    # $perl5 =~ s/do\{(.*)\}/$1/s;
    my ($package, $file) = caller(4);

    # allow 'regex' declarations:
    #     use base 'Pugs::Grammar::Base';  
    #   see: t/rules/from_perl6_rules/capture.t

    # postprocess perl5 backend
    if (  $backend_identifier eq 'perl5'
       || $backend_identifier eq 'perl5:Pugs::Emitter::Perl6::Perl5'
       ) {

    $perl5 = 
        ( $package 
            ? "package $package;\n" 
            : "package Main; # no package name\n" ).
        ((!$package or ($package eq 'main')) ? (
            "use Config;\n".
            "use lib split(/\\Q\$Config{path_sep}/, \$ENV{PERL6LIB} || '');\n"
        ) : '').
        "use Scalar::Util;
         use Pugs::Runtime::Perl6;
         use Pugs::Runtime::Perl6Prelude;
         use Pugs::Runtime::Perl5Container;
         use base 'Pugs::Grammar::Base';  
         use strict;
         no strict 'refs';
         no warnings ('void', 'uninitialized');
         \$::_V6_COMPILER_OS      = '$^O';
         \$::_V6_COMPILER_NAME    = 'v6.pm';
         \$::_V6_COMPILER_VERSION = '$v6::VERSION';
         undef \$::_V6_MATCH_;
         my \%_V6_PAD;
         our \%_V6_STATE;
        " .  
        # "Pugs::Runtime::Perl6Prelude->import();\n" .   # XXX - is import() needed?
        $perl5 . "\n" .
        "; 1;\n";

    if ( $ENV{V6TIDY} )
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

    }  # / postprocess perl5 backend

    return $perl5;
}

# print "ARGS: \$0='$0' \@ARGV=[ @ARGV ]\n";

if ( @ARGV && !caller ) {
    # We are the main program here
    my ($compile_only, $code);

    if ( @ARGV && $ARGV[0] eq '--compile-only') {
        shift(@ARGV);
        $compile_only++;
    }

    shift(@ARGV) if $ARGV[0] =~ /^--pugs/;
    if ( @ARGV && $ARGV[0] =~ /^-B(.*)/i ) {
        shift(@ARGV);
        $backend_identifier = $1 if $1;
    }
    splice(@ARGV, 0, 2) if @ARGV && $ARGV[0] =~ /^-B$/;

    if ( @ARGV && $ARGV[0] =~ /^-G(.*)/i ) {
        shift(@ARGV);
        $grammar_identifier = $1 if $1;
    }

    while (@ARGV && $ARGV[0] =~ /^-(\w)(.+)/) {
        use Config;
        if($1 eq 'I') {
            $ENV{PERL6LIB} = ((defined($ENV{PERL6LIB}) && $ENV{PERL6LIB} ne '')
                              ? "$Config{path_sep}$ENV{PERL6LIB}"
                              : "");
            $ENV{PERL6LIB} = "$2$ENV{PERL6LIB}";
        }
        shift @ARGV;
    }

    if (@ARGV && $ARGV[0] eq '-v' ) {
        print banner();
        exit 0;
    }

    if (@ARGV && $ARGV[0] =~ s/^-e//) {
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
elsif ( $0 eq '-e' && @ARGV && $ARGV[-1] eq '-v' ) {
    print banner();
    exit 0;
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
    if ( @ARGV && $ARGV[0] =~ /^-B(.*)/i ) {
        shift(@ARGV);
        $backend_identifier = $1 if $1;
    }
    splice(@ARGV, 0, 2) if $ARGV[0] && $ARGV[0] =~ /^-B$/;

    if ( @ARGV && $ARGV[0] =~ /^-G(.*)/i ) {
        shift(@ARGV);
        $grammar_identifier = $1 if $1;
    }

    while (@ARGV && $ARGV[0] =~ /^-(\w)(.+)/) {
        use Config;
        if($1 eq 'I') {
            $ENV{PERL6LIB} = ((defined($ENV{PERL6LIB}) && $ENV{PERL6LIB} ne '')
                              ? "$Config{path_sep}$ENV{PERL6LIB}"
                              : "");
            $ENV{PERL6LIB} = "$2$ENV{PERL6LIB}";
        }
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

Print the version banner:

    $ perl -e 'use v6-alpha' - -v
    $ perl lib/v6.pm -v

=head1 DESCRIPTION

The C<v6> module is a front-end to the experimental Perl6-to-Perl5 compiler.

The current state of this compiler implementation only provides a small sample of
Perl 6 syntax and semantics.

=head2 Other Perl 6 implementations

C<Pugs> Perl 6 is currently the most complete implementation. Pugs is written on top of Haskell.

Perl 6 on C<Parrot> aims to become the best performing implementation.
Parrot is a virtual machine designed to efficiently compile and execute bytecode for interpreted languages.

C<v6.pm> is completely independent of Pugs or Parrot. 

=head2 Compile-time system

L<Pugs::Compiler::Rule> provides an implementation for Perl 6 Rules, which are used to define the Perl 6 grammar.

L<Parse::Yapp> is used for implementing the operator precedence parser.

L<Module::Compile> and L<Cache::Cache> provide the precompilation cache
infrastructure.

=head2 Runtime system

The object system is provided by L<Moose>. 
Moose is an extension of the Perl 5 object system.
Moose is built on top of L<Class::MOP>, which is a metaclass system for Perl 5.

L<Data::Bind> and L<Sub::Multi> implement the semantics for perl6-style variable binding, as well as subroutine call argument passing and binding, in Perl 5.

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

* V6TIDY

If set, the compiler output will be much more readable, but there will
be a lot of slowdown in compiler speed.

=head1 COMMAND LINE SWITCHES

* --compile-only

When using C<v6.pm> from the command line, dumps the emitted code
to C<STDOUT> and then exit:

    $ perl -e 'use v6-alpha' - --compile-only ' 42.say '

    $ perl -Ilib lib/v6.pm --compile-only -e '<hello>.say;'

* -B

Selects alternate code generation backends.

The default is '-Bperl5:Pugs::Emitter::Perl6::Perl5'.
'-Bperl5' also invokes the default backend.

    $ perl -e 'use v6-alpha' - --compile-only -Bperl5:MyEmitter ' 42.say '

The backend module must provide the C<emit($grammar, $ast)> subroutine.

* -G

Selects alternate grammar frontends.

The default is '-Gperl5:Pugs::Grammar::Perl6'.

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
