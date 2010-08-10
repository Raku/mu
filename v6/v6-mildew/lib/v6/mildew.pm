package v6::mildew;
$v6::VERSION = '0.032';
# ABSTRACT: allows running mildew from perl5 using use v6-mildew
use v5.10;
use strict;
use warnings;
use Module::CompileV6-base;
use File::Basename;
use File::Temp qw(tmpnam);
use String::Escape qw(printable);
use SMOP;

binmode(STDOUT, ":utf8");
binmode(STDERR, ":utf8");
binmode(STDIN,  ":utf8");

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

    my ($package, $file) = caller(4);

    require Mildew::Backend::C::V6;
    require Mildew::Frontend::STD;
    require Mildew::Compiler;


    my $compiler = Mildew::Compiler->new(
        backend=>Mildew::Backend::C::V6->new(cflags=>[SMOP::include_flags()]),
        frontend=>Mildew::Frontend::STD->new());

    my $so = tmpnam;
    say "compiling <<$source>> to $so";
    $compiler->compile($source,$so);

    my $perl5 = 'use V6::mildew::Runtime;V6::mildew::Runtime::load("'.printable($so).'");1;';

    # Don't write when we failed to compile, otherwise it never recompiles!
    die unless defined $perl5 
            && length  $perl5;


    return $perl5;
}


1;
