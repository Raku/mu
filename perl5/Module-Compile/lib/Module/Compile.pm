package Module::Compile; 

use 5.006;
use strict;
use warnings;

our $VERSION = '0.11';

# A lexical hash to keep track of which files have already been filtered
my $filtered = {};

# All subroutines are prefixed with pmc_ so subclasses don't accidentally
# override things they didn't intend to.

# Determine which stack frame points to the code we are filtering.
# This is a method in case it needs to be overridden.
sub pmc_stack_frame { 0 };

# This is called while parsing/chunking source code to determine if the
# module/class in a use/no line is part of the Module::Compile game.
#
# Return true if this class supports PMC compilation.
#
# The hope is that this will allow interoperability with modules that
# do not inherit from Module::Compile but still want to do this sort
# of thing.
sub pmc_compiler { 1 };

# All Module::Compile based modules inherit this import routine.
sub import {
    my ($class, $flag) = @_;

    $class->pmc_set_base($flag || '') and return;

    my ($module, $line) = (caller($class->pmc_stack_frame))[1, 2];

    return if $filtered->{$module}++;

    my $callback = sub {
        my $content = shift;
        my $preface = $class->pmc_preface($module, $line);
        my $output = $class->pmc_template($preface, $content);
        $class->pmc_output($module, $output);
    };

    $class->pmc_filter($callback);
}

# Set up inheritance
sub pmc_set_base {
    my ($class, $flag) = @_;
    if ($class eq __PACKAGE__ and $flag and $flag eq '-base') {
        no strict 'refs';
        push @{(caller(1))[0] . '::ISA'}, __PACKAGE__;
        return 1;
    }
    return;
}

# Get the code before the first source filter call
sub pmc_preface {
    my ($class, $module, $line) = @_;
    my $preface = '';
    local $/ = "\n";
    open INPUT, $module
      or die "Can't open '$module' for input:\n$!";
    for (1 .. ($line - 1)) {
        $preface .= <INPUT>;
    }
    return $preface;
}

# Generate the actual code that will go into the .pmc file.
sub pmc_template {
    my ($class, $preface, $content) = @_;
    my $check = $class->freshness_check();
    return <<"...";
# Generated .pmc file - do not edit!
$check$preface$content
...
}

# This returns a piece of Perl code to do a runtime check to see if the
# .pmc file is fresh.
sub freshness_check {
    my $time = gmtime;
    return '';
}

# Write the output to the .pmc file
# If we can't open the file, just return. The filtering will not be cached,
# but that might be ok.
sub pmc_output {
    my ($class, $module, $output) = @_;
    my $pmc = $module . 'c';
    open OUTPUT, "> $pmc"
      or return;
    print OUTPUT $output;
    close OUTPUT;
}

# We use a source filter to get all the code for compiling.
sub pmc_filter {
    my ($class, $post_process) = @_;
    my $done = 0;
    require Filter::Util::Call;
    Filter::Util::Call::filter_add(sub {
        return 0 if $done;
        my $data = "use $class;\n";
        my $end = '';
        while (my $status = Filter::Util::Call::filter_read()) {
            return $status if $status < 0;
            if (/^__(?:END|DATA)__\r?$/) {
                $end = $_;
                last;
            }
            $data .= $_;
            $_ = '';
        }
        my $filtered = $class->pmc_process($data);
        chomp($filtered);
        $filtered .= "\n$end";
        $post_process->($filtered);
        $_ = $filtered;

        $done = 1;
    });
}

# Break the code into chunks. Compile the chunks.
sub pmc_process {
    my $class = shift;
    my $data = shift;
    my @chunks = $class->pmc_chunk($data);
    while (@chunks = $class->pmc_reduce(@chunks)) {
        if (@chunks == 1 and @{$chunks[0][1]} == 0) {
            return $chunks[0][0];
        }
    }
    die "How did I get here?!?";
}

# Analyze the remaining chunks and determine which compilers to call to reduce
# the problem.
#
# XXX This routine must do some kind of reduction each pass, or infinite loop
# will ensue. It is not yet certain if this is the case.
sub pmc_reduce {
    my $class = shift;
    my @chunks;
    my $prev;
    while (@_) {
        my $chunk = shift;
        my $next = $_[0];
        if ($next and "@{$chunk->[1]}" eq "@{$next->[1]}") {
            shift;
            $chunk->[0] .= $next->[0];
        }
        elsif ( 
            (not $prev or @{$prev->[1]} < @{$chunk->[1]}) and
            (not $next or @{$next->[1]} < @{$chunk->[1]})
        ) {
            my $prev_len = $prev ? @{$prev->[1]} : 0;
            my $next_len = $next ? @{$next->[1]} : 0;
            my $offset = ($prev_len > $next_len) ? $prev_len : $next_len;
            my $length = @{$chunk->[1]} - $offset;
            $class->pmc_call($chunk, $offset, $length);
        }
        push @chunks, $chunk;
        $prev = $chunk;
    }
    return @chunks;
}

# Call a set of compilers on a piece of source code.
sub pmc_call {
    my $class = shift;
    my $chunk = shift;
    my $offset = shift;
    my $length = shift;

    my $text = $chunk->[0];
    my @classes = splice(@{$chunk->[1]}, $offset, $length);
    for my $c (@classes) {
        $_ = $text;
        my $return = $c->pmc_compile($text);
        $text = (defined $return and $return !~ /^\d+$/)
            ? $return
            : $_;
    }
    $chunk->[0] = $text;
}

# Divide a Perl module into chunks. This code divides a module based on lines
# that use/no a Module::Compile subclass.
sub pmc_chunk {
    my $class = shift;
    my $data = shift;
    my @parts = split /^(\s*(?:use|no)\s+[\w\:]+.*\n)/m, $data;
    my @chunks = ();
    my @classes = ();
    my $text = '';
    while (@parts) {
        my $part = shift @parts;
        if ($part =~ /^\s*(use|no)\s+([\w\:]+).*\n/) {
            my ($use, $klass, $file) = ($1, $2, $2);
            $file =~ s{::}{/}g;
            {
                local $@;
                eval { require "$file.pm" };
                die $@ if $@ and "$@" !~ /^Can't locate /;
            }
            if (not ($klass->can('pmc_compiler') and $class->pmc_compiler)) {
                $text .= $part;
            }
            else {
                push @chunks, [$text, [@classes]];
                $text = '';
                if ($use eq 'use') {
                    push @classes, $klass
                      unless grep {$_ eq $klass} @classes;
                }
                else {
                    @classes = grep {$_ ne $klass} @classes;
                }
            }
        }
        else {
            $text .= $part;
        }
    }
    push @chunks, [$text, [@classes]]
        if length $text;
    return @chunks;
}

# Compile/Filter some source code into something else. This is almost
# always overridden in a subclass.
sub pmc_compile {
    my ($class, $source) = @_;
    return $source;
}

1;

__END__

=head1 NAME

Module::Compile - Perl Module Compilation

=head1 SYNOPSIS

    package Foo;
    use Module::Compile -base;

    sub pmc_compile {
        my ($class, $source) = @_;
        # Convert $source into (most likely Perl 5) $compiled_output
        return $compiled_output;
    }

In F<Bar.pm>:
  
    package Bar;
    
    use Foo;
    ...
    no Foo

To compile F<Bar.pm> into F<Bar.pmc>:

    perl -c Bar.pm

=head1 DESCRIPTION

This module provides a system for writing modules that I<compile> other
Perl modules.

Modules that use these compilation modules get compiled into some
altered form the first time they are run. The result is cached into
C<.pmc> files.

Perl has native support for C<.pmc> files. It always checks for them, before
loading a C<.pm> file.

You get the following benefits:

=head1 EXAMPLE

You can declare a C<v6.pm> compiler with:

    package v6;
    use Module::Compile -base;
    
    sub pmc_compile {
        my ($class, $source) = @_;
        # ... some way to invoke pugs and give p5 code back ...
    }

and use it like:

    # MyModule.pm
    use v6-pugs;
    module MyModule;
    # ...some p6 code here...
    no v6;
    # ...back to p5 land...

On the first time this module is loaded, it will compile Perl 6
chunks into Perl 5 (as soon as the C<no v6> line is seen), and
merge it with the Perl 5 chunks, saving the result into a
F<MyModule.pmc> file.

The next time around, Perl 5 will automatically load F<MyModule.pmc>
when someone says C<use MyModule>. On the other hand, Perl 6 can run
MyModule.pm s a Perl 6 module just fine, as C<use v6-pugs> and C<no v6>
both works in a Perl 6 setting.

The B<v6.pm> module will also check if F<MyModule.pmc> is up to date. If
it is, then it will touch its timestamp so the C<.pmc> is loaded on the
next time.

=head1 BENEFITS

Module::Compile compilers gives you the following benefits:

=over

=item *

Ability to mix many source filterish modules in a much more sane manner.
Module::Compile controls the compilation process, calling each compiler
at the right time with the right data.

=item *

Ability to ship precompiled modules without shipping Module::Compile and
the compiler modules themselves.

=item *

Easier debugging of compiled/filtered code. The C<.pmc> has the real
code you want to see.

=item *

Zero runtime penalty after compilation, because C<perl> has already been
doing the C<.pmc> check on every module load since 1999!

=back

=head1 AUTHORS

Audrey Tang <autrijus@autrijus.org>

Ingy döt Net <ingy@cpan.org>

=head1 COPYRIGHT

Copyright (c) 2006. Ingy döt Net. All rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
