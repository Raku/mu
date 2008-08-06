#. TODO:
#.

#===============================================================================
# This is the default class for handling Test::Base data filtering.
#===============================================================================
package Test::Base::Filter;
use Spiffy -Base;
use Spiffy ':XXX';

field 'current_block';

our $arguments;
sub current_arguments {
    return undef unless defined $arguments;
    my $args = $arguments;
    $args =~ s/(\\s)/ /g;
    $args =~ s/(\\[a-z])/'"' . $1 . '"'/gee;
    return $args;
}

sub assert_scalar {
    return if @_ == 1;
    require Carp;
    my $filter = (caller(1))[3];
    $filter =~ s/.*:://;
    Carp::croak "Input to the '$filter' filter must be a scalar, not a list";
}

sub _apply_deepest {
    my $method = shift;
    return () unless @_;
    if (ref $_[0] eq 'ARRAY') {
        for my $aref (@_) {
            @$aref = $self->_apply_deepest($method, @$aref);
        }
        return @_;
    }
    $self->$method(@_);
}

sub _split_array {
    map {
        [$self->split($_)];
    } @_;
}

sub _peel_deepest {
    return () unless @_;
    if (ref $_[0] eq 'ARRAY') {
        if (ref $_[0]->[0] eq 'ARRAY') {
            for my $aref (@_) {
                @$aref = $self->_peel_deepest(@$aref);
            }
            return @_;
        }
        return map { $_->[0] } @_;
    }
    return @_;
}

#===============================================================================
# these filters work on the leaves of nested arrays
#===============================================================================
sub Join { $self->_peel_deepest($self->_apply_deepest(join => @_)) }
sub Reverse { $self->_apply_deepest(reverse => @_) }
sub Split { $self->_apply_deepest(_split_array => @_) }
sub Sort { $self->_apply_deepest(sort => @_) }


sub append {
    my $suffix = $self->current_arguments;
    map { $_ . $suffix } @_;
}

sub array {
    return [@_];
}

sub base64_decode {
    $self->assert_scalar(@_);
    require MIME::Base64;
    MIME::Base64::decode_base64(shift);
}

sub base64_encode {
    $self->assert_scalar(@_);
    require MIME::Base64;
    MIME::Base64::encode_base64(shift);
}

sub chomp {
    map { CORE::chomp; $_ } @_;
}

sub chop {
    map { CORE::chop; $_ } @_;
}

sub dumper {
    no warnings 'once';
    require Data::Dumper;
    local $Data::Dumper::Sortkeys = 1;
    local $Data::Dumper::Indent = 1;
    local $Data::Dumper::Terse = 1;
    Data::Dumper::Dumper(@_);
}

sub escape {
    $self->assert_scalar(@_);
    my $text = shift;
    $text =~ s/(\\.)/eval "qq{$1}"/ge;
    return $text;
}

sub eval {
    $self->assert_scalar(@_);
    my @return = CORE::eval(shift);
    return $@ if $@;
    return @return;
}

sub eval_all {
    $self->assert_scalar(@_);
    my $out = '';
    my $err = '';
    Test::Base::tie_output(*STDOUT, $out);
    Test::Base::tie_output(*STDERR, $err);
    my $return = CORE::eval(shift);
    no warnings;
    untie *STDOUT;
    untie *STDERR;
    return $return, $@, $out, $err;
}

sub eval_stderr {
    $self->assert_scalar(@_);
    my $output = '';
    Test::Base::tie_output(*STDERR, $output);
    CORE::eval(shift);
    no warnings;
    untie *STDERR;
    return $output;
}

sub eval_stdout {
    $self->assert_scalar(@_);
    my $output = '';
    Test::Base::tie_output(*STDOUT, $output);
    CORE::eval(shift);
    no warnings;
    untie *STDOUT;
    return $output;
}

sub exec_perl_stdout {
    my $tmpfile = "/tmp/test-blocks-$$";
    $self->_write_to($tmpfile, @_);
    open my $execution, "$^X $tmpfile 2>&1 |"
      or die "Couldn't open subprocess: $!\n";
    local $/;
    my $output = <$execution>;
    close $execution;
    unlink($tmpfile)
      or die "Couldn't unlink $tmpfile: $!\n";
    return $output;
}

sub flatten {
    $self->assert_scalar(@_);
    my $ref = shift;
    if (ref($ref) eq 'HASH') {
        return map {
            ($_, $ref->{$_});
        } sort keys %$ref;
    }
    if (ref($ref) eq 'ARRAY') {
        return @$ref;
    }
    die "Can only flatten a hash or array ref";
}

sub get_url {
    $self->assert_scalar(@_);
    my $url = shift;
    CORE::chomp($url);
    require LWP::Simple;
    LWP::Simple::get($url);
}

sub hash {
    return +{ @_ };
}

sub head {
    my $size = $self->current_arguments || 1;
    return splice(@_, 0, $size);
}

sub join {
    my $string = $self->current_arguments;
    $string = '' unless defined $string;
    CORE::join $string, @_;
}

sub lines {
    $self->assert_scalar(@_);
    my $text = shift;
    return () unless length $text;
    my @lines = ($text =~ /^(.*\n?)/gm);
    return @lines;
}

sub norm {
    $self->assert_scalar(@_);
    my $text = shift;
    $text = '' unless defined $text;
    $text =~ s/\015\012/\n/g;
    $text =~ s/\r/\n/g;
    return $text;
}

sub prepend {
    my $prefix = $self->current_arguments;
    map { $prefix . $_ } @_;
}

sub read_file {
    $self->assert_scalar(@_);
    my $file = shift;
    CORE::chomp $file;
    open my $fh, $file
      or die "Can't open '$file' for input:\n$!";
    CORE::join '', <$fh>;
}

sub regexp {
    $self->assert_scalar(@_);
    my $text = shift;
    my $flags = $self->current_arguments;
    if ($text =~ /\n.*?\n/s) {
        $flags = 'xism'
          unless defined $flags;
    }
    else {
        CORE::chomp($text);
    }
    $flags ||= '';
    my $regexp = eval "qr{$text}$flags";
    die $@ if $@;
    return $regexp;
}

sub reverse {
    CORE::reverse(@_);
}

sub slice {
    die "Invalid args for slice"
      unless $self->current_arguments =~ /^(\d+)(?:,(\d))?$/;
    my ($x, $y) = ($1, $2);
    $y = $x if not defined $y;
    die "Invalid args for slice"
      if $x > $y;
    return splice(@_, $x, 1 + $y - $x);
}

sub sort {
    CORE::sort(@_);
}

sub split {
    $self->assert_scalar(@_);
    my $separator = $self->current_arguments;
    if (defined $separator and $separator =~ s{^/(.*)/$}{$1}) {
        my $regexp = $1;
        $separator = qr{$regexp};
    }
    $separator = qr/\s+/ unless $separator;
    CORE::split $separator, shift;
}

sub strict {
    $self->assert_scalar(@_);
    <<'...' . shift;
use strict;
use warnings;
...
}

sub tail {
    my $size = $self->current_arguments || 1;
    return splice(@_, @_ - $size, $size);
}

sub trim {
    map {
        s/\A([ \t]*\n)+//;
        s/(?<=\n)\s*\z//g;
        $_;
    } @_;
}

sub unchomp {
    map { $_ . "\n" } @_;
}

sub write_file {
    my $file = $self->current_arguments
      or die "No file specified for write_file filter";
    if ($file =~ /(.*)[\\\/]/) {
        my $dir = $1;
        if (not -e $dir) {
            require File::Path;
            File::Path::mkpath($dir)
              or die "Can't create $dir";
        }
    }
    open my $fh, ">$file"
      or die "Can't open '$file' for output\n:$!";
    print $fh @_;
    close $fh;
    return $file;
}

sub yaml {
    $self->assert_scalar(@_);
    require YAML;
    return YAML::Load(shift);
}

sub _write_to {
    my $filename = shift;
    open my $script, ">$filename"
      or die "Couldn't open $filename: $!\n";
    print $script @_;
    close $script
      or die "Couldn't close $filename: $!\n";
}

__DATA__

=head1 NAME

Test::Base::Filter - Default Filter Class for Test::Base

=head1 SYNOPSIS

    package MyTestSuite;
    use Test::Base -Base;

    ... reusable testing code ...

    package MyTestSuite::Filter;
    use Test::Base::Filter -Base;

    sub my_filter1 {
        ...
    }

=head1 DESCRIPTION

Filters are the key to writing effective data driven tests with Test::Base.
Test::Base::Filter is a class containing a large default set of generic
filters. You can easily subclass it to add/override functionality.

=head1 FILTERS

This is a list of the default stock filters (in alphabetic order):

=head2 append

list => list

Append a string to each element of a list.

    --- numbers lines chomp append=-#\n join
    one
    two
    three

=head2 array

list => scalar

Turn a list of values into an anonymous array reference.

=head2 base64_decode

scalar => scalar

Decode base64 data. Useful for binary tests.

=head2 base64_encode

scalar => scalar

Encode base64 data. Useful for binary tests.

=head2 chomp

list => list

Remove the final newline from each string value in a list.

=head2 chop

list => list

Remove the final char from each string value in a list.

=head2 dumper

scalar => list

Take a data structure (presumably from another filter like eval) and use
Data::Dumper to dump it in a canonical fashion.

=head2 escape

scalar => scalar

Unescape all backslash escaped chars.

=head2 eval

scalar => list

Run Perl's C<eval> command against the data and use the returned value
as the data.

=head2 eval_all

scalar => list

Run Perl's C<eval> command against the data and return a list of 4
values:

    1) The return value
    2) The error in $@
    3) Captured STDOUT
    4) Captured STDERR

=head2 eval_stderr

scalar => scalar

Run Perl's C<eval> command against the data and return the
captured STDERR.

=head2 eval_stdout

scalar => scalar

Run Perl's C<eval> command against the data and return the
captured STDOUT.

=head2 exec_perl_stdout

list => scalar

Input Perl code is written to a temp file and run. STDOUT is captured and
returned.

=head2 flatten

scalar => list

Takes a hash or array ref and flattens it to a list.

=head2 get_url

scalar => scalar

The text is chomped and considered to be a url. Then LWP::Simple::get is
used to fetch the contents of the url.

=head2 hash

list => scalar

Turn a list of key/value pairs into an anonymous hash reference.

=head2 head[=number]

list => list

Takes a list and returns a number of the elements from the front of it. The
default number is one.

=head2 join

list => scalar

Join a list of strings into a scalar.

=head2 Join

Join the list of strings inside a list of array refs and return the
strings in place of the array refs.

=head2 lines

scalar => list

Break the data into an anonymous array of lines. Each line (except
possibly the last one if the C<chomp> filter came first) will have a
newline at the end.

=head2 norm

scalar => scalar

Normalize the data. Change non-Unix line endings to Unix line endings.

=head2 prepend=string

list => list

Prepend a string onto each of a list of strings.

=head2 read_file

scalar => scalar

Read the file named by the current content and return the file's content.

=head2 regexp[=xism]

scalar => scalar

The C<regexp> filter will turn your data section into a regular
expression object. You can pass in extra flags after an equals sign.

If the text contains more than one line and no flags are specified, then
the 'xism' flags are assumed.

=head2 reverse

list => list

Reverse the elements of a list.

=head2 Reverse

list => list

Reverse the list of strings inside a list of array refs.

=head2 slice=x[,y]

list => list

Returns the element number x through element number y of a list.

=head2 sort

list => list

Sorts the elements of a list in character sort order.

=head2 Sort

list => list

Sort the list of strings inside a list of array refs.

=head2 split[=string|pattern]

scalar => list

Split a string in into a list. Takes a optional string or regexp as a
parameter. Defaults to /\s+/. Same as Perl C<split>.

=head2 Split[=string|pattern]

list => list

Split each of a list of strings and turn them into array refs.

=head2 strict

scalar => scalar

Prepend the string:

    use strict; 
    use warnings;

to the block's text.

=head2 tail[=number]

list => list

Return a number of elements from the end of a list. The default
number is one.

=head2 trim

list => list

Remove extra blank lines from the beginning and end of the data. This
allows you to visually separate your test data with blank lines.

=head2 unchomp

list => list

Add a newline to each string value in a list.

=head2 write_file[=filename]

scalar => scalar

Write the content of the section to the named file. Return the filename.

=head2 yaml

scalar => list

Apply the YAML::Load function to the data block and use the resultant
structure. Requires YAML.pm.

=head1 AUTHOR

Ingy döt Net <ingy@cpan.org>

=head1 COPYRIGHT

Copyright (c) 2006. Ingy döt Net. All rights reserved.
Copyright (c) 2005. Brian Ingerson. All rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See http://www.perl.com/perl/misc/Artistic.html

=cut
