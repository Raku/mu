module Config::Tiny-0.01;
use v6;

# Config::Tiny has already been ported to Perl6 at
# http://tpe.freepan.org/repos/adamk/Config-Tiny/lib/Config/Tiny.pm
#
# Unfortunately, OO doesn't exist in Pugs yet so
# I decided to write OO before OO existed

sub new () returns Ref {
    my %self;
    my %methods = (
        'err_str' => { %self<_err_str> },
        'read' => sub ($file) {
            if ( ! $file ) {
                %self<_err_str> = 'You did not specify a file name' and return FALSE;
            }
            my $input = open($file);
            if ( ref $input ne 'IO' ) {
                %self<_err_str> = "Failed to open $file for reading" and return FALSE;
            }
            my $sect = '';
            my $cnt  = 0;
            for =$input -> $line is copy {
                ++$cnt;
                chomp $line;

                # Skip comments and empty lines
                # Workaround for next
                if ( $line ~~ rx:perl5/^\s*(?:\#|\;|$)/ ) { }
                else {
                    # Handle section headers
                    if ($line ~~ rx:perl5/^\s*\[(.+?)\]\s*$/) {
                        $sect = $1;
                    }
                    # Handle properties
                    elsif ($line ~~ rx:perl5/^\s*([^=]+?)\s*=\s*(.*?)\s*$/) {
                        my ($key, $val) = ($1, $2);
                        %self{$sect}{$key} = $val;
                    }
                    else {
                        %self<_err_str> = "Syntax error at line $cnt: $line" and return FALSE;
                    }
                }
            }
            $input.close;
            return TRUE;
        },
        'write' => sub ($file) {
            if ( ! $file ) {
                %self<_err_str> = 'You did not specify a file name' and return FALSE;
            }
            my $output = open('>' ~ $file);
            if ( ref $output ne 'IO' ) {
                %self<_err_str> = "Failed to open $file for writing" and return FALSE;
            }
            for grep {$_ ne '_err_str' } sort keys %self -> $section {
                $output.say("[$section]") if $section.chars;
                for sort keys %self{$section} {
                    $output.say("$_=%self{$section}{$_}");
                }
            }
        $output.close;
        return TRUE;
        },
        'data' => sub { \%self },
    );
    return \%methods;
}
sub TRUE  { 1 }
sub FALSE { 0 }

=pod

=head1 NAME

Config::Tiny - Read/Write .ini style files with as little code as possible

=head1 SYNOPSIS

    # In your configuration file
    rootproperty=blah

    [section]
    one=twp
    three= four
    Foo =Bar
    empty=

    # In your program
    use Config::Tiny;

    # Create a config
    my $cfg = new();

    # Open the config
    $cfg<read>( 'cfg.ini' ) || die $cfg<err_str>();

    # Reading properties 
    my $rootproperty = $cfg<data>(){}<rootproperty>; # Syntax below is nicer 

    my $data = $cfg<data>();
    my $one = $data<section><one>;
    my $Foo = $data<section><Foo>;

    # Changing data
    $cfg<data>()<newsection> = { 'this' => 'that' }; # Add a section - Syntax below is nicer 

    my $data = $cfg<data>();
    $data<section><Foo> = 'Not Bar!';                # Change a value
    delete $data{''};                                # Delete a value or section

    # Save a config
    $cfg<write>( 'new.ini' );

=head1 DESCRIPTION

Config::Tiny is a utility to read and write .ini style configuration files
with as little code as possible, reducing load time and memory overhead.
Written using Pugs as of 2005-05-05, it was written to provide functionality
while Perl6 was still being developed

This module is primarily for reading human written files, and anything we
write shouldn't need to have documentation/comments. If you need something
with more power, move up to Config::Simple, Config::General or one of the
many other Config:: modules. To rephrase, Config::Tiny does not preserve
your comments, whitespace, or the order of your config file.

=head1 CONFIGURATION FILE SYNTAX

Files are the same as windows .ini files, for example.

        [section]
        var1=value1
        var2=value2

If a property is outside of a section, it will be assigned to the root
section, available at C<$Config-E<gt>{''}>.

Lines starting with '#' or ';' are comments, and blank lines are ignored.

When writing back to the config file, any comments are discarded.

=head1 METHODS

Since the current version of Pugs does not support OO, methods are
current hash keys to code refs

=head2 new

The constructor C<new> creates and returns an empty Config::Tiny object.

=head2 read $filename

The C<read> method reads a config file, and returns a boolean value
of success or failure

=head2 data

The C<data> method returns a hash reference representing the configuration data

=head2 write

The C<write> method the file for the properties, and writes it
to disk.  Returns boolean value of success or failure;

=head2 err_str

The C<err_str> method returns the last error message

=head1 AUTHOR

Joshua Gatcomb, <Limbic_Region_2000@Yahoo.com>

This module is based on Adam Kennedy's Perl5 module by the same name

=head1 SEE ALSO

Adam's Perl5 implementation
L<http://search.cpan.org/~adamk/Config-Tiny-2.01/lib/Config/Tiny.pm>
Adam's Perl6 implementation
L<http://tpe.freepan.org/repos/adamk/Config-Tiny/lib/Config/Tiny.pm>

=head1 COPYRIGHT

 Copyright (c) 2005 Joshua Gatcomb. All rights reserved.
 This program is free software; you can redistribute it
 and/or modify it under the same terms as Perl itself.

=cut