module Config::Tiny-2.01;
use v6;

class Config::Tiny {
  has (Str|Hash of Str) %.cfg is rw;

  submethod BUILD (?%.cfg) {}

  # Create an object from a file
  method read(::?CLASS $class, Str $file) {
    return read_string $class: slurp($file) ||
      die "Failed to slurp file \"$file\": $!\n";
  }

  method read_string(::?CLASS $class, Str $config) {
    # Parse the file
    my $ns      = '_';
    my $counter = 0;
    my %cfg;
    for split /[\015**{1..2}\012|\015|\012]/, $config {
      $counter++;

      # Skip comments and empty lines
      next if /^\s*[\#|\;|$]/;

      # Handle section headers
      if /^\s*\[(.+?)\]\s*$/ {
	# Create the sub-hash if it doesn't exist.
	# Without this sections without keys will not
	# appear at all in the completed struct.
	%cfg{$ns = $1} ||= {};
	next;
      }

      # Handle properties
      if /^\s*(<-[=]>+?)\s*=\s*(.*?)\s*$/ {
	%cfg{$ns}{$1} = $2;
	next;
      }

      die "Syntax error at line $counter: \"$'\"";
    }

    return $class.new(cfg => \%cfg);
  }

  # Save an object to a file
  method write(Str $file) {
    my $fh = open ">", $file or
      die "Couldn't open \"$file\" for writing: $!\n";

    print $fh: .write_string;
  }

  # Save an object to a string
  method write_string() {
    my $contents = '';

    for
      sort { (($^b eq "_") <=> ($^a eq "_")) || ($^a cmp $^b) }
      keys %.cfg
    -> $cfg {
      my %block = %.cfg{$section};
      $contents ~= "\n"           if length $contents;
      $contents ~= "[$section]\n" unless $section eq "_";
      for sort keys %block -> $property {
	$contents ~= "$property=%block{$property}\n";
      }
    }
    
    return $contents;
  }

  multi sub *coerce:<as> (::?CLASS $self, Str ::to) { .write_string }
}

1;

__END__

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
    my Config::Tiny $Config .= new();
    
    # Open the config
    $Config = Config::Tiny.read("file.conf");
    
    # Reading properties
    my $rootproperty = $Config.cfg<_><rootproperty>;
    my $one = $Config.cfg<section><one>;
    my $Foo = $Config.cfg<section><Foo>;

    # Changing data
    $Config.cfg<newsection>   = { this => 'that' }; # Add a section
    $Config.cfg<section><Foo> = "Not Bar!";         # Change a value
    delete $Config.cfg<_>;                          # Delete a value or section

    # Save a config
    $Config.write("file.conf");

    # Config::Tiny objects stringify to their text representation:
    say "The config is\n" ~ ~$Config;

=head1 DESCRIPTION

Config::Tiny is a perl class to read and write .ini style configuration files
with as little code as possible, reducing load time and memory overhead.
Memory usage is normally scoffed at in Perl, but in my opinion should be
at least kept in mind.

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
section, available at C<$Config-E<gt>{_}>.

Lines starting with '#' or ';' are comments, and blank lines are ignored.

When writing back to the config file, any comments are discarded.

=head1 METHODS

=head2 new

The constructor C<new> creates and returns an empty Config::Tiny object.

=head2 read($filename)

The C<read> constructor reads a config file, and returns a new Config::Tiny
object containing the properties in the file. 

Returns the object on success, or C<undef> on error.

=head2 read_string($string)

The C<read_string> method takes as argument the contents of a config file as a string
and returns the Config::Tiny object for it.

=head2 write($filename)

The C<write $filename> generates the file for the properties, and writes it
to disk. 

Returns true on success or C<undef> on error.

=head2 write_string

Generates the file for the object and returns it as a string.

=head1 SUPPORT

Bugs should be reported via the CPAN bug tracker at

L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Config%3A%3ATiny>

For other issues, or commercial enhancement or support, contact the author.

=head1 TO DO

I'm debating adding a get and set method to get or set a section.key based
value...

Implementation is left as an exercise for the reader.

=head1 AUTHOR

Ingo Blechschmidt (port to Perl 6)

Adam Kennedy (Maintainer), L<http://ali.as/>, cpan@ali.as

Thanks to Sherzod Ruzmetov <sherzodr@cpan.org> for Config::Simple,
which inspired this module by being not quite "simple" enough for me :)

=head1 SEE ALSO

L<Config::Simple>, L<Config::General>

=head1 COPYRIGHT

Copyright 2005 Ingo Blechschmidt (port to Perl 6).

Copyright 2002 - 2005 Adam Kennedy. All rights reserved.
This program is free software; you can redistribute
it and/or modify it under the same terms as Perl itself.

The full text of the license can be found in the
LICENSE file included with this module.

=cut
