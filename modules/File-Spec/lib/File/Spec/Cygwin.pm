#!pugs
use v6;

require File::Spec::Unix-0.0.1;

class File::Spec::Cygwin-0.0.1 is File::Spec::Unix;

method canonpath (Str $path) returns Str {
    $path ~~ s|\\|/|g;
    return .SUPER::canonpath($path);
}

method file_name_is_absolute (Str $file) returns Bool {
    return 1 if $file ~~ m{^([a-z]:)?[\\/]}is; # C:/test
    return .SUPER::file_name_is_absolute($file);
}

my $tmpdir;
method tmpdir () returns Str {
    return $tmpdir if defined $tmpdir;
    $tmpdir = ._tmpdir( %*ENV{TMPDIR}, "/tmp", 'C:/temp' );
}

1;

__END__

=head1 NAME

File::Spec::Cygwin - methods for Cygwin file specs

=head1 SYNOPSIS

 require File::Spec::Cygwin; # Done internally by File::Spec if needed

=head1 DESCRIPTION

See L<File::Spec> and L<File::Spec::Unix>.  This package overrides the
implementation of these methods, not the semantics.

This module is still in beta.  Cygwin-knowledgeable folks are invited
to offer patches and suggestions.

=head1 METHODS

=over 4

=item canonpath

Any C<\> (backslashes) are converted to C</> (forward slashes),
and then File::Spec::Unix canonpath() is called on the result.


=item file_name_is_absolute

True is returned if the file name begins with C<drive_letter:>,
and if not, File::Spec::Unix file_name_is_absolute() is called.

=item tmpdir (override)

Returns a string representation of the first existing directory
from the following list:

    $ENV{TMPDIR}
    /tmp
    C:/temp

Since Perl 5.8.0, if running under taint mode, and if the environment
variables are tainted, they are not used.


=back

=cut
