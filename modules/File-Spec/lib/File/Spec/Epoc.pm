#!pugs
use v6;

require File::Spec::Unix-0.0.1;

class File::Spec::Epoc-0.0.1 is File::Spec::Unix;

# make this public (read-only by default) 
# attributes, and they will auto-generate 
# their own accessors for us to use
has Bool $.case_tolerant = 1;

method canonpath (Str $path) returns Str {
    $path ~~ s|/+|/|g;                             # xx////xx  -> xx/xx
    $path ~~ s|(/\.)+/|/|g;                        # xx/././xx -> xx/xx
    $path ~~ s|^(\./)+||s unless $path eq "./";    # ./xx      -> xx
    $path ~~ s|^/(\.\./)+|/|s;                     # /../../xx -> xx
    $path ~~  s|/\Z(?!\n)|| unless $path eq "/";          # xx/       -> xx
    return $path;
}

1;

__END__

=head1 NAME

File::Spec::Epoc - methods for Epoc file specs

=head1 SYNOPSIS

 require File::Spec::Epoc; # Done internally by File::Spec if needed

=head1 DESCRIPTION

See File::Spec::Unix for a documentation of the methods provided
there. This package overrides the implementation of these methods, not
the semantics.

This package is still work in progress ;-)

=head1 AUTHORS

o.flebbe@gmx.de

=over 4

=item canonpath()

No physical check on the filesystem, but a logical cleanup of a
path. On UNIX eliminated successive slashes and successive "/.".

=back

=head1 SEE ALSO

See L<File::Spec> and L<File::Spec::Unix>.  This package overrides the
implementation of these methods, not the semantics.

=cut
