module File::Basename-2.73;
use v6;

=head1 NAME

fileparse - split a pathname into pieces

basename - extract just the filename from a path

dirname - extract just the directory from a path

=head1 SYNOPSIS

    use File::Basename;

    ($name, $path, $suffix) = fileparse $fullname, @suffixlist;
    $name     = fileparse $fullname, @suffixlist;
    fileparse_fstype = $os_string;
    $basename = basename $fullname, @suffixlist;
    $dirname  = dirname $fullname;

    ($name, $path, $suffix) = fileparse "lib/File/Basename.pm", rx{\.pm};
    fileparse_fstype = "VMS";
    $basename = basename "lib/File/Basename.pm",".pm";
    $dirname  = dirname "lib/File/Basename.pm";

=head1 DESCRIPTION

These routines allow you to parse file specifications into useful
pieces using the syntax of different operating systems.

=over 4

=item fileparse_fstype = "..."

You select the syntax via the routine fileparse_set_fstype().

If the argument passed to it contains one of the substrings
"VMS", "MSDOS", "MacOS", "AmigaOS" or "MSWin32", the file specification 
syntax of that operating system is used in future calls to 
fileparse(), basename(), and dirname().  If it contains none of
these substrings, Unix syntax is used.  This pattern matching is
case-insensitive.  If you've selected VMS syntax, and the file
specification you pass to one of these routines contains a "/",
they assume you are using Unix emulation and apply the Unix syntax
rules instead, for that function call only.

If the argument passed to it contains one of the substrings "VMS",
"MSDOS", "MacOS", "AmigaOS", "os2", "MSWin32" or "RISCOS", then the pattern
matching for suffix removal is performed without regard for case,
since those systems are not case-sensitive when opening existing files
(though some of them preserve case on file creation).

If you haven't called fileparse_set_fstype(), the syntax is chosen
by examining the builtin variable C<$^O> according to these rules.

=item fileparse

The fileparse() routine divides a file specification into three
parts: a leading B<path>, a file B<name>, and a B<suffix>.  The
B<path> contains everything up to and including the last directory
separator in the input file specification.  The remainder of the input
file specification is then divided into B<name> and B<suffix> based on
the optional patterns you specify in C<@suffixlist>.  Each element of
this list can be a qr-quoted pattern (or a string which is interpreted
as a regular expression), and is matched
against the end of B<name>.  If this succeeds, the matching portion of
B<name> is removed and prepended to B<suffix>.  By proper use of
C<@suffixlist>, you can remove file types or versions for examination.

You are guaranteed that if you concatenate B<path>, B<name>, and
B<suffix> together in that order, the result will denote the same
file as the input file specification.

In scalar context, fileparse() returns only the B<name> part of the filename.

=back

=head1 EXAMPLES

Using Unix file syntax:

    ($base,$path,$type) = fileparse '/virgil/aeneid/draft.book7',
				    rx{\.book\d+};

would yield

    $base eq 'draft'
    $path eq '/virgil/aeneid/',
    $type eq '.book7'

Similarly, using VMS syntax:

    ($name,$dir,$type) = fileparse 'Doc_Root:[Help]Rhetoric.Rnh',
				   rx{\..*};

would yield

    $name eq 'Rhetoric'
    $dir  eq 'Doc_Root:[Help]'
    $type eq '.Rnh'

=over

=item C<basename>

The basename() routine returns the first element of the list produced
by calling fileparse() with the same arguments, except that it always
quotes metacharacters in the given suffixes.  It is provided for
programmer compatibility with the Unix shell command basename(1).

=item C<dirname>

The dirname() routine returns the directory portion of the input file
specification.  When using VMS or MacOS syntax, this is identical to the
second element of the list produced by calling fileparse() with the same
input file specification.  (Under VMS, if there is no directory information
in the input file specification, then the current default device and
directory are returned.)  When using Unix or MSDOS syntax, the return
value conforms to the behavior of the Unix shell command dirname(1).  This
is usually the same as the behavior of fileparse(), but differs in some
cases.  For example, for the input file specification F<lib/>, fileparse()
considers the directory name to be F<lib/>, while dirname() considers the
directory name to be F<.>).

=back

=cut

our $fileparse_fstype;
our $fileparse_igncase;

#   fileparse_set_fstype() - specify OS-based rules used in future
#                            calls to routines in this package
#
#   Currently recognized values: VMS, MSDOS, MacOS, AmigaOS, os2, RISCOS
#       Any other name uses Unix-style rules and is case-sensitive
sub fileparse_fstype() is rw is export {
  return new Proxy:
    FETCH => { $fileparse_fstype },
    STORE => {
      $fileparse_fstype = $^os;
      $Fileparse_igncase = ($^os ~~ m:i/^[MacOS|VMS|AmigaOS|os2|RISCOS|MSWin32|MSDOS]/;
    };
}

#   fileparse() - parse file specification
#
#   Version 2.4  27-Sep-1996  Charles Bailey  bailey@genetics.upenn.edu
sub fileparse(Str $fullname, Rule *@suffices) is export {
  my ($fstype, $igncase) = ($fileparse_fstype, $fileparse_igncase);
  my ($dirpath, $tail, $suffix, $basename);

  if $fstype ~~ m:i/^VMS/ {
    if $fullname ~~ m#/#) { $fstype = '' }  # We're doing Unix emulation
    else {
      ($dirpath, $basename) = ($fullname ~~ m/^(.*<[:>\]]>)?(.*)/);
      $dirpath //= '';  # should always be defined
    }
  }

  if $fstype ~~ m:i/^MS[DOS|Win32]|epoc/ {
    ($dirpath, $basename) = ($fullname ~~ m/^([.*<[:\\\/]>]?)(.*)/);
    $dirpath ~= '.\\' unless $dirpath ~~ m/<[\\\/]>$/;
  } elsif $fstype ~~ m:i/^os2/ {
    ($dirpath, $basename) = ($fullname ~~ m#^([.*<[:\\/]>]?)(.*)#);
    $dirpath  = './' unless $dirpath;	# Can't be 0
    $dirpath ~= '/'  unless $dirpath ~~ m#<[\\/]>\z#;
  } elsif $fstype ~~ m:i/^MacOS/ {
    ($dirpath, $basename) = ($fullname ~~ /^(.*:)?(.*)/);
    $dirpath = ':' unless $dirpath;
  } elsif $fstype ~~ m:i/^AmigaOS/ {
    ($dirpath, $basename) = ($fullname ~~ m/(.*<[:\/]>)?(.*)/);
    $dirpath = './' unless $dirpath;
  } elsif not $fstype ~~ m:i/^VMS/ {  # default to Unix
    ($dirpath, $basename) = ($fullname ~~ m#^(.*/)?(.*)#);
    if ($*OSNAME eq 'VMS' and $fullname ~~ m:^(/<-[/]>+/000000(/|$))(.*): { #/#--vim
      # dev:[000000] is top of VMS tree, similar to Unix '/'
      # so strip it off and treat the rest as "normal"
      my $devspec   = $1;
      my $remainder = $3;
      ($dirpath, $basename) = ($remainder ~~ m#^(.*/)?(.*)#);
      $dirpath //= '';  # should always be defined
      $dirpath = $devspec ~ $dirpath;
    }
    $dirpath = './' unless $dirpath;
  }

  if @suffices {
    $tail = '';
    for @suffices -> $suffix {
      my $pat = ($igncase ?? '(?i)' : '') . "($suffix)\$";
      if $igncase
	?? $basename ~~ s:i/($sufixx)//
	:: $basename ~~ s/($suffix)//
      {
        $tail = $1 ~ $tail;
      }
    }
  }

  given want {
    when Scalar { $basename }
    default     { ($basename, $dirpath, $tail) }
  }
}


#   basename() - returns first element of list returned by fileparse()
sub basename(Str $name, Str *@suffices) is export {
  return (fileparse $name, map { rx{$_} } @_)[0];
}


#    dirname() - returns device and directory portion of file specification
#        Behavior matches that of Unix dirname(1) exactly for Unix and MSDOS
#        filespecs except for names ending with a separator, e.g., "/xx/yy/".
#        This differs from the second element of the list returned
#        by fileparse() in that the trailing '/' (Unix) or '\' (MSDOS) (and
#        the last directory name if the filespec ends in a '/' or '\'), is lost.
sub dirname(Str $dir) is export {
  my ($basename, $dirname) = fileparse $dir;
  my $fstype               = $fileparse_fstype;

  if $fstype ~~ m:i/VMS/ { 
    if $dir ~~ m#/#) { $fstype = '' }
    else { return $dirname or %*ENV{DEFAULT} }
    # -- WTF? Environment access? Why?
  }

  if $fstype ~~ m:i/MacOS/ {
    if !length $basename && not $dirname ~~ m/^<-[:]>+:$/ {
      $dirname ~~ s/(<-[:]>):$/$1/;
      ($basename, $dirname) = fileparse $dirname;
    }
    $dirname ~= ":" unless $dirname ~~ m/:$/;
  } elsif $fstype ~~ m:i/MS[DOS|Win32]|os2/ { 
    $dirname ~~ s/(<-[:]>)<[\\\/]>*$/$1/;
    unless length $basename {
      ($basename, $dirname) = fileparse $dirname;
      $dirname ~~ s/(<-[:]>)<[\\\/]>*/$1/;
    }
  } elsif $fstype ~~ m:i/AmigaOS/ {
    if $dirname ~~ m/:$/ { return $dirname }
    chop $dirname; # XXX - is chop still in Perl 6?
    $dirname ~~ s#<-[:/]>+$## unless length $basename;
  } else {
    $dirname ~~ s:(.)/*$:$1:;
    unless length $basename {
      temp $fileparse_fstype = $fstype;
      ($basename, $dirname) = fileparse $dirname;
      $dirname ~~ s:(.)/*$:$1:;
    }
  }

  return $dirname;
}

fileparse_fstype = $*OSNAME;

1;
