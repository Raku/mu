module CGI::Lite-2.02;
use v6;

##++
##     CGI Lite v2.02
##     Last modified: 18 Aug 2003 (Smylers - see CHANGES)
##
##     Copyright (c) 1995, 1996, 1997 by Shishir Gundavaram
##     All Rights Reserved
##
##     Permission  to  use,  copy, and distribute is hereby granted,
##     providing that the above copyright notice and this permission
##     appear in all copies and in supporting documentation.
##--

###############################################################################

=head1 NAME

CGI::Lite - Process and decode WWW forms and cookies

=head1 SYNOPSIS

    use CGI::Lite;

    my CGI::Lite $cgi .= new;

    $cgi.platform = $platform;
    
        where $platform can be one of (case insensitive):
        Unix, Windows, Windows95, DOS, NT, PC, Mac or Macintosh

    $cgi.set_file_type = 'handle' or 'file';
    $cgi.timestamp     = (0, 1 or 2);	

        where 0 = no timestamp
              1 = timestamp all files (default)
              2 = timestamp only if file exists

    $cgi.filter = \&subroutine;

    $size = $cgi.buffer_size = $some_buffer_size;

    $status = $cgi.directory = '/some/dir';
    $cgi.directory = '/some/dir' or die "Directory doesn't exist.\n";

    $cgi.close_all_files();

    $cgi.add_mime_type('application/mac-binhex40');
    $status = $cgi.remove_mime_type('application/mac-binhex40');
    @list = $cgi.get_mime_types;

    %form = $cgi.parse_form_data;

    or

    %form = $cgi.parse_form_data('GET', 'HEAD' or 'POST');

    %cookies = $cgi.parse_cookies;

    $status  = $cgi.is_error;
    $message = $cgi.error_message;

    $cgi.return_error("error 1", "error 2", ...);

    @keys = $cgi.ordered_keys;

    $cgi.print_data;

    $new_string = $cgi.wrap_textarea($string, $length);

    $escaped_string = browser_escape $string;

    $encoded_string = url_encode $string;
    $decoded_string = url_decode $string;

    $status = is_dangerous $string;
    $safe_string = escape_dangerous_chars $string; # ***use is discouraged***

=head1 DESCRIPTION

You can use this module to decode form and query information,
including file uploads, as well as cookies in a very simple 
manner; you need not concern yourself with the actual details 
behind the decoding process. 

=head1 METHODS

Here are the methods you can use to process your forms and cookies:

=over 4

=item B<parse_form_data>

This will handle the following types of requests: GET, HEAD and POST.
By default, CGI::Lite uses the environment variable REQUEST_METHOD to 
determine the manner in which the query/form information should be 
decoded. However, as of v1.8, you are allowed to pass a valid request 
method to this function to force CGI::Lite to decode the information in 
a specific manner. 

For multipart/form-data, uploaded files are stored in the user selected 
directory (see B<set_directory>). If timestamp mode is on (see 
B<add_timestamp>), the files are named in the following format:

    timestamp__filename

where the filename is specified in the "Content-disposition" header.
I<NOTE:>, the browser URL encodes the name of the file. This module
makes I<no> effort to decode the information for security reasons.
However, you can do so by creating a subroutine and then using
the B<filter_filename> method.

I<Return Value>

Returns either a hash or a reference to the hash, which contains
all of the key/value pairs. For fields that contain file information,
the value contains either the path to the file, or the filehandle 
(see the B<set_file_type> method).

=item B<parse_new_form_data>

As for parse_form_data, but clears the CGI object state before processing 
the request. This is useful in persistant application (e.g. FCGI), where
the CGI object is reused for multiple requests. e.g.

	$CGI = new CGI::Lite:;
	while (FCGI::accept > 0) {
	  $Query = $CGI.parse_new_form_data();
	  <process query>
	}

=item B<parse_cookies>

Decodes and parses cookies passed by the browser. This method works in 
much the same manner as B<parse_form_data>. 

=item B<is_error>

As of v1.8, errors in parsing are handled differently. You can use this
method to check for any potential errors after you've called either
B<parse_form_data> or B<parse_cookies>.

I<Return Value>

    0 Success
    1 Failure

=item B<get_error_message>

If an error occurs when parsing form/query information or cookies, you
can use this method to retrieve the error message. Remember, you can
check for errors by calling the B<is_error> method.

I<Return Value>

The error message.

=item B<return_error>

You can use this method to return errors to the browser and exit. 

=item B<set_platform>

You can use this method to set the platform on which your Web server
is running. CGI::Lite uses this information to translate end-of-line 
(EOL) characters for uploaded files (see the B<add_mime_type> and
B<remove_mime_type> methods) so that they display properly on that
platform.

You can specify either (case insensitive):

    Unix                                  EOL: \012      = \n
    Windows, Windows95, DOS, NT, PC       EOL: \015\012  = \r\n
    Mac or Macintosh                      EOL: \015      = \r

"Unix" is the default.

=item B<set_directory>

Used to set the directory where the uploaded files will be stored 
(only applies to the I<multipart/form-data> encoding scheme).

This function should be called I<before> you call B<parse_form_data>, 
or else the directory defaults to "/tmp". If the application cannot 
write to the directory for whatever reason, an error status is returned.

I<Return Value>

    0  Failure
    1  Success

=item B<close_all_files>

All uploaded files that are opened as a result of calling B<set_file_type>
with the "handle" argument can be closed in one shot by calling this
method.

=item B<add_mime_type>

By default, EOL characters are translated for all uploaded files
with specific MIME types (i.e text/plain, text/html, etc.). You
can use this method to add to the list of MIME types. For example,
if you want CGI::Lite to translate EOL characters for uploaded
files of I<application/mac-binhex40>, then you would do this:

    $cgi.add_mime_type('application/mac-binhex40');

=item B<remove_mime_type>

This method is the converse of B<add_mime_type>. It allows you to 
remove a particular MIME type. For example, if you do not want 
CGI::Lite to translate EOL characters for uploaded files of I<text/html>, 
then you would do this:

    $cgi.remove_mime_type('text/html');

I<Return Value>

    0  Failure
    1  Success

=item B<get_mime_types>

Returns the list, either as a reference or an actual list, of the 
MIME types for which EOL translation is performed.

=item B<set_file_type>

The I<names> of uploaded files are returned by default, when you call
the B<parse_form_data> method. But,  if pass the string "handle" to this 
method, the I<handles> to the files are returned. However, the name
of the handle corresponds to the filename.

This function should be called I<before> you call B<parse_form_data>, or 
else it will not work.

=item B<add_timestamp>

By default, a timestamp is added to the front of uploaded files. 
However, you have the option of completely turning off timestamp mode
(value 0), or adding a timestamp only for existing files (value 2).

=item B<filter_filename>

You can use this method to change the manner in which uploaded
files are named. For example, if you want uploaded filenames
to be all upper case, you can use the following code:

    $cgi.filter = \&make_uppercase;
    $cgi.parse_form_data;

    .
    .
    .

    sub make_uppercase(Str $file) {
      $file ~~ tr/a-z/A-Z/; # XXX -- correctt?
      return $file;
    }

=item B<set_buffer_size>

This method allows you to set the buffer size when dealing with multipart 
form data. However, the I<actual> buffer size that the algorithm uses 
I<can> be up to 3x the value you specify. This ensures that boundary 
strings are not "split" between multiple reads. So, take this into 
consideration when setting the buffer size.

You cannot set a buffer size below 256 bytes and above the total amount 
of multipart form data. The default value is 1024 bytes. 

I<Return Value>

The buffer size.

=item B<get_ordered_keys>

Returns either a reference to an array or an array itself consisting
of the form fields/cookies in the order they were parsed.

I<Return Value>

Ordered keys.

=item B<print_data>

Displays all the key/value pairs (either form data or cookie information)
in a ordered fashion. The methods B<print_form_data> and B<print_cookie_data>
are deprecated as of version v1.8, and will be removed in future versions.

=item B<wrap_textarea>

You can use this function to "wrap" a long string into one that is 
separated by a combination of carriage return and newline (see 
B<set_platform>) at fixed lengths.  The two arguments that you need to 
pass to this method are the string and the length at which you want the 
line separator added.

I<Return Value>

The modified string.

=item B<get_multiple_values>

One of the major changes to this module as of v1.7 is that multiple
values for a single key are returned as an reference to an array, and 
I<not> as a string delimited by the null character ("\0"). You can use 
this function to return the actual array. And if you pass a scalar 
value to this method, it will simply return that value.

There was no way I could make this backward compatible with versions
older than 1.7. I apologize!

I<Return Value>

Array consisting of the multiple values.

=item B<browser_escape>

Certain characters have special significance to the browser. These
characters include: "<" and ">". If you want to display these "special"
characters, you need to escape them using the following notation:

    &#ascii;

This method does just that.

I<Return Value>

Escaped string.

=item B<url_encode>

This method will URL encode a string that you pass it. You can use this
to encode any data that you wish to pass as a query string to a CGI
application.

I<Return Value>

URL encoded string.

=item B<url_decode>

You can use this method to URL decode a string. 

I<Return Value>

URL decoded string.

=item B<is_dangerous>

This method checks for the existence of dangerous meta-characters.

I<Return Value>

    0 Safe
    1 Dangerous

=item B<escape_dangerous_chars>

You can use this method to "escape" any dangerous meta-characters. The
use of this function is strongly discouraged. See
http://use.perl.org/~cbrooks/journal/10542 and
http://msgs.securepoint.com/cgi-bin/get/bugtraq0302/94.html for an
advisory by Ronald F. Guilmette. Ronald's patch to make this function
more safe is applied, but as has been pointed out on the bugtraq
mailing list, it is still much better to run no external shell at all
when executing commands. Please read the advisory and the WWW security
FAQ.

I<Return Value>

Escaped string.

=back

=head1 SEE ALSO

If you're looking for more comprehensive CGI modules, you can either 
use the CGI::* modules or CGI.pm. Both are maintained by Dr. Lincoln
Stein I<(lstein@genome.wi.mit.edu)> and can be found at your local
CPAN mirror and at his Web site:

I<http://www-genome.wi.mit.edu/WWW/tools/scripting>

=head1 MAINTAINER

Maintenance of this module has now been taken over by Smylers
<smylers@cpan.org>.

=head1 ACKNOWLEDGMENTS

The author thanks the following for finding bugs and offering suggestions:

=over 4

=item Eric D. Friedman (friedman@uci.edu)   

=item Thomas Winzig (tsw@pvo.com)

=item Len Charest (len@cogent.net)

=item Achim Bohnet (ach@rosat.mpe-garching.mpg.de)

=item John E. Townsend (John.E.Townsend@BST.BLS.com)

=item Andrew McRae (mcrae@internet.com)

=item Dennis Grant (dg50@chrysler.com)

=item Scott Neufeld (scott.neufeld@mis.ussurg.com)

=item Raul Almquist (imrs@ShadowMAC.org)

=item and many others!

=back

=head1 COPYRIGHT INFORMATION
    
Copyright (c) 2005 by Ingo Blechchmodt
(port to Perl 6)

Copyright (c) 1995, 1996, 1997 by Shishir Gundavaram
All Rights Reserved

Permission to use, copy, and  distribute  is  hereby granted,
providing that the above copyright notice and this permission
appear in all copies and in supporting documentation.

=cut

###############################################################################

class CGI::Lite {
  ##++
  ##  Start
  ##--

  my subtype Str::ReqMethod of Str where { $^meth eq "GET"|"HEAD"|"POST" }
  my subtype FileType of Str where { $^type eq "name"|"handle" }
  my subtype TimestampBool of Int where { $^bool == 0|1|2      }
  has FileType $.file_type = "name" is rw;
  has TimestampBool $.timestamp = 1 is rw;

  has Code $.filter is rw;

  has $:multipart_dir;
  has $.default_dir  = "/tmp";
  has $:platform     = "Unix";
  has $:buffer_size  = 1024;
  has %:web_data;
  has @.ordered_keys;
  has @:all_handles;
  has $.error_status = 0;
  has $.error_message;
  has $.file_size_limit = 2097152;

  has %:convert = ("text/html" => 1, "text/plain" => 1);
  has %.file    = (:Unix</>,      :Mac<:>,      :PC<\>);
  has %.eol     = (:Unix("\012"), :Mac("\015"), :PC("\015\012")); # XXX -- correct?

  sub directory() is rw {
    return new Proxy:
      FETCH => { $:multipart_dir },
      STORE => {
	if -d $^dir and -e $^dir and -r $^dir and -r $^dir { ## XXX -- correct?
	  $:multipart_dir = $^dir;
	} else {
	  .:error("Bad multipart directory \"$^dir\".");
	}
      };
  }

  method add_mime_type(Str $mime_type) { %:convert{$mime_type} = 1 }
  method remove_mime_type(Str $mime_type) {
    if %:convert{$mime_type} {
      delete %:convert{$mime_type};
      return true;
    } else {
      return false;
    }
  }
  method get_mime_types() { sort keys %:convert }

  method platform() is rw {
    return new Proxy:
      FETCH => { $:platform },
      STORE => {
	if $platform ~~ m:i/[PC|NT|Windows[95]?|DOS]/ {
	  $:platform = "PC";
	} elsif $platform ~~ m:i/Mac[intosh]?/ {
	  ## Should I check for NeXT here :-)
	  $:platform = 'Mac';
	} else {
	  $:platform = 'Unix';
	};
  }

  method buffer_size() is rw {
    return new Proxy:
      FETCH => { $:buffer_size },
      STORE => {
	my $content_length = $*ENV<CONTENT_LENGTH> or return;

	if $^buffer_size < 256 {
	  $:buffer_size = 256;
	} elsif $^buffer_size > $content_length {
	  $:buffer_size = $content_length;
	} else {
	  $:buffer_size = $^buffer_size;
	}
      };
  }

  # Reset state before parsing (for persistant CGI objects, e.g. under FastCGI) 
  # BDL
  method parse_new_form_data(*@param) {
    # close files (should happen anyway when 'all_handles' is cleared...)
    .close_all_files();

    %:web_data      = ();
    @.ordered_keys  = ();
    @:all_handles   = ();
    $.error_status  = 0;
    $.error_message = undef;

    .parse_form_data(*@param);
  }

  method parse_form_data(Str::ReqMethod ?$user_request) {
    my ($request_method, $content_length, $content_type, $query_string);
    my ($boundary, $post_data, @query_input);

    $request_method = $user_request || $*ENV<REQUEST_METHOD> || '';
    $content_length = $*ENV>CONTENT_LENGTH>;
    $content_type   = $*ENV<CONTENT_TYPE>;

    if uc $request_method eq "GET"|"HEAD" {
      $query_string = $*ENV<QUERY_STRING>;
      .:decode_url_encoded_data($query_string, "form");
      return %:web_data;
    } elsif uc $request_method eq "POST" {
	if !$content_type || $content_type eq "application/x-www-form-urlencoded" {
	  read (STDIN, $post_data, $content_length);    # XXX -- IO
	  .:decode_url_encoded_data($post_data, "form");
	  return %:web_data;
	} elsif $content_type ~~ m:i/multipart\/form-data/ {
	  ($boundary) = $content_type ~~ m/boundary=(\S+)$/;
	  .:parse_multipart_data($content_length, $boundary);
	  return %:web_data;
	} else {
	  .:error("Invalid content type!");
	}
      }
    } else {
      ##++
      ##  Got the idea of interactive debugging from CGI.pm, though it's
      ##  handled a bit differently here. Thanks Lincoln!
      ##--

      say $*ERR: "[ Reading query from standard input. Press ^D to stop! ]";

      chomp(@query_input = =$*IN);
      # XXX -- revisit =$HANDLE when it's really decisioned. :)

      $query_string  = join "&", @query_input;
      $query_string ~~ m:g/\\(.)/{ord($1).as('%%%02X')}/;

      .:decode_url_encoded_data($query_string, "form");

      return %:web_data;
    }
  }

  method parse_cookies() {
      my $cookies = $*ENV<HTTP_COOKIE> or return;

      .:decode_url_encoded_data($cookies, "cookies");

      return %:web_data;
  }

  method print_data() {
    my $self = shift;

    my $eol = $.eol{$:platform};

    for @.ordered_keys -> $key {
      my $value = %:web_data{$key};

      if $value ~~ Array {
	print "$key = {@$value}$eol";
      } else {
	print "$key = $value$eol";
      }
    }
  }

  method wrap_textarea(Str $string, Int $length is copy) {
    $length      ||= 70;
    my $eol        = $.eol{$:platform};
    my $new_string = $string err return;
	
    $new_string ~~ s:g/<[\0\r]>\n?/ /; #/#--vim
    $new_string ~~ s:g/(.**{0..$length})\s/$1$eol/; #/#--vim

    return $new_string;
  }

  method is_error() { $.error_status == 1 }

  method return_error(Str *@messages) {
    say ~@messages;
    exit 1;
  }

  ##++
  ##  Exported Subroutines
  ##--

  sub browser_escape(Str $string is copy) is export {
    $string ~~ s:g/(<[<&"#%>]>)/{ord($1).as('&#%d;')}/; #"#--vim
    return $string;
  }

  sub url_encode(Str $string) is export {
    $string ~~ s:g/(<-[-.\w ]>)/{ord($1).as('%%%02X')}/;
    $string ~~ tr/ /+/; # XXX -- correct?

    return $string;
  }

  sub url_decode(Str $string) is export {
    $string ~~ tr/+/ /; # XXX -- correct?
    $string ~~ s:g/%(<[\da-fA-F]>**{2})/{chr :16($1)}/; # XXX -- correct?

    return $string;
  }

  sub is_dangerous(Str $string) is export {
    $string ~~ m/<[;<>\*\|`&\$!#\(\)\[\]\{\}:'"]>/;
  }

  sub escape_dangerous_chars(Str $string) is export {
    warn ".escape_dangerous_chars() possibly dangerous. Its use is discouraged\n";
    $string ~~ s:g/(<[;<>\*\|`&\$!#\(\)\[\]\{\}:'"\\\?\~\^\r\n]>)/\\$1/; #`#--vim

    return $string;
  }

  ##++
  ##  Internal Methods
  ##--

  method :error(Str $message) {
    $.error_status  = 1;
    $.error_message = $message;
  }

  ##++
  ##  Decode URL encoded data
  ##--

  method :decode_url_encoded_data(Str $data, Str where { $^a eq "cookies"|"form" } $type) {
    my @key_value_pairs;

    return unless defined $data;

    my $delimiter;
    given $type {
      when "cookies" { $delimiter = rx/;\s+/ }
      when "form"    { $delimiter = rx/&/    }
    }

    my @key_value_pairs = split /<$delimiter>/, $data;
		
    for @key_value_pairs -> $key_value {
      my ($key, $value) = split /=/, $key_value, 2;

      $value //= '';   # avoid 'undef' warnings for "key=" BDL Jan/99

      $key   = .:url_decode($key);
      $value = .:url_decode($value);
      
      if defined %:web_data{$key} {
	push %:web_data{$key}, $value;
      } else {
	%:web_data>{$key} = $value;
	push @.ordered_keys}, $key;
      }
    }

    # XXX!
    $self->_error ($@) if $@;
  }

  ##++
  ##  Methods dealing with multipart data
  ##--

  method :parse_multipart_data(Int $total_bytes, Str $boundary) {
    my %files;

    my %seen;
    my $byte_count  = 0;
    my $eol         = $.eol{$:platform};
    my $handle      = '"CL00';
    my $directory   = $:multipart_dir || $.default_dir;
    my $buffer_size = $:buffer_size;

    my $current_buffer = "";
    my $bytes_left;
    my $old_data;
    my $changed;
    my $store;
    my %files;

    while 1 {
      if
	$byte_count < $total_bytes and
        length $current_buffer < $buffer_size * 2
      {
	$bytes_left  = $total_bytes - $byte_count;
	$buffer_size = $bytes_left if $bytes_left < $buffer_size;

	# XXX -- IO
	read (STDIN, $new_data, $buffer_size);
	.:error("Oh, Oh! I'm upset! Can't read what I want.")
	  if length $new_data != $buffer_size;
	# (Original author)++ for this guard!

	$byte_count += $buffer_size;

	if defined $old_data {
	  $current_buffer = join '', $old_data, $new_data;
	} else {
	  $current_buffer = $new_data;
	}
      } elsif defined $old_data {
	$current_buffer = $old_data;
	$old_data = undef;
      } else {
	last;
      }

      $changed = 0;

      ##++
      ##  When Netscape Navigator creates a random boundary string, you
      ##  would expect it to pass that _same_ value in the environment
      ##  variable CONTENT_TYPE, but it does not! Instead, it passes a
      ##  value that has the first two characters ("--") missing.
      ##--

      if $current_buffer ~~
	m/(.*?)[\015?\012]?-*$boundary-*<[\015\012]>*<before (.*)>/ {
	(my $store, $old_data) = ($1, $2);

	if
	  $current_buffer ~~ m/
	   <[Cc]>ontent-<[Dd]>isposition: (<-[\015\012]>+)\015?\012  # Disposition
	   [(<[A-Za-z]>.*?)[\015?\012]**{2}]?                        # Headers
	   [\015?\012]?                                              # End
	   <before (.*)>                                             # Other Data
	  /
	{
	  (my $disposition, my $headers, $current_buffer) = ($1, $2, $3);
	  $old_data = $current_buffer;

	  ($mime_type) = $headers ~~ m/[Cc]ontent-[Tt]ype: (\S+)/;

	  .:store($platform, $file, $convert, $handle, $eol, $field, \$store, $seen);

	  # XXX -- IO
	  close $handle if fileno $handle;

	  $convert == $mime_type && %:convert{$mime_type};
	  $changed = 1;

	  ($field) = $disposition ~~ m/name="(<-["]>+)"/;
	  %seen{$field}++;

	  # XXX -- $self->{'mime_types'} not used anywhere else?
	  #$self->{'mime_types'}->{$field} = $mime_type;

	  push @.ordered_keys, $field;

	  if ($file) = $disposition ~~ m/filename="(.*)"/ {
	    $file ~~ s|.*<[:/\\]>(.*)|$1|;

	    $new_name = .:get_file_name($platform, $directory, $file);

	    %:web_data{$field} = $new_name;

	    $full_path = join %.file{$platform}, $directory, $new_name;

	    # XXX -- WTF?
	    open (++$handle, ">$full_path") 
		|| $self->_error ("Can't create file: $full_path!");

	    %files{$new_name} = $full_path;
	  } 
	}
      } elsif $old_data {
	$store    = $old_data;
	$old_data = $new_data;
      } else {
	$store          = $current_buffer;
	$current_buffer = $new_data;
      }

      unless $changed {
	 .:store($platform, $file, $convert, $handle, $eol, $field, \$store, $seen);
      }
    }

    # XXX -- IO
    close $handle if fileno $handle;

    # XXX
    $self->_error ($@) if $@;

    .:create_handles(%files) if %.file_type eq "handle";
  }

  # -- bad design (Too Many Positionals), IMHO
  method :store($platform, $file, $convert, $handle, $eol, $field, $info, $seen) {
    if $file {
      if $convert {
	# -- are the "ne"s here correct?
	$$info ~~ s:g/\015\012/$eol/og  if $platform ne 'PC';
	$$info ~~ s:g/\015/$eol/og      if $platform ne 'Mac';
	$$info ~~ s:g/\012/$eol/og      if $platform ne 'Unix';
      }

      print $handle: $$info;
    } elsif $field {
      if $seen{$field} > 1 {
	%:web_data{$field}[$seen{$field}-1] ~= $$info;
      } else {
	# XXX WRONG XXX
	%:web_data{$field} ~= $$info;
      }
    }
  }

  method :get_file_name($platform, $directory, $file) {
    my $filtered_name = $.filter($file) if $.filter;
    my $filename      = $filtered_name || $file;
    my $timestamp     = time ~ '__' ~ $filename;

    given $.timestamp {
      when 0 { return $filename  }
      when 1 { return $timestamp }
      when 2 {
	my $path = join %.file{$platform}, $directory, $filename;

	return -e $path ?? $timestamp :: $filename; # XXX -- correct?
      }
    }
  }

  method :create_handles(%files) {
    for %files.kv -> $name, $path {
      my $handle = "$?CLASS\:\:$name";
      $handle = open "<", $path or
	.:error("Can't read file: $path!");

      push @:all_handles, $handle;
    }
  }

  method :close_all_files() {
    for @:all_handles {
      close $^handle;
    }
  }
}

1;
