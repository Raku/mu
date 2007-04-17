class File::Util-6.0.0;

use v6-alpha;

has Str $.os;
has Str $.content;
has Str $.sl;
has Str $.nl;
has Int $.lines;
has Int $.exist;
has Int $.valid;
has Int $.canread;
has Int $.canwrite;
has Int $.isbin;

has $.fsize;

has Hash $.traversed;

has Array @.content_array;
has Array @.filetype;

has $.readlimit is rw; # set readlimit to a default of 50 megabytes
has $.maxdives  is rw; # maximum depth for recursive list_dir calls

rule dirsplit {\\|\:|\/};
rule illegal_chr {\?|<gt>|<lt>|\||\\|\/|\:|\*|\"|\t};
#"

submethod BUILD {
           
    given $*OS {
        when 'darwin' { $.os = 'UNIX' }
        when 'cygwin' { $.os = 'CYGWIN'}
        when 'MSWin' { $.os = 'WINDOWS'}
        when 'vms' { $.os = 'VMS'}
        when 'bsdos' { $.os = 'UNIX'}
        when 'dos' { $.os = 'DOS'}
        when 'MacOS' {$.os = 'MACINTOSH'}
        when 'epoc' { $.os = 'EPOC'}
        when 'os2' { $.os = 'OS2'}
        default {$.os = 'UNIX'}
    }

    $.readlimit  = 52428800; # set readlimit to a default of 50 megabytes
    $.maxdives   = 1000;     # maximum depth for recursive list_dir calls
    
    # $.ebcdic = qq[\t] ne qq[\011] ? 1 : 0;
    $.sl = { 'DOS' => '\\', 'EPOC'   => '/', 'MACINTOSH' => ':',
      'OS2' => '\\', 'UNIX'   => '/', 'WINDOWS'   => '\\',
      'VMS' => '/',  'CYGWIN' => '/', }{ $.os }||'/';
    
    # $.binmode = $.os =~ /WINDOWS|DOS|OS2|MSWin/ ? 1 : 0;
    # $.nl = $.binmode ? qq[\015\012]
    #         : $.ebcdic || $.os eq 'VMS' ? qq[\n]
    #            : $.os eq 'MACINTOSH' ? qq[\015]
    #               : qq[\012];     
}

multi method load_file (Str $filename, Int $as_lines?, Int $no_lock?) {
    
    my @dirs = $filename.split($.sl);
    my ($file, $path);
    if (@dirs.elems > 0) {
        $file = @dirs.pop;
        $path = @dirs.join($.sl);
    }

    if $path.chars > 0 {
        $path = '.' ~ $.sl ~ $path unless $path ~~ /(?:^\/)|(?:^\w\:)/;
    }else{
        $path = '.';
    }
    
    # check the existence of the file
    self.existent($filename);
    return self.throw('no such file', filename => $filename)
        if $.exist == undef;

    # check if we can read the parent directory
    self.can_read($path);
    return self.throw('can\'t dread', filename => $path, dirname => $path)
        if $.canread == undef;
    
    # check if we can read the file
    self.can_read($filename);
    return self.throw('can\'t fread', filename => $filename) 
        if $.canread == undef;
        
    # check if this is not a directory
    return self.throw('called open on a dir', filename => $filename) 
        if $filename ~~ :d;
    
    # check if the size is not exceeding the readlimit
    self.size($filename);
    return self.throw('readlimit exceeded', filename => $filename, size => $.fsize)
        if $.fsize > $.readlimit;
    
    # get the content of the file
    # XXX flock with no-lock
    $.content = slurp $filename;

    # XXX is there a way to transform $.content into an array ?
    @.content_array = $.content.split("\n") 
        if $as_lines == 1;
}

multi method load_file (IO $filehandle, Int $as_lines) {

    # XXX readlimit
    $.content = slurp $filehandle;

    # content became an array if as-list ?
    @.content_array = $.content.split("\n")
        if $as_lines == 1;
}

method write_file (Str $filename, Str $content, $options?) {

    # if the call to this method didn't include a filename to which the caller
    # wants us to write, then complain about it
    return self.throw('no input', meth => 'write_file')
        if $filename.chars == 0;

    # if prospective filename contains 2+ dir separators in sequence then
    # this is a syntax error we need to whine about
    return self.throw('bad chars')
        if $filename ~~ /(?:$.sl){2,}/;
}

method strip_path (Str $filename) {}

method escape_filename (Str $filename is rw, Str $escape? = "_", Int $strip_path?) {

    return undef if $filename.chars == 0;
    my $file = $filename;
    $file ~~ s/<illegal_chr>/$escape/;
    $file ~~ s/<dirsplit>/$escape/;
    return $file;
}

method make_dir (Str $dirname, $bitmask?, $if_not_exists) {
    
    my @dirs_in_path = $dirname.split(/\//);
    for @dirs_in_path -> $dir {
        self.valid_filename($dir);
        return if $.valid == undef;
    }
    
    for @dirs_in_path -> $dir {
        
    }
}

method load_dir (Str $dirname) {
    my @files = .list_dir($dirname);
    for @files -> $file {
        # self.load_file("t/" ~ $file);        
    }
}

method list_dir (Str $dirname, %options?){
    
    # my $maxd = $.maxdives;
    my $maxd = 12;
    my $path = $dirname;
    
    # if %options.exists('--recursing'){
    # }
    # for @options -> $opt {
    # }
    
    # check if we have a valid dirname
    return self.throw('no input', meth => 'list_dir')
        if $dirname.chars == 0;
    
    self.existent($filename);
    return self.throw('no such file', filename => $dirname)
        if $.exist == undef;
        
    return self.throw('called opendir on a file')
        unless $dirname ~~ :d;

    # if (@options.grep:{/--recursing/}) {
    #     my $pdir = $dirname;
    #     $pdir ~~ s/(^.*)$!sl.*/$1/;
    #     %.traversed{$pdir} = $pdir;
    # }

    # if ((list %.traversed).elems >= $maxd) {
    #     return .throw('maxdives exceeded');
    # }
    
    my $dh = opendir $dirname;
    my @dirs = $dh.readdir;

    my @shadow = @dirs;
    @dirs = ();
    while @shadow {
        my $f = @shadow.shift;
        next if $f eq '.';
        next if $f eq '..';
        next if $f ~~ /^\./;

        my $pathname = $path ~ "/" ~ $f;
        if $pathname ~~ :d {
            @dirs.push($path ~ "/" ~ $f);
        }
    }
    
    for @dirs -> $dir {
        self.list_dir($dir);
    }
    # for @options -> $option {
    # }
    return @dirs.sort;
}

method size (Str $filename) {
    return $.fsize = undef unless $filename ~~ :e;
    $.fsize = $filename ~~ :s;
}

method existent (Str $filename) {
    $filename ~~ :e ?? $.exist = 1 !! $.exist = undef;
}

method valid_filename (Str $filename){
    if $filename ~~ /<illegal_chr>/ {
        $.valid = undef;
    } else {
        $.valid = 1;
    }
}

method line_count (Str $filename){
    my $fh = open $filename, :r;
    my @row = =$fh;
    close $fh;
    $.lines = @row.elems;
}

# XXX timestamp
method created (Str $filename) {}
# XXX bitmask
method bitmask (Str $filename) {}

method can_read (Str $filename) {
    $filename ~~ :r ?? $.canread = 1 !! $.canread = undef;
}

method can_write (Str $filename) {
    $filename ~~ :w ?? $.canwrite = 1 !! $.canwrite = undef;
}

method is_bin (Str $filename) {
    $filename ~~ :B ?? $.isbin = 1 !! $.isbin = undef;
}

method file_type (Str $filename) {
    return undef unless $filename ~~ :e;
    
    @.filetype.push('PLAIN')     if $filename ~~ :f;
    @.filetype.push('TEXT')      if $filename ~~ :T;
    @.filetype.push('BINARY')    if $filename ~~ :B;
    @.filetype.push('DIRECTORY') if $filename ~~ :d;
    @.filetype.push('SYMLINK')   if $filename ~~ :l;
    @.filetype.push('SOCKET')    if $filename ~~ :S;
    @.filetype.push('PIPE')      if $filename ~~ :p;
    @.filetype.push('BLOCK')     if $filename ~~ :b;
    @.filetype.push('SOCKET')    if $filename ~~ :S;
    @.filetype.push('CHARACTER') if $filename ~~ :c;
    @.filetype.push('TTY')       if $filename ~~ :t;
}

# XXX last_access
method last_access (Str $filename) {}
# XXX last_modified
method last_modified (Str $filename) {}

method throw (Str $call, $meth?, $filename?, $opts?, $missing?) {
    say $call ~ $filename;
}

=head1 NAME

File::Util - Easy, versatile, portable file handling

=head1 SYNOPSIS

    use File::Util;
    
    my $f = File::Util.new;
    $f.load_file('t/01.use.t');
    $f.content.say;

=head1 DESCRIPTION

File::Util provides a comprehensive toolbox of utilities to automate all
kinds of common tasks on file / directories.  Its purpose is to do so
in the most portable manner possible so that users of this module won't
have to worry about whether their programs will work on other OSes
and machines.

=over4

=item load_file (filename, qr//)
=item load_file (file_handle, qr//)

If [file name] is passed, returns the contents of [file name] in a string.
If a [file handle reference] is passed instead, the filehandle will be 
C<CORE::slurp()> and the data obtained by the slurp will be returned in a string.

=head1 AUTHOR

Tommy Butler <cpan@atrixnet.com>
porting to Perl6 by Franck Cuny <franck.cuny@gmail.com>

=head1 LICENSE

This library is free software . You can redistribute it and/or modify
it under the same terms as perl itself.

=cut