use v6-alpha;

class File::Find-6.0.0;

use File::Spec;

has Int   $.debug;
has Int   $.follow;
has Array $.dirs;
has Array $.queue;
has Array $.results;
has Code  $.wanted_dir;
has Code  $.wanted_file;

submethod BUILD { $.debug = 0; $.follow = 0; }

method find ( $self: :@dirs, :$wanted_dir, :$wanted_file ) {
    my @start    = @dirs        || @.dirs;
    my $wdir_cb  = $wanted_dir  || $.wanted_dir  || sub { 1 };
    my $wfile_cb = $wanted_file || $.wanted_file || sub { 1 };
    for @start -> $dir {
        my $abs = $dir;
        $abs = rel2abs($abs);
        $.queue.push($abs);
    }
    my $dir;
    while $dir = $.queue.shift {
        say "$dir" if $.debug;
        my $dh = $dir.opendir;
        for $dh.readdir -> $node {
            next if $node ~~ m:P5/^(?:\.){1,2}$/;
            my $abs = catdir( $dir, $node );
            if $abs !~~ :d  {
                $abs = catfile( $dir, $node );
                if $wfile_cb( $node, $dir, $abs ) {
                    if ($abs ~~ :l && $.follow) || $abs ~~ :!l {
                        $.results.push($abs);
                        say "  $abs ~~ :!f " if $.debug;                        
                    }else{ say "    $abs ~~ :l " if $.debug }
                }
                else { say "  $abs ~~ :f " if $.debug }
            }
            else {
                if $wdir_cb( $node, $dir, $abs ) {
                    if ($abs ~~ :l && $.follow) || $abs ~~ :!l {
                        $.queue.push($abs);
                        say "  $abs ~~ :!d " if $.debug;
                    }else { say "   $abs ~~ :l " if $.debug }
                }
                else { say "  $abs ~~ :d " if $.debug }
            }
        }
	$dh.closedir;
	undefine $dh;
    }
    return $.results;
}

=head1 NAME

File::Find - Traverse a directory tree

=head1 SYNOPSIS

    use File::Find;

    my $f = File::Find.new;
    $f.wanted_file = sub ( $file, $path, $pathfile ) {
        return 1 if $file ~~ m:P5/^.*\.pm$/;
    }
    $f.dirs = qw/lib/;
    my @files = $f.find;

    # debug attribute for statistics
    $f.debug = 1;

    # attribute for following symlink
    $f.follow = 1;
    
=head1 DESCRIPTION

Perl 6 port of the C<File::Find> library.

This is no direct port, the api got cleaned up a bit. :)
Some features are still missing but will be added in the future...

=head1 CALLBACKS

We have two callbacks C<wanted_file> and C<wanted_dir>.

=over 4

=item wanted_file

Gets called before a file is added to results.
Should return 1 or 0. (defaults to 1)

    sub ( $file, $path, $pathfile ) {
        return 1;
    }

=item wanted_dir

Gets called before a directory is added to the queue.
Should return 1 or 0. (defaults to 1)

    sub ( $dir, $path, $pathdir ) {
        return 1;
    }

=back

=head1 AUTHOR

Sebastian Riedel <sri@oook.de>

=head1 LICENSE

This library is free software . You can redistribute it and/or modify
it under the same terms as perl itself.

=cut
