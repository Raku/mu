package SVNShell;

use strict;
use warnings;

use base 'Exporter';
our $VERSION = 0.01;
our @EXPORT_OK = qw(set_svn_cmd_prefix svnversion svninfo svnup);

my $svn_cmd_prefix = '';

sub set_svn_cmd_prefix {
    my ( $dir ) = @_;
    $svn_cmd_prefix = $dir;
}

    
sub svnversion {
    my ( $dir ) = @_;

    my $svnin_cmd =  $svn_cmd_prefix . 'svnversion ' . $dir . ' 2>&1 |';
    unless ( open( SVNINFO, $svnin_cmd ) ) {
        return ( undef, $! );
    }
    my $svnin_log = '';
    { 
        local $/ = undef;
        $svnin_log = <SVNINFO>; 
    }
    close SVNINFO;

    my ( $actual_rev ) = $svnin_log =~ /^\s*(\d+)\s*$/msi;
    return ( $actual_rev, $svnin_log );
}


sub svninfo {
    my ( $dir ) = @_;

    my $svnin_cmd =  $svn_cmd_prefix . 'svn info ' . $dir . ' 2>&1 |';
    unless ( open( SVNINFO, $svnin_cmd ) ) {
        return ( undef, $! );
    }
    my $svnin_log = '';
    { 
        local $/ = undef;
        $svnin_log = <SVNINFO>; 
    }
    close SVNINFO;

    my ( $actual_rev ) = $svnin_log =~ /Revision:\s*(\d+)/msi;
    return ( $actual_rev, $svnin_log );
}


sub svnup {
    my ( $dir, $rev, $try_this_file ) = @_;

    my $head = 0;
    if ( uc($rev) eq 'HEAD' ) {
        $head = 1;
        $rev = uc( $rev );

    } elsif ( $rev !~ /^\d+$/ ) {
        return ( 0, "Bad revision number '$rev'.\n"  );    
    }
    
    # svn up for not existing revision fill server logs
    if ( !$head && $try_this_file ) {
        my $svnup_cmd = $svn_cmd_prefix . 'svn st -u "' . $dir .'\\' . $try_this_file . '" 2>&1 |';
        if ( open( SVNUP, $svnup_cmd ) ) {
            my $svnup_log = '';
            while ( my $line = <SVNUP> ) { $svnup_log .= $line; }
            close SVNUP;

            if ( my ($last_rev) = $svnup_log =~ /^\s*Status\ against\ revision\:\s*(\d+)/s ) {
                if ( $last_rev < $rev ) {
                    my $msg = "Last revision in repository is '$last_rev'.\n";
                    return ( 0, $msg );    
                }
            } else {
                return ( 0, "Unknown svn st output!\n". $svnup_log ."\n". $! );
            }
        } else {
            return ( 0, $! );
        }
    }

    my $svnup_cmd =
        $svn_cmd_prefix . 'svn up ' . $dir 
        . ( ( $head ) ? '' : ' --revision ' . $rev )
        . ' 2>&1 |'
    ;
    if ( open( SVNUP, $svnup_cmd ) ) {
        my $svnup_log = '';
        while ( my $line = <SVNUP> ) { $svnup_log .= $line; }
        close SVNUP;
    
        $svnup_log =~ s{\n}{\n    }gs;
        if ( $svnup_log =~ /(No such revision)|(non-existent revision) \d+/s ) {
            return ( 0, $svnup_log );    
        }

        if ( my ( $new_rev) = $svnup_log =~ /Updated to revision (\d+)/s ) {
            return ( 1, $svnup_log, $new_rev );    
        }
        return ( 0, $svnup_log );
    }
    return ( 0, $! );
}

1;
