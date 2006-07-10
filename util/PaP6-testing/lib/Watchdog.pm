package Watchdog;

use strict;
use warnings;

use base 'Exporter';
our $VERSION = 0.01;
our @EXPORT_OK = qw(sys sys_for_watchdog);

use Storable;

sub sys {
    my ( $cmd, $temp_out_fn ) = @_;

    my $output;
    $temp_out_fn = '.out' unless $temp_out_fn;
    open my $oldout, ">&STDOUT"     or die "Can't dup STDOUT: $!";
    open OLDERR,     ">&", \*STDERR or die "Can't dup STDERR: $!";

    open STDOUT, '>', $temp_out_fn or die "Can't redirect STDOUT to '$temp_out_fn': $! $@";
    open STDERR, ">&STDOUT"     or die "Can't dup STDOUT: $!";

    select STDERR; $| = 1;      # make unbuffered
    select STDOUT; $| = 1;      # make unbuffered

    my $status = system($cmd);

    close STDOUT;
    close STDERR;

    open STDOUT, ">&", $oldout or die "Can't dup \$oldout: $!";
    open STDERR, ">&OLDERR"    or die "Can't dup OLDERR: $!";

    unless ( open( FH_STDOUT, "<$temp_out_fn") ) {
        carp("File $temp_out_fn not open!");
        unlink $temp_out_fn;
        next;
    }
    {
        local $/ = undef;
        $output = <FH_STDOUT>;
    }
    close FH_STDOUT;
    return ( $status, $output );
}


sub sys_for_watchdog {
    my ( $cmd, $log_fn, $timeout, $sleep ) = @_; 

    die "cmd is mandatory" unless defined $cmd;
    $log_fn = $cmd . '.log' unless defined $log_fn;
    $timeout = 5*60 unless defined $timeout;

    my $ipc_fn = '../watchdog-setting.bin';

    if ( -e $ipc_fn ) {
        print "found '$ipc_fn', probably already running\n";
#        exit 0;
    }
    print "running '$cmd' ...\n";

    my $info = {
        'log_fn'  => $log_fn,
        'pid'     => $$,
        'cmd'     => $cmd,
        'timeout' => $timeout,
    };
    $info->{'sleep'} = $sleep if defined $sleep;
    store( $info, $ipc_fn ) or die "store failed\n$!\n$@";

=head todo
=pod
    sub catch_sig {
        my $signame = shift;
        print "captured $signame\n";
        unlink $ipc_fn;
        exit;
    }

    foreach my $sig ( qw/QUIT KILL CHILD/ ) {
        $SIG{$sig} = \&catch_sig;
    }
    #print dump( \%SIG );
=cut

    my ( $status, $output ) = sys( $cmd, $log_fn );

    unlink $ipc_fn;
    while ( -e $ipc_fn ) {
        print "trying to unlink $ipc_fn\n";
        sleep 1;
        unlink $ipc_fn;
    }
    print ( ( $status ) ? "finished, but exit code is $status\n" : "finished ok\n" );
    return ( $status, $output );
}

1;
