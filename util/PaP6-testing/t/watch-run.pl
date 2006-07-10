use strict;
use Storable;
use Data::Dump qw(dump);

sub sys {
    my ( $cmd, $temp_out_fn ) = @_;

    my $output;
    $temp_out_fn = '.out' unless $temp_out_fn;
    open my $oldout, ">&STDOUT"     or die "Can't dup STDOUT: $!";
    open OLDERR,     ">&", \*STDERR or die "Can't dup STDERR: $!";

    open STDOUT, '>', $temp_out_fn or die "Can't redirect STDOUT: $!";
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

    my $ipc_fn = 'watchdog-setting.bin';

    if ( -e $ipc_fn ) {
        print "found '$ipc_fn', probably already running\n";
        return 0;
    }
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

    sys( $cmd, $log_fn );

    unlink $ipc_fn;
    print "finished\n";
    return 1;
}


my $cmd = 'perl watch-test-infinite.pl';
my $log_fn = $cmd . '.log';

print "running '$cmd' with sys_for_watchdog ...\n";
my $res = sys_for_watchdog( $cmd, $0 . '-1.log', 2 );
print "sys_for_watchdog return $res\n";
print "\n";

print "once more ...\n";
print "running '$cmd' with sys_for_watchdog ...\n";
my $res = sys_for_watchdog( $cmd, $0 . '-2.log', 4 );
print "sys_for_watchdog return $res\n";
print "\n";

print "finished\n";
