# hlida zmeny nastaveni 'watchdog-setting.bin'
# hlida vystup programu presmerovany do souboru

use strict;
use File::stat;
use Time::HiRes qw(sleep);
use Storable;
use Data::Dump qw(dump);
use Win32::Process::Info; 

# default
my $timeout_to_kill = 3*60;
my $sleep_between_checks = 10;

sub kill_inside {
    my ( $pid ) = @_;
    print "trying to find and kill one child (parent pid: $pid)\n";

    my $pi = Win32::Process::Info->new(); 
    my %subs = $pi->Subprocesses(); # Figure out subprocess relationships.

    if (0) {
        open FP, ">>$0.pids";
        my @pids = $pi->ListPids (); # Get all known PIDs 
        my @info = $pi->GetProcInfo (); # Get the max 
        my %subs = $pi->Subprocesses(); # Figure out subprocess relationships.
        print FP dump( \@pids ) . "\n---\n\n";; 
        print FP dump( \@info ) . "\n---\n\n";; 
        print FP dump( \%subs ) . "\n---\n\n";
        print FP dump( $subs{$pid} );
        close FP;
        print "pid log saved\n";
    }

    
    my @childs = @{ $subs{$pid} };
    my $to_kill = $pid;
    
    # go deep
    while ( (exists $subs{$to_kill}) && (scalar @{ $subs{$to_kill} }) ) {
        $to_kill = $subs{$to_kill}->[-1];
    }
    
    print "found pid $to_kill\n";
    unless ( kill( 'INT', $to_kill ) ) {
        return kill( 9, $to_kill );
    }
    return 1;
}


my ( $st, $mtime, $diff );
my $ipc_fn = 'watchdog-setting.bin';

my $text = '';
my ( $o_text, $ch_time, $mch_time );
$ch_time = time();
$mch_time = $ch_time;
my $last_ff_time;
my $same_num = 0;
my $prev_pid = 0;
my $pid = 0;
my $prev_fn = '';
my $fn = '';
while (1) {
    while ( -f $ipc_fn ) {
        my $info = retrieve($ipc_fn) or die "$!\n$@";
#        print dump( $info ) . "\n";

        if ( exists $info->{'timeout'} ) {
            $timeout_to_kill = $info->{'timeout'};
            if ( exists $info->{'sleep'} ) {
                $sleep_between_checks = $info->{'sleep'};
            } else {
                $sleep_between_checks =  int($timeout_to_kill/5)+1;
            }
        }

        $prev_fn = $fn;
        $fn = $info->{'log_fn'};
        if ( $prev_fn ne $fn ) {
            print "new process log file (fn: $fn)\n";
            $mch_time = $ch_time;
            $ch_time = time();
            $same_num = 0;
        }

        $prev_pid = $pid;
        $pid = $info->{'pid'};
        if ( $prev_pid != $pid ) {
            print "new process (pid: $pid)\n";
            $mch_time = $ch_time;
            $ch_time = time();
            $same_num = 0;
        }
        
        open ( LOG, $fn ) or die "$fn $!$@";
        $o_text = $text;
        {
            local $/ = undef;
            $text = <LOG>;
        }
        close LOG;
        
        my $diff =  time() - $ch_time;
        my $n_diff = $diff - $timeout_to_kill;

        if ( $o_text ne $text ) {
            $mch_time = $ch_time;
            $ch_time = time();
            my $a_diff = $ch_time - $mch_time;
            print "changed after $a_diff s (reserve:$n_diff, timeout:$timeout_to_kill)\n";
            $same_num = 0;

        } else {
            $same_num++;
            if ( $diff > $timeout_to_kill ) {
                print "error, not changed for $diff s (reserve:$n_diff, timeout:$timeout_to_kill)\n";
                my $res = kill_inside( $pid );
                if ( $res ) {
                    print "pid: child pid was killed\n";
                    $same_num = 0;
                    $mch_time = $ch_time;
                    $ch_time = time();
                    print "child for pid $pid killed\n";
                    $same_num = 0;
                } else {
                    print "pid: $pid wasn't killed\n";
                }

            } elsif ( $diff > 0 ) {
                print "same for $diff s (reserve:$n_diff, timeout:$timeout_to_kill, same_num:$same_num)\n";
            }
        }
        sleep( $sleep_between_checks );
        $last_ff_time = time();
    }
    print "'$ipc_fn' not found " . (
        defined $last_ff_time ? 'for ' .( time()-$last_ff_time ) . ' s' : ''
    ) . "\n";
    sleep $sleep_between_checks;
}

