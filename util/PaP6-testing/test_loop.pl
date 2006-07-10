#!perl 

use strict;
use warnings;

use Carp qw(carp croak);
use FindBin qw($RealBin); 

use File::Spec::Functions;
use File::Path;
use File::Copy;

use lib 'libcpan';
use Data::Dump qw(dump);
use File::Copy::Recursive qw(dircopy);

use lib 'lib';
use Watchdog qw(sys sys_for_watchdog);
use SVNShell qw(svnversion svnup);

use lib "$FindBin::Bin/lib";

use Term::ReadKey;
ReadMode('cbreak');

my $ver = 5;

print "Working path: '" . $RealBin . "'\n" if $ver > 3;

print "Loading config file.\n" if $ver > 1;

my $fp_conf = catfile( $RealBin, 'conf.pl' );
print "Config file path: '" . $fp_conf . "'\n" if $ver > 1;
my $conf = require $fp_conf;

dump( $conf ) if $ver > 4;

print "Validating config.\n" if $ver > 1;
foreach my $c_num ( 0..$#$conf ) {
    my $ck = $conf->[$c_num];
    print "num: $c_num\n" if $ver > 3;

    # global keys
    foreach my $mck ( qw(name repository commands) ) {
        croak "Option '$mck' not found for '$ck'!\n" unless defined $ck->{$mck};
        print "$mck: '" . $ck->{$mck} . "'\n" if !ref($ck->{$mck}) && $ver > 3;
    }

    # commands keys
    foreach my $ac_num ( 0..$#{$ck->{commands}} ) {
        my $ack = $ck->{commands}->[$ac_num];
        print "  num: $ac_num\n" if $ver > 3;
        foreach my $mack ( qw(name cmd) ) {
            croak "Option '$mack' not found for '$ack'!\n" unless defined $ack->{$mack};
            print "  $mack: '" . $ack->{$mack} . "'\n" if !ref($ack->{$mack}) && $ver > 3;
        }
    }
    $ck->{src_dn} = $ck->{name} . '-src' unless exists $ck->{src_dn};
    $ck->{temp_dn} = $ck->{name} . '-temp' unless exists $ck->{temp_dn};
    $ck->{results_dn} = $ck->{name} . '-results' unless exists $ck->{results_dn};
    $ck->{src_add_dn} = $ck->{name} . '-src-add' unless exists $ck->{src_add_dn};
    
    # todo
    $ck->{temp_dn_back} = '..' unless exists $ck->{temp_dn_back};

    # todo
    $ck->{rm_temp_dir} = 1 unless exists $ck->{rm_temp_dir};
    $ck->{rewrite_temp_dir} = 1  unless exists $ck->{rewrite_temp_dir};
}
print "\n" if $ver > 3;

dump( $conf ) if $ver > 3;
print "\n" if $ver > 3;

# todo - load from dump

sub default_cmd_state {
    return {
        'cmd_num' => 0,
        'before_done' => 0,
        'cmd_done' => 0,
        'after_done' => 0,
    };
}

sub default_state {
    return {
        'svnup_done' => 0,
        'rm_temp_dir_done' => 0,
        'copy_src_dir_done' => 0,
        'after_temp_copied_done' => 0,
        'cmd' => default_cmd_state(),
    };
}


my $fp_state = catfile( $RealBin, 'state.pl' );
print "Runstate config file path: '" . $fp_state . "'\n" if $ver > 1;
my $state;
if ( -e $fp_state ) {
    $state = require $fp_state;
} else {
    $state = default_state();
}


my $conf_last = $#$conf;
my $conf_first = ( exists $state->{ck_num} ) ? $state->{ck_num} : 0;
my $first_time = 1;

while ( 1 ) {
    NEXT_CONF: foreach my $ck_num ( $conf_first..$conf_last ) {
        if ( $first_time ) {
            $conf_first = 0;
        } else {
            $state = default_state();
        }
        $first_time = 0;

        my $ck = $conf->[$ck_num];
        # todo
        #   $state->{ck_hash} = hash($ck)
        #   $conf_a->{ignore_ck_hash}
        $state->{ck_num} = $ck_num;

        print 
            ( $ck->{skip} ? 'Skipping' : 'Running' ) .
            " testing for configuration: '$ck->{name}' (" .
            ($ck_num+1) . " of " . ($conf_last+1). ")\n"
        if $ver > 1;
        next if $ck->{skip};

        # 'svn co' if needed
        unless ( -d $ck->{src_dn} ) {
            print "Source dir not found: 'svn co'\n";
            my $cmd = 'svn co "' . $ck->{repository} . '" "' . $ck->{src_dn} . '"';
            my ( $cmd_rc, $out ) = sys_for_watchdog( 
                $cmd, 
                $ck->{results_dn} . '/svn_co.txt' 
            );
            if ( $cmd_rc ) {
                print "svn co failed, return code: $cmd_rc\n" if $ver > 1;
                print "svn co output: '$out'\n" if $ver > 2;
                next;
            }
            $state->{svnup_done} = 1;
        } else {
            print "Source dir found: '$ck->{src_dn}'\n" if $ver > 3;
        }

        # get revision num
        unless ( $state->{svnup_done} ) {
            print "Getting revision number for src dir.\n" if $ver > 1;
            my ( $o_rev, $o_log ) = svnversion( $ck->{src_dn} );
            die "svn info failed: $o_log" unless defined $o_rev;
            $state->{src_rev} = $o_rev;
            print "Src revision number: $state->{src_rev}\n" if $ver > 1;
            if ( $state->{src_rev} !~ /^(\d+)$/ ) {
                print "Bad revision number. No clean src dir.\n" if $ver > 1;
                next NEXT_CONF;
            }
        }
        # svn up
        while ( not $state->{svnup_done} ) {    
            my $to_rev = 'HEAD';
            # $to_rev = $state->{src_rev} + 1;
            my $slt = 5*30;

            my ( $up_ok, $o_log, $new_rev ) = svnup( 
                $ck->{src_dn}, 
                $to_rev, 
                'README'
            );
            if ( $up_ok ) {
                if ( $new_rev > $state->{src_rev} ) {            
                    $state->{svnup_done} = 1;
                } else {
                    print "Newer revision not found in repository!\n" if $ver > 1;
                }
            } else {
                print "org dir svn up to $to_rev failed:\n" if $ver > 1;
                print "'$o_log'\n" if $ver > 2 && defined($o_log);
                print "waiting for $slt s ...\n";
            }

            if ( !$state->{svnup_done} ) {
                next NEXT_CONF if $conf_last > $conf_first; 
                sleep $slt;
                next;
            }

            print "Src dir svn up to $to_rev done\n" if $ver > 1;
            print "New src revision number: $new_rev \n" if $ver > 1;
            $state->{src_rev} = $new_rev;
        }

        if ( $ck->{rm_temp_dir} ) {
            if ( $state->{rm_temp_dir_done} ) {
                print "SKIP: Remove temp dir.\n" if $ver > 2;
            } else {
                print "Removing temp dir '$ck->{temp_dn}' ...\n" if $ver > 2;
                if ( -d $ck->{temp_dn} ) {
                    rmtree( $ck->{temp_dn} ) or die $!;
                    while ( -d $ck->{temp_dn} ) {
                        print "Remove temp dir failed.\n" if $ver > 1;
                        my $wait_time = 10;
                        print "Waiting for ${wait_time}s.\n" if $ver > 1;
                        sleep $wait_time;
                        rmtree( $ck->{temp_dn} ) or die $!;
                    }
                    print "Remove temp dir done.\n" if $ver > 1;
                } else {
                    print "Temp dir not found.\n" if $ver > 1;
                }
                $state->{rm_temp_dir_done} = 1;
            }
        }

        if ( $ck->{rm_temp_dir} || $ck->{rewrite_temp_dir} ) {
            if ( $state->{copy_src_dir_done} ) {
                print "SKIP: Copy src dir.\n" if $ver > 2;
            } else {
                print "Copying src '$ck->{src_dn}' to temp '$ck->{temp_dn}' ...\n" if $ver > 2;
                dircopy( $ck->{src_dn}, $ck->{temp_dn} ) or die "$!\n$@";
                print "Copy src dir to temp dir done.\n" if $ver > 1;
                $state->{copy_src_dir_done} = 1;
            }
        }

        if ( $ck->{after_temp_copied} ) {
            if ( $state->{after_temp_copied_done} ) {
                print "SKIP: after_temp_copied hook.\n" if $ver > 1; 
            } else {
                print "Running after_temp_copied hook.\n" if $ver > 2; 
                my $after_temp_copied_ret_code = $ck->{after_temp_copied}->( $ck, $state, $ver );
                print "After_temp_copied hook return: $after_temp_copied_ret_code\n" if $ver > 2;
                unless ( $after_temp_copied_ret_code ) {
                    print "Running after_temp_copied hook failed." if $ver > 1; 
                    next;
                }
            }
        }

        my $cmd_last = $#{ $ck->{commands} };
        my $cmd_first;
        if ( $state->{cmd} ) {
            $cmd_first = $state->{cmd}->{cmd_num};
            die "cmd_first > cmd_last" if $cmd_first > $cmd_last;

        } else {
            $cmd_first = 0;
        }

        chdir( $ck->{temp_dn} ) or die $!;

        my ( $o_rev, $o_log );
        # get revision num
        ( $o_rev, $o_log ) = svnversion( '.' );
        die "svn info failed: $o_log" unless defined $o_rev;
        $state->{temp_rev} = $o_rev;
        print "Revision number: $state->{temp_rev}\n" if $ver > 1;

        my $timestamp = time();
        $state->{results_path_prefix} = 
            $RealBin . '/' 
            . $ck->{results_dn} . '/'
            . 'r' . $state->{temp_rev} . '-' . $timestamp .  '/'
        ;
        unless ( mkpath( $state->{results_path_prefix} ) ) {
            print "Can't create results dir '$state->{results_path_prefix}'.\n" if $ver > 1;
            next;
        }
        print "Results dir: '$state->{results_path_prefix}'.\n" if $ver > 3;

        my $cmd_first_time = 1;
        foreach my $cmd_num ( $cmd_first..$cmd_last ) {
            if ( $cmd_first_time ) {
                $cmd_first_time = 0;
            } else {
                $state->{cmd} = default_cmd_state();
            }

            my $cmd_conf = $ck->{commands}->[$cmd_num];
            my $cmd = $cmd_conf->{cmd};
            my $cmd_name = $cmd_conf->{name};

            my $cmd_say = "'$cmd_name' (" . ($cmd_num+1) . " of " . ($cmd_last+1). ")";
            print "Command: $cmd_say\n" if $ver > 1;

            # max_time for watchdog
            my $cmd_mt = $cmd_conf->{mt};

            $state->{cmd}->{num} = $cmd_num;

            if ( $cmd_conf->{before} ) {
                if ( $state->{cmd}->{before_done} ) {
                    print "SKIP: before_cmd hook.\n" if $ver > 2;
                } else {
                    print "Running before_cmd hook.\n" if $ver > 2; 
                    my $before_ret_code = $cmd_conf->{before}->( $ck, $state, $ver );
                    print "Before cmd hook return: $before_ret_code\n" if $ver > 2;
                    unless ( $before_ret_code ) {
                        print "Running before_cmd hook failed.\n" if $ver > 1; 
                        next;
                    }
                }
            }
            $state->{cmd}->{before_done} = 1;

            if ( $state->{cmd}->{cmd_done} ) {
                print "SKIP: after_cmd hook.\n" if $ver > 2;
            } else {
                my $cmd_log_fp = 
                     $state->{results_path_prefix} 
                     . ($cmd_num+1) . '-' .  $cmd_name . '.out'
                ;
                # todo, .out to configure
                my ( $cmd_rc, $out ) = sys_for_watchdog( 
                    $cmd, 
                    $cmd_log_fp,
                    $cmd_mt
                );
                print "Command '$cmd_name' return $cmd_rc.\n" if $ver > 3;
            }
            $state->{cmd}->{cmd_done} = 1;

            if ( $cmd_conf->{after} ) {
                if ( $state->{cmd}->{after_done} ) {
                    print "SKIP: after_cmd hook.\n" if $ver > 2;
                } else {
                    print "Running after_cmd hook.\n" if $ver > 2; 
                    my $after_ret_code = $cmd_conf->{after}->( $ck, $state, $ver );
                    print "After_cmd hook return: $after_ret_code\n" if $ver > 3;
                    unless ( $after_ret_code ) {
                        print "Running after_cmd hook failed.\n" if $ver > 1; 
                    }
                }
            }
            $state->{cmd}->{after_done} = 1;
        }

        chdir( $ck->{temp_dn_back} ) or die $!;
    }

    my $char = undef;
    while ( defined ($char = ReadKey(-1)) ) { 1; }
    if ( $char ) {
        print "User press '$char'.\n" if $ver > 3; 
        $char = uc( $char );
        # TODO
        if ( $char eq 'P' ) {
            print "User press pause key.\n" if $ver > 2;

        } elsif ( $char eq 'C' ) {
            print "User press continue key.\n" if $ver > 2;

        } elsif ( $char eq 'Q' || $char eq 'E' ) {
            print "User press exit key.\n" if $ver > 2;
            exit;
        }
    }
    print "\n" if $ver > 1;
}
