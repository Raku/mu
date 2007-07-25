package modules::p6bot::svnbot;

use strict;
use warnings;

use URI::Escape;
use POSIX qw();
use Time::Local qw(timegm_nocheck);
use Time::Zone qw();
use SVN::Core;
use SVN::Client;
use SVN::Ra;
use YAML ();
use Template;
use File::Spec;

my %repos = ();
my %repos_open = ();

my $timedate_format = '%Y/%m/%d %H:%M:%S';

sub init {
	my ($self) = @_;
}

sub implements {
    my @functions = ("before_configure","on_configure","on_privmsg","on_single_mode","on_notice");
    return @functions;
}

sub shutdown {
	my ($self,$nid,$server,$nick,$ident,$host,$channel)= @_;
	# TODO: delete_svn_timers();
}

sub on_privmsg {
    my ($self,$nid,$server,$nick,$ident,$host,$target,$text) = @_;
}

sub before_configure {
    my ($self,$confname) = @_;
	%repos = ();
	%repos_open = ();
}

sub check_repo {
    
}

sub init_svn_repo {
    my ( $self,$nid,$channel,$t,$name,$url,$interval ) = @_;
    $channel = lc( $channel );
    $interval > 15 or $interval = 15;
    # unique identifier for this repository, at runtime.
    # unnecessary currently # $repos_open{$name}{'repo_id'} = "$nid,$channel,$t";
    # get the latest published revision to this channel
    $repos_open{$name}{'last_pub_rev'} = main::get_item($self,"$nid,$channel,$name,last_sent_rev");
    # initialize the SVN remote access object
    $repos_open{$name}{'ra'} = SVN::Ra->new ( $url );
    # store the latest known HEAD revision
    main::get_item($self,"$nid,$channel,$name,last_sent_rev",($repos_open{$name}{'last_rev'} = $ra->get_latest_revnum());
    $repos_open{$name}{'url'} = $url;
    $repos_open{$name}{'interval'} = $interval;
    $repos{"$nid,$channel,$t"}{'init'} = 1;
}

sub on_configure {
    my ($self,$net_context,$chan_context,$configfile,$linenumber,$configline) = @_;
	# svnrepo1 name pugs
    # svnrepo1 url http://svn.pugscode.org/pugs/
        # url can be file://local/repo/path/
    # svnrepo1 template /path/to/simple/template
        # path to a very simple template for commit notify messages. 
        # variables are: <$username $message $revision $rootdir>
        # template lines including $message are repeated, each with a line from $message
        # Each repo must have exactly one of the following: interval socket port
        # which defines how the bot learns about commits
    # svnrepo1 interval 30
        # interval is the time between 'svn log' calls to the remote DAV host or FSFS repo
    # TODO: svnrepo1 socket /socket/on/which/to/listen/for/commit/notifies
        # e.g. from a post-commit script.
    # TODO: svnrepo1 port 44832 # probably something above 1024, since we're [hopefully] not root
        # e.g. from a post-commit script.

    # svnrepo2 name parrot
    # svnrepo2 url http://svn.parrotcode.org/parrot/
    # svnrepo2 interval 300

    my $t = 0;
    
    if ($configline =~ /^\s+svnrepo(\d+)\s+(interval|port)\s+(\d+?)$/i) {
        if ($chan_context eq "") {
            main::lprint("svnrepo command outside of channel context on $configfile:$linenumber");
            return 0;
        }
        $t = $1;
        $repos{"$net_context,lc($chan_context),$1"}{$2} = $3;
    }
    elsif ($configline =~ /^\s+svnrepo(\d+)\s+(name|url|template|socket)\s+(.+?)$/i) {
        if ($chan_context eq "") {
            main::lprint("svnrepo command outside of channel context on $configfile:$linenumber");
            return 0;
        }
        $t = $1;
        $repos{"$net_context,lc($chan_context),$1"}{$2} = $3;
	}
	# Initialize the svn_repo if we've received a complete config.
	init_svn_repo( $self,$net_context,$chan_context,
	    $t,
	    $repos{"$net_context,lc($chan_context),$t"}{'name'},
	    $repos{"$net_context,lc($chan_context),$t"}{'url'},
	    $repos{"$net_context,lc($chan_context),$t"}{'template'},
	    (
	        $repos{"$net_context,lc($chan_context),$t"}{'interval'} || 
	        $repos{"$net_context,lc($chan_context),$t"}{'socket'} ||
	        $repos{"$net_context,lc($chan_context),$t"}{'port'}
	    )
	) if (
	    !$repos{"$nid,lc($channel),$t"}{'init'} &&
	    $t && 
	    $repos{"$net_context,lc($chan_context),$t"}{'name'} &&
	    $repos{"$net_context,lc($chan_context),$t"}{'url'} &&
	    $repos{"$net_context,lc($chan_context),$t"}{'template'} &&
	    (
	        $repos{"$net_context,lc($chan_context),$t"}{'interval'} || 
	        $repos{"$net_context,lc($chan_context),$t"}{'socket'} ||
	        $repos{"$net_context,lc($chan_context),$t"}{'port'}
	    )
	);
    return 1;
}


1;
