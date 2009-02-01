package modules::local::stwikilog;
use strict;
use warnings;
use LWP::UserAgent;
use XML::RAI;
use HTML::Entities;
#XML::RAI::Item->add_mapping('branch', qw(dc:branch));

my $url     = 'http://www.perlfoundation.org/feed/workspace/parrot?category=Recent%20Changes';
my $lastpost;

sub init {
    my $self = shift;
    my $rev = main::get_item($self, "lastpost");
    undef $rev unless length $rev;
    $lastpost = $rev if defined $rev;
    main::create_timer("stwikilog_fetch_rss_timer", $self, "fetch_rss", 31);
}

sub implements {
    return qw();
}

sub numify_ts {
    my ($ts) = shift;
    $ts =~ s/[-T:\+]//g;
    return $ts;
}

sub shutdown {
    my $self = shift;
    main::delete_timer("stwikilog_fetch_rss_timer");
    main::store_item($self, "lastpost", $lastpost) if defined $lastpost;
}

my $lwp = LWP::UserAgent->new();
$lwp->timeout(10);
$lwp->env_proxy();

sub fetch_rss {
    my $response = $lwp->get($url);
    if($response->is_success) {
        my $rss = XML::RAI->parse_string($response->content);
        process_rss($rss);
    } else {
        main::lprint("stwikilog: fetch_rss: failure fetching $url");
    }
}

sub process_rss {
    my $rss = shift;
    my @items = @{$rss->items};
    my $newest = $items[0];
    my $newestpost = numify_ts($newest->created);
    #main::lprint("stwikilog: newepost is $newestpost");
    my @newposts;
    
    # skip the first run, to prevent new installs from flooding the channel
    if(defined($lastpost)) {
        # output new entries to channel
        foreach my $item (@items) {
            my ($post) = numify_ts($item->created);
            #main::lprint("stwikilog:     post is $post");
	    #main::lprint("stwikilog: lastpost is $lastpost\n");
	    last if $post <= $lastpost;
	    unshift(@newposts,$item);
        }
        output_item($_) foreach (@newposts);
    }
    $lastpost = $newestpost;
}

sub output_item {
    my $item = shift;
    my $creator = $item->creator;
    my $link    = $item->link;
    my $title   = $item->title;
    put("$creator | $title:");
    put("link: $link");
}

sub put {
    my $line = shift;
    main::send_privmsg("magnet", "#parrot", $line);
}

1;
