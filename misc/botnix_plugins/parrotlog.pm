package modules::local::parrotlog;
use strict;
use warnings;
use LWP::UserAgent;
use XML::RAI;
use HTML::Entities;
use WWW::Shorten::Metamark;
use WWW::Shorten 'Metamark';

# Parse RSS generated from trac's "revision log" page.

my $url  = 'http://trac.parrot.org/parrot/log/?limit=100&mode=stop_on_copy&format=rss';
my $lastrev;
my $copy_of_self;

sub init {
    my $self = shift;
    $copy_of_self = $self;
    my $rev = main::get_item($self, "parrot_lastrev");
    undef $rev unless length $rev;
    $lastrev = $rev if defined $rev;
    main::lprint("parrotlog: init: initialized lastrev to $lastrev") if defined $lastrev;
    main::create_timer("parrotlog_fetch_feed_timer", $self, "fetch_feed", 30);
}

sub implements {
    return qw();
}

sub shutdown {
    my $self = shift;
    main::delete_timer("parrotlog_fetch_feed_timer");
    main::store_item($self, "parrot_lastrev", $lastrev) if defined $lastrev;
}

my $lwp = LWP::UserAgent->new();
$lwp->timeout(30);
$lwp->env_proxy();

sub fetch_feed {
    my $response = $lwp->get($url);
    if($response->is_success) {
        my $feed = XML::RAI->parse_string($response->content);
        process_feed($feed);
    } else {
        main::lprint("parrotlog: fetch_feed: failure fetching $url");
    }
}

sub process_feed {
    my $rss = shift;
    my @items = @{$rss->items};
    @items = sort { $a->link cmp $b->link } @items; # ascending order
    my $newest = $items[-1];
    my ($newestrev) = $newest->link   =~ m|/changeset/(\d+)/|;
    ($newestrev)    = $items[0]->link =~ m|/changeset/(\d+)/| if exists $ENV{TEST_RSS_PARSER};

    # skip the first run, to prevent new installs from flooding the channel
    if(defined($lastrev)) {
        # output new entries to channel
        foreach my $item (@items) {
            my ($rev) = $item->link =~ m|/changeset/(\d+)/|;
            output_item($item) if $rev > $lastrev;
        }
    }
    $lastrev = $newestrev;
    main::store_item($copy_of_self, "parrot_lastrev", $lastrev);
}

sub longest_common_prefix {
    my $prefix = shift;
    for (@_) {
        chop $prefix while (! /^\Q$prefix\E/);
    }
    return $prefix;
}


sub output_item {
    my $item = shift;
    my $prefix  = 'unknown';
    my $creator = $item->creator;
    my $link    = $item->link;
    my $desc    = $item->description;

    my ($rev)   = $link =~ m|/changeset/(\d+)/|;
    my $response = $lwp->get($link);
    if(!$response->is_success) {
        return main::lprint("parrotlog: could not fetch $link");
    }

    my $changeset_text = $response->content;
    $changeset_text =~ s/<[^>]+>//g;
    decode_entities($changeset_text);
    $changeset_text =~ s/\s+/ /gs;
    ($prefix) = $changeset_text =~ /Location: (\S+) /;
    ($prefix) = $changeset_text =~ /Files: 1 \S+ (\S+) / unless defined $prefix;
    $prefix = 'unknown' unless defined $prefix;

    $desc =~ s/<[^>]+>//g;
    decode_entities($desc);
    $desc =~ s|^\s+||g;
    $desc =~ s|\s+$||g;
    my @log_lines = split(/[\r\n]+/, $desc);

    put("parrot: r$rev | $creator++ | $prefix:");
    foreach my $line (@log_lines) {
	put("parrot: $line");
    }
    put("parrot: review: $link");
    main::lprint("parrotlog: output_item: output rev $rev");
}

sub put {
    my $line = shift;
    main::send_privmsg("magnet", "#parrot", $line);
}

1;
