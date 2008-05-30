#!/usr/bin/perl
use strict;
use warnings;

use SVN::Log::Index;
use Data::Dumper;
use CGI;
use HTML::Template;
use Encode qw(encode decode);
$CGI::POST_MAX = 1024;
$CGI::DISABLE_UPLOADS = 1;
my $cgi = new CGI;
my $results_per_page = 20;

print "Content-Type: text/html; charset=utf-8\n\n";

my $q = $cgi->param('q');
my $t = HTML::Template->new(
		filename 		=> 'search.tmpl',
		global_vars		=> 1,
		);

if (length $q){
	$q = encode("utf8", $q);
    $results_per_page = 50 if $cgi->param('terse');
	search($q, $t);
	$t->param(Q => $q);
}

print decode("utf-8", $t->output);

sub search {
	my ($qs, $t) = @_;

	my $index_file = 'svn_index';
	my $idx = SVN::Log::Index->new({ index_path => $index_file });
	$idx->open();

	my $result = $idx->search($qs);
	my $total_hits = $result->total_hits;
	if ($total_hits == 0){
		$t->param(NO_RESULTS => 1);
		return;
	}

	my $offset = $cgi->param('offset') || 0;
	if ($offset !~ m/\A\d+\z/smx){
		$offset = 0;
	}
	if ($offset > $total_hits){
		$offset = $total_hits - $results_per_page;
	}

	# This returns a 'KinoSearch::Search::Hits' object
	$result->seek($offset, $results_per_page);

	my @offsets = map { { offset => $results_per_page * $_, number => $_ + 1 } } 
			0 .. int $total_hits / $results_per_page;

	my @results;

	while (my $hr = $result->fetch_hit_hashref){
		my %h;
		for (qw(revision author message)){
			$h{$_} = $hr->{$_};
		}
        unless ($cgi->param('terse')){
            $h{paths} = [ map { {path => $_} } split m/\s+/, $hr->{paths} ];
        }
		push @results, \%h;
	}
	$t->param(OFFSETS 		=> \@offsets);
	$t->param(RESULTS 		=> \@results);
	$t->param(RESULT_COUNT 	=> $total_hits);

}
