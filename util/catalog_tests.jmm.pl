#!/usr/bin/perl
use warnings;
use strict;
use File::Find;
use File::Spec::Functions ':ALL';
use File::Path;
use File::Basename;
use IO::File;
use HTML::TreeBuilder;
use Regexp::Common qw/balanced delimited/;
use Pod::Simple::HTML;
use Pod::PlainText;
$|++;

() = <<__NOTES__;

Strategy: iterate through all tests first.  Collect links and format as HTML
as we go.  Then get the design docs, and format those as HTML, linking to 
the links, and inserting a names where needed.

__NOTES__

my $t_dir;      # The root directory of the test tree.
my $output_dir; # The root directory of the output tree.
# ref to hash of information about links that we mean to insert.
# Top level index is file, second level is an array ref of hash refs.
# FIXME: Document next level.
my $link_info;

($t_dir, $output_dir)=@ARGV;
$t_dir ||= 't';
$output_dir ||= 't_index';

$t_dir = rel2abs($t_dir);
$output_dir = rel2abs($output_dir);
print "From tree at $t_dir to tree at $output_dir\n";

find(\&handle_t_file, 't');

infest_syns($link_info);

# Note: this is intended to be called from File::Find::find as a wanted
# routine, so takes odd parameters.
sub handle_t_file {
  return unless /\.t$/;
  my $input_path=rel2abs($_);
  my $output_path=inpath_to_outpath($input_path);
  
  print "$input_path => $output_path\n";
  
  mkpath(dirname $output_path);

  my $infile = IO::File->new("<$input_path") or die "Can't open input test file $input_path: $!";
  my $outfile = IO::File->new(">$output_path") or die "Can't open output test file $output_path: $!";
  
  my $outtree = HTML::TreeBuilder->new_from_content("<html><head><title></title></head><body><tt></tt></body></html>");
  
  $outtree->look_down(_tag=>'title')->push_content($input_path);
  my $body = $outtree->look_down(_tag=>'tt');
  
  my $quotable = qr/\w+|$RE{delimited}{-delim=>'"'}/;
  
  while (my $rest = <$infile>) {
	chomp $rest;
	while ($rest =~ m{
					  (.*?)                                     # Leading bit
					  (L <+
					    ($quotable)(?:/($quotable))             # Normal bit of link -- fixme, support URLs.
					    (?:
					      \s+                                   # Whitespace before re
					      $RE{delimited}{-delim=>'/'}{-keep}    # Regex
					      ([xim]*)                              # RE options
					    )?                                      # End of regex block
					  >+)                                       # End of link
                      (.*)                                      # rest of thing
					}sx) {
	  my $text = $1;
	  my $whole = $2;
	  my $linkfile = $3;
	  my $linkhead = $4;
	  # $5  captures the entire match
	  # $6  captures the opening delimiter (provided only one delimiter was specified)
      my $regex=$7;  # captures delimited portion of the string (provided only one delimiter was specified)
      # $8  captures the closing delimiter (provided only one delimiter was specified)
	  my $reopts=$9;
	  $rest=$10;
	  
	  $linkfile = $1 if ($linkfile =~ /^"(.*)"$/);
	  $linkhead = $1 if ($linkhead =~ /^"(.*)"$/);
	  
#	  print STDERR "before link: $text\n";
#	  print STDERR "$whole: linkfile: $linkfile linkhead: $linkhead regex: $regex reopts: $reopts\n";
#	  print STDERR "after link: $rest\n";
	  
	  $body->push_content(HTML::Element->new('pre')->push_content($text));
	  
	  my $link = {};
	  $link->{linkfile}=$linkfile;
	  $link->{linkhead}=$linkhead;
	  if ($regex) {
		$reopts||='';
		$link->{regex} = eval "qr/$regex/$reopts";
	  }
	  $link->{sourcepath}=$output_path;

	  my $syn_path = catfile($output_dir, "Synopsis", "$linkfile.html");
	  $body->push_content(HTML::Element->new(
		'a',
		href => abs2rel($syn_path, dirname($output_path)) . "#" .(0+$link),
		name => 0+$link
	  )->push_content($whole));

	  push @{$link_info->{$linkfile}}, $link;
    }
	
	if ($rest) {
	  $body->push_content($rest);
	}
	$body->push_content(HTML::Element->new('br'));
  }
  
  $outfile->print($outtree->as_HTML(undef, ' '));
  $outtree->delete;
}

sub infest_syns {
	my $index = shift;

	my $p = Pod::PlainText->new(width => 1000);

	mkpath(my $syndir = catdir($output_dir, "Synopsis"));
	foreach my $syn (keys %$index){
		# create HTML out of the pod
		my $synhtml = catfile($syndir, "$syn.html");
		my $synpod = catfile($t_dir, "Synopsis", "$syn.pod");
		Pod::Simple::HTML->parse_from_file($synpod, $synhtml);

		# and parse it into a tree
		my $sobj = HTML::TreeBuilder->new_from_file($synhtml);

		# this makes it prettier
		#$sobj->look_down(_tag=>"head")->push_content(HTML::Element->new("link", rel=>"stylesheet", type=>"text/css", href=>"http://dev.perl.org/css/perl.css"));

		foreach my $link (reverse @{ $index->{$syn} }){ # reverse is since we're splicing right after the h1
			my $target = $link->{linkfile};
			my $heading = $link->{linkhead};
			my $source = $link->{sourcepath};
			my $regex = $link->{regex};

			# create a representation that is like the $html->as_text
			$heading = $p->interpolate($heading);
	   		$heading =~ s/^\s+|\s+$//g;;
			$heading =~ tr/`'//d;

			if ($heading){
				my $heading_re = qr/^\Q$heading\E$/;
				my $h = $sobj->look_down(_tag=>qr/^h\d$/, sub { $_[0]->as_text =~ $heading_re });
				
				unless ($h) {
					warn qq{Couldn't resolve L<$target/"$heading">\n};
					next;
				};

				# create the backlink <a href...>
				my $backlink = HTML::Element->new('a', href=>(abs2rel($source, dirname($synhtml)) . "#" . (0+$link), name=>0+$link));
				
				if ($regex){ # currently doesn't work
					# we need to grep all the elements as_text until the next <h1>,
					# and find the one that matches $regex, and then split it, and
					# make a little <sup> link to the test in place.
					warn "We don't handle regex skips yet\n" }
				#} else {
					$backlink->push_content(abs2rel($source, $output_dir));
					$h->postinsert($backlink, ", ");
				#}
			} else {
				warn "link in $source to $target does not have a heading\n";
			}

			#warn "$target $heading ... $regex -> $source #" . (0+$link);
		}
  		my $outfile = IO::File->new(">$synhtml") or die "Can't open output test file $synhtml: $!";
		$outfile->print($sobj->as_HTML(undef, ' '));
	}
}

sub inpath_to_outpath {
    my $inpath=shift;
    # print "$inpath => ";

    my $outpath = abs2rel($inpath, $t_dir);
    # print "$outpath => ";

    $outpath = rel2abs(catfile("t", $outpath), $output_dir);
    # print "$outpath => ";
    
    # print "\n";
    
    $outpath =~ s/\.t$/\.html/;
    
    return $outpath;
}
