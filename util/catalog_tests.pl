#!/usr/bin/perl
use warnings;
use strict;
use File::Find;
use File::Spec::Functions ':ALL';
use File::Path;
use File::Basename;
use IO::File;
use HTML::TreeBuilder;
use HTML::Entities;
use Regexp::Common qw/balanced delimited/;
use Pod::Simple::HTML;
use Pod::PlainText;
use Tie::RefHash;
use List::Util 'first';
use Data::Dumper;
use YAML qw(LoadFile);

use HTML::Template;

$|++;
my $start = time;
() = <<__NOTES__;

Strategy: iterate through all tests first.  Collect links and format as HTML
as we go.  Then get the design docs, and format those as HTML, linking to 
the links, and inserting a names where needed.

__NOTES__



# You will need the Synopses checked out. Do this:
#   cd ..
#
#   svn co $PERL6_DOC ;  mv trunk perl6_doc
# or 
#   svn co $PERL6-BIBLE
#
# where:
#   $PERL6_DOC   =  http://svn.perl.org/perl6/doc/trunk/
#   $PERL6-BIBLE =  http://tpe.freepan.org/repos/iblech/Perl6-Bible/

my $syn_src_dir; # The root directory for Synopsis POD
my $t_dir;       # The root directory of the test tree.
my $output_dir;  # The root directory of the output tree.

# ref to hash of information about links that we mean to insert.
# Top level index is file, second level is an array ref of hash refs.
# FIXME: Document next level.
my $link_info;
my $total_links;
my $total_files;

($syn_src_dir, $t_dir, $output_dir)=@ARGV;
if  ( $syn_src_dir ) {
    die "Synopsis POD directory not found!" unless -d $syn_src_dir;
} else {
    $syn_src_dir = catdir($t_dir, 'Synopsis');
    unless ( -f catfile($syn_src_dir, "S12.pod") ) {
        $syn_src_dir = catdir('..', 'perl6_doc', 'design' );
        $syn_src_dir = catdir('..', 'Perl6-Bible', 'lib', 'Perl6', 'Bible') 
                        unless -f catfile($syn_src_dir, 'syn', "S12.pod");
    }
}

$t_dir      ||= 't';
$output_dir ||= 't_index';

$t_dir       = rel2abs($t_dir);
$output_dir  = rel2abs($output_dir);
$syn_src_dir = rel2abs($syn_src_dir);

my @syn = map { m|^.*/(\D\d\d).pod$|; $1 }
                     (<$syn_src_dir/apo/*>,
                      <$syn_src_dir/exe/*>,
                      <$syn_src_dir/syn/*>);
my $syn_indexs = {};
for (@syn) {
    m/^(\D)/;
    push @{$syn_indexs->{$1}}, 
            {  file => $_ . ".html",
               name => $_,
            };
}

print "Synopsis: $syn_src_dir\n";
print "Tests   : $t_dir\n";
print "Output  : $output_dir\n";
print "\n";
  
my $quotable = qr/\w+|$RE{delimited}{-delim=>'"'}/;
my $link     = qr{(.*?)                                     # Leading bit
                  (L <+
                    ($quotable)(?:/($quotable))?             # Normal bit of link 
                        (?:
                          \s+                                   # Whitespace before re
                          $RE{delimited}{-delim=>'/'}{-keep}    # Regex
                          ([xim]*)                              # RE options
                        )?                                      # End of regex block
                      >+)                                       # End of link
                      (.*)                                      # rest of thing
                }sx;
           

my $index = {};
my (@unresolved, @bad_regex, @bad_heading);

my $tests = LoadFile("tests.yml");
my $files = {};
$files->{$_->{file}} = $_ for @{ $tests->{meat}->{test_files} };


find(\&handle_t_file, $t_dir);

infest_syns($link_info);

my @dirs = sort keys %{$index->{_dirs}};
my $index_file = catfile($output_dir,"index.html");
open( my $fh,'>',  $index_file) or die "Failed to open $index_file: $!";
my $template = HTML::Template->new(filename => 'util/catalog_tmpl/index.tmpl');
my $i = 0;
my $c = int((@dirs+1) / 3)+1;
$template->param(directories => [ map { { title => $_,
                                          wrap  => !(++$i % $c),
                                        }} @dirs ]);
$template->param(updated => localtime() . "");
$template->param(files => $total_files);
$template->param(links => $total_links);
print $fh $template->output();
close $fh;

my $syn_index = catfile($output_dir,"Synopsis", "index.html");
open( $fh, ">",  $syn_index) or die "Failed to open $syn_index: $!";
$template = HTML::Template->new(filename => 'util/catalog_tmpl/Synopsis.tmpl');
$template->param("syn", [ @{$syn_indexs->{S}} ]);
$template->param("exe", [ @{$syn_indexs->{E}} ]);
$template->param("apo", [ @{$syn_indexs->{A}} ]);
print $fh $template->output();
close $fh;

for (@dirs) {
    build_indexes(catdir("t",$_), $index->{_dirs}->{$_});
}

sub build_indexes {
    my $path     = shift;
    my $index    = shift;
    
    return unless exists $index->{_dirs} or exists $index->{_files};
    
    my $index_file = catfile($output_dir, $path, "index.html");
    open (my $fh,'>', $index_file) or die "Failed to open $index_file: $!";
    my @dirs  = sort keys %{$index->{_dirs}};
    my @files = sort @{$index->{_files}};
    my $template = HTML::Template->new(filename => 'util/catalog_tmpl/directory.tmpl');
    my $i = 0;
    my $c = int((@dirs+1) / 3) +1;
    $template->param(directories => [ map { { 
                                            title => $_,
                                            wrap  => !(++$i % $c),
                                             }} @dirs  ]);    
    $template->param(files       => [sort {$a->{file} cmp $b->{file}} @files ]); 
    print $fh $template->output();
    close $fh;
    for (@dirs) {
        build_indexes( catdir($path, $_), $index->{_dirs}->{$_});
    }
} 


my $error_file = catfile($output_dir,"error.html");
open( my $error, '>', $error_file) or die "Failed to open $error_file: $!";
$template = HTML::Template->new(filename => 'util/catalog_tmpl/error.tmpl');
$template->param(unresolved => \@unresolved);
print $error $template->output;
close $error;
print "Took: " . (time - $start) . " sec(s)\n";
# Note: this is intended to be called from File::Find::find as a wanted
# routine, so takes odd parameters.
sub handle_t_file {
  return unless /\.t$/;
  my $input_path    = rel2abs($_);
  my $relative_file = abs2rel($input_path,$t_dir);
  $relative_file =~ s/t$/html/;
  my $output_path   = inpath_to_outpath($input_path);
  my ($path, $file) = $input_path =~ m|^$t_dir/(.*)/(.*)\.t$|;
  my $links = 0;
#  die Dumper( $files->{"t/" . abs2rel($input_path,$t_dir)} );
  mkpath(dirname $output_path);
  my $test_results = $files->{"t/" . abs2rel($input_path,$t_dir)};
  my $lines = {};
  for my $test (@{$test_results->{events}} ) {
      next unless defined $test->{pos};
      my ($line) = $test->{pos} =~ /line (\d+)/;
      next unless $line;
      if (exists $lines->{$line}) {
        $lines->{$line} = 0 if $test->{ok} == 0;
      } elsif ($test->{todo} eq '') {
        $lines->{$line} = 1;
      } else {
        $lines->{$line} = 2;
      }
   }
  my $infile  = IO::File->new($input_path, "<:utf8") 
                         or die "Can't open input test file $input_path: $!";
  my $outfile = IO::File->new($output_path, ">:utf8")
                         or die "Can't open output test file $output_path: $!";
                         
  my $template = HTML::Template->new(filename => '/home/eric256/pugs/util/catalog_tmpl/code.tmpl');  
  $template->param("file" => $file);
  my $output = ""; 
  
  while (my $rest = <$infile>) {
    chomp $rest;
    $output .= "<a name='line_$.'></a>";
    while ( $rest =~ $link ) { 
      my $text = $1;
      my $whole = $2;
      my $linkfile = $3;
      my $linkhead = $4;
      # $5  captures the entire match
      # $6  captures the opening delimiter (provided only one delimiter was specified)
      my $regex    = $7; 
      # captures delimited portion of the string (provided only one delimiter was specified)
      # $8  captures the closing delimiter (provided only one delimiter was specified)
      my $reopts   = $9;
      $rest        = $10;
      
      $linkfile = $1 if ($linkfile =~ /^"(.*)"$/);
 
      if ($linkfile =~ /^http/) {
        $output .= "$text <a href='$linkfile'>$whole</a>";
        $links++;
        next;
      }
      
      $linkhead = $1 if ($linkhead =~ /^"(.*)"$/);
#     $body->push_content(HTML::Element->new('pre')->push_content($text));
      
      my $link = {};
      $link->{linkfile} = $linkfile;
      $link->{linkhead} = $linkhead;
      $link->{whole}    = $whole;
      $link->{relfile}  = $relative_file;
      if ($regex) {
        $reopts||='';
        $link->{regex} = eval "qr/$regex/$reopts";
      }
      $link->{sourcepath}=$output_path;
      $output .= $text;
      my $syn_path = catfile($output_dir, "Synopsis", "$linkfile.html");
      $output .= "<a href='". abs2rel($syn_path, dirname($output_path)) . "#" .(0+$link) . 
                    "' id='" . (0+$link) . "'>" . encode_entities($whole) . "</a>";
      push @{$link_info->{$linkfile}}, $link;
      $links++;
    }
    
    if ($rest) {
       $rest = encode_entities($rest);
#      $body->push_content($rest);
      my $test_class = {
                      0 => 'test_fail',
                      1 => 'test_pass',
                      2 => 'test_todo',
                    };
      my $class = 'non_test';
      if (exists $lines->{$.}) {
        $class = $test_class->{$lines->{$.}};
      }
      $output .= "<span class='$class'>$rest</span>";
      
    }
#    $body->push_content("\n");
     $output .= "\n";
  }
  $template->param("tests", $output);
  my $data = { 
        file   => $file, 
        links  => $links,
        ok     => $test_results->{results}->{ok},
        todo   => $test_results->{results}->{todo},
        failed => ($test_results->{results}->{seen} || 0) -
                  ($test_results->{results}->{ok} || 0),
  };
  my (@paths) = splitdir($path);
  my $loc  = 'push @{$index';
  $loc .= "->{_dirs}->{" . $_ . "}" for @paths;
  $loc .= "->{_files}} , \$data";
  eval $loc;
  $total_links += $links;
  $total_files++;
  $outfile->print($template->output);
}

sub infest_syns {
    my $index = shift;

    my $p = Pod::PlainText->new(width => 1000);

    mkpath(my $syndir = catdir($output_dir, "Synopsis"));
    
    for my $syn (@syn) {
        # create HTML out of the pod
        
        my $synhtml = catfile($syndir     , "$syn.html");
        my $synpod  = catfile($syn_src_dir, "$syn.pod" );
        unless ( -f $synpod ) {
            if    ( $syn =~ /^S/i ) { $synpod = catfile($syn_src_dir, 'syn', "$syn.pod"); }
            elsif ( $syn =~ /^A/i ) { $synpod = catfile($syn_src_dir, 'apo', "$syn.pod"); } 
            elsif ( $syn =~ /^E/i ) { $synpod = catfile($syn_src_dir, 'exe', "$syn.pod"); } 
        }
        Pod::Simple::HTML->parse_from_file($synpod, $synhtml);
        
        # and parse it into a tree
        my $sobj = HTML::TreeBuilder->new_from_file($synhtml);

        # this makes it prettier
        $sobj->look_down(_tag=>"head")->push_content(
                HTML::Element->new("link", rel  => "stylesheet", 
                                           type => "text/css", 
                                           href => "http://dev.perl.org/css/perl.css"
                                   ));
        if (exists $index->{$syn}) {
        # This makes later processing easier
        $sobj->objectify_text;

        for my $headlevel (reverse 1..7) {
          my $tag = 'h'.$headlevel;
          
          while (my $beg = $sobj->look_down(_tag => $tag)) {

            my $beg_n = $beg->pindex;
            my $end   = $beg->right;
            
            if (!defined $end) {
              $beg->tag('div');
              $beg->attr('class', 'empty_head '.$tag);

              next;
            }

            $end = $end->right until (!$end->right or $end->right->tag eq $tag);
            
            my $end_n = $end->pindex;

            my $name = join '', 
              map {$_->attr('text')} 
                $beg->look_down(_tag=>'~text');

            $name = $1 if $name =~ m/^"(.*)"/;

            my $div = HTML::Element->new('div', class=>$tag, name=>$name);
            
            my @kids = $beg->parent->splice_content($beg_n, $end_n-$beg_n+1, $div);
            $div->push_content(@kids);
            $kids[0]->tag('div');
          }
        }
        
        $sobj->deobjectify_text;

        tie my %sup_links, 'Tie::RefHash';
        foreach my $link (reverse @{ $index->{$syn} }){
          # reverse is since we're splicing right after the h1
            my $target  = $link->{linkfile};
            my $heading = $link->{linkhead};
            my $source  = $link->{sourcepath};
            my $regex   = $link->{regex};

            # create a representation that is like the $html->as_text
            $heading = $p->interpolate($heading);
            $heading =~ s/^\s+|\s+$//g;;
            $heading =~ tr/`'//d;

            if ($heading) {
                my $heading_re = qr/^\Q$heading\E$/i;

#               print STDERR "Trying to get heading >$heading<\n";

                my $h = $sobj->look_down( _tag  => 'div', 
                                          class => qr/^h\d$/,
                                          name  => $heading_re);
                
                unless ($h) {
                    push @unresolved, { target   => $target, 
                                        heading  => $heading,
                                        relative => $link->{relfile} };
                    next;
                };

                # create the backlink <a href...>
                my $backlink = HTML::Element->new('a', 
                         href  => (abs2rel($source, dirname($synhtml)) . "#" . 
                                  (0+$link)), id=>0+$link, 
                         title => $link->{whole},
                         class => 'testlink');
                # $backlink->push_content(abs2rel($source, $output_dir));
                $h->push_content($backlink);
                my $t = HTML::Element->new('sup');
                $t->push_content('T');
                $backlink->push_content($t);

                
                my $found;
                if ($regex) {
                    # we're skipping forward till we find a regex
                    my @stuff = $h->look_down(sub {$_[0]->as_text =~ $regex});
                    if (!@stuff) {
                      goto notregex;
                    }
                    # Prefer deeper or earlier matches.
                    @stuff = sort {$b->depth   <=> $a->depth    or
                                   $a->address cmp $b->address
                                  } @stuff;

                    $h = $stuff[0];
                    
#                   $h->dump(\*STDERR);

                    my $i=-1;
                    foreach ($h->content_list) {
                      $i++;
                      next if ref $_;
                      next unless /(.*)($regex)(.*)/;
                      
                      $h->splice_content($i, 1, $1, $2, $backlink, $3);
                      $found = 1;
                      last;
                    }

                    if (!$found) {
                      # Part of the content is inside a pre.
                      $h->push_content($backlink);
                      $found = 1;
                    }
                  }
                
              notregex:
                # insert just a normal link, after the header, when there is no regex
                # or if the regex failed
                unless ($found) {
                  push @bad_regex, { regex   => $regex,
                                     target  => $target,
                                     heading =>  $heading, 
                                     source  => $source } if $regex;
                  ($h->content_list)[0]->push_content($backlink);
                }
                

              } else {
                # perhaps L<S02> etc should just link to the top?
                # this is what you get at the moment
                push @bad_heading, "link in $source to $target does not have a heading\n";
              }
            
          }
        }
        # finally, write out the synopsis
        my $outfile = IO::File->new(">$synhtml") 
                             or die "Can't open output test file $synhtml: $!";
        $outfile->print($sobj->as_HTML(undef, ' ', {}));
    }
}

sub inpath_to_outpath {
    my $inpath  = shift;
    my $outpath = abs2rel($inpath, $t_dir);
    $outpath    = rel2abs(catfile("t", $outpath), $output_dir);
    $outpath    =~ s/\.t$/\.html/;
    return $outpath;
}
