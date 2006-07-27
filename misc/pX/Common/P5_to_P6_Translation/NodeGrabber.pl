#A quick utility to grab nodetypes from a yaml file
#or find instances of a given node type.
#Useage: NodeGrabber.pl outfile
#OR      NodeGrabber.pl FIND nodetype


if($ARGV==1){
  open OUTFILE, ">$ARGV[0]" or die "Cannot open output file:\n $!";
  my %nodes;
  opendir(CURDIR, ".") or die $!;
  @allfiles = grep !/^\./, readdir CURDIR;
  #print (@allfiles);
  foreach $file (@allfiles){
    open CURFILE, $file or die "$file caused this error: $!";
    while(<CURFILE>){
      if($_ =~ m/P5AST::(.*) $/){
	$nodes{$1} = "P5AST ". $1;
      }elsif($_ =~ m/p5::(.*) $/){
	$nodes{$1} = "p5 ".$1;
      }
    }
    close CURFILE;
  }

  foreach $name (sort (values %nodes)){
    print "$name\n";
    print OUTFILE "$name\n";
  }
  close OUTFILE;
}else{
  opendir(CURDIR, ".") or die $!;
  @allfiles = grep !/^\./, readdir CURDIR;
  #print (@allfiles);
  foreach $file (@allfiles){
    open CURFILE, $file or die "$file caused this error: $!";
    while(<CURFILE>){
      if($_ =~ m/P5AST::$ARGV[1] $/i){
	print "Found in ".$file."\n";
      }elsif($_ =~ m/p5::$ARGV[1] $/i){
	print "Found in ".$file."\n";
      }
    }
    close CURFILE;
  }

}
