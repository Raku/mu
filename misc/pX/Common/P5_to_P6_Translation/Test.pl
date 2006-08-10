#This is a simple test script to run the translator across all files in the directory in which it is run.
#Useage is:
#    $ perl Test.pl [PATH]
#to run a compiled version stored at PATH (which is optional). It can also be run as
#    $ perl Test.pl -i [PATH]
#which runs ghc seperately for each test. This option is very slow, but it is useful when the code does not compile (such as on Intel Macs without special consideration).

$switch = shift @ARGV;
if($switch eq "-i"){
  $prog = "ghc-6.4.1 -e \"Main.mainParse ";
  $path = $switch;
}else{
  $prog = "translate";
  $path = shift @ARGV;
}
$errors = 0;
$parses = 0;
$unknown = 0;
$fine = 0;
open(DETAIL, "> Test.detail") or die "Cannot open .detail file: $!";
open(UNKNOWN, "> Test.unknown") or die "Cannot open .unknown file: $!";
opendir(CURDIR, ".") or die "Cannot open directory $!";
@allfiles = grep /\.yml$/, readdir CURDIR;

foreach $file (@allfiles){
  if($prog eq "translate"){
    $command = $path."translate -Oo -U ".$file." ".$file.".out";
  }else{
    $command = "ghc-6.4.1 -e \"Main.mainParse \\\"".$file."\\\" \\\"".$file.".out\\\" \\\"ou\\\"\" ".$path."ASTTranslate-sage.hs ".$path."ASTDefinition.hs ".$path."ASTUtil.hs ".$path."ASTParser.hs";
  }
  $res = `$command`;
  if($res =~ /Error/){
    print "***Parse error in $file***\n";
    $parses = $parses + 1;
    print DETAIL "$res\n";
  }elsif($res =~ /Exception/){
    print "***Exception in $file***\n";
    $errors = $errors + 1;
    print DETAIL "$res\n";
  }elsif(($res =~ /UnknownAbs/) | ($res =~ /UnknownLit/)){
    print "$file looks fine, but has unknown nodes.\n";
    $unknown = $unknown + 1;
    print UNKNOWN "$res\n";
  }else{
    print "$file looks fine.\n";
    $fine = $fine + 1;
  }
}

$total = $fine + $parses + $errors + $unknown;
print "\n\n";
print "**********************Report**********************\n";
print "*Total Files Attempted: ".$total."\n";
print "********\n";
print "*Succesful Files: ".$fine."\n";
print "********\n";
print "*Files with Unknown Nodes: ".$unknown."\n";
print "********\n";
print "*Files with Parse Errors: ".$parses."\n";
print "********\n";
print "*Files with Exceptions: ".$errors."\n";
print "********\n";
print "*Percentage of files succesfully handeled: ".((($fine+$unknown)/$total)*100)."%\n";
print "*(This includes files with unknown nodes as succesful)\n";
print "**************************************************\n";
