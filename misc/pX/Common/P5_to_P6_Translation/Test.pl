#This is a simple test script to run the translator across all files in the directory in which it is run.
#Useage is:
#    $ perl Test.pl [PATH]
#to run a compiled version stored at PATH (which is optional). It can also be run as
#    $ perl Test.pl -i [PATH]
#which runs ghc seperately for each test. This option is very slow, but it is useful when the code does not compile (such as on Intel Macs without special consideration).

use File::Find;
use Cwd;

sub test {
  if($_ =~ /\.yml$/ && -s > 100){
    $file = $_;
    $filepath = $File::Find::dir;
    $newfile = $file;
    $newfile =~ s/\.yml$//;
    $startdir = cwd();
    chdir $filepath;
    if($prog eq "translate"){
      $command = "\"".$path."/translate\" -Oo -U ".$file." ".$newfile;
    }else{
      $command = "ghc-6.4.1 -e \"Main.mainParse \\\"".$file."\\\" \\\"".$newfile."\\\" \\\"ou\\\"\" ".$path."ASTTranslate-sage.hs ".$path."ASTDefinition.hs ".$path."ASTUtil.hs ".$path."ASTParser.hs";
    }
    $res = `$command`;
    if(($res =~ /UnknownAbs/) | ($res =~ /UnknownLit/)){
      print "$filepath$file looks fine, but has unknown nodes.\n";
      $unknown = $unknown + 1;
      print DETAIL "$res\n";
      print UNKNOWN "$res\n";
    }elsif($res =~ /line 1, column 1/){
      print "I don't think I need to check $filepath$file, it looks empty";
    }elsif($res =~ /Finished/){
      print "$filepath$file looks fine.\n";
      $fine = $fine + 1;
      print DETAIL "$res\n";
    }elsif($res =~ /Error/){
      print "***Parse error in $filepath$file***\n";
      $parses = $parses + 1;
      print DETAIL "$res\n";
    }elsif($res =~ /Exception/){
      print "***Exception in $filepath$file***\n";
      $errors = $errors + 1;
      print DETAIL "$res\n";
    }else{
      print "***Parse error in $filepath$file***\n";
      $parses = $parses + 1;
      print DETAIL "$res\n";
    }
    chdir $startdir;
  }
}

$startdir = cwd();
$switch = shift @ARGV;
if($switch eq "-i"){
  $prog = "ghc-6.4.1 -e \"Main.mainParse ";
  $path = $switch;
}else{
  $prog = "translate";
  $path = shift @ARGV;
  if($path eq ""){
    $path = cwd();
  }
}
$errors = 0;
$parses = 0;
$unknown = 0;
$fine = 0;
open(DETAIL, "> Test.detail") or die "Cannot open .detail file: $!";
open(UNKNOWN, "> Test.unknown") or die "Cannot open .unknown file: $!";

find(\&test, ".");

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

print DETAIL "\n\n";
print DETAIL "**********************Report**********************\n";
print DETAIL "*Total Files Attempted: ".$total."\n";
print DETAIL "********\n";
print DETAIL "*Succesful Files: ".$fine."\n";
print DETAIL "********\n";
print DETAIL "*Files with Unknown Nodes: ".$unknown."\n";
print DETAIL "********\n";
print DETAIL "*Files with Parse Errors: ".$parses."\n";
print DETAIL "********\n";
print DETAIL "*Files with Exceptions: ".$errors."\n";
print DETAIL "********\n";
print DETAIL "*Percentage of files succesfully handeled: ".((($fine+$unknown)/$total)*100)."%\n";
print DETAIL "*(This includes files with unknown nodes as succesful)\n";
print DETAIL "**************************************************\n";
