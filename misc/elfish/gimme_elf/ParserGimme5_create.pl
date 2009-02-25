#! /usr/bin/perl -w
use strict;
use warnings;
use utf8;

my $std = slurp("STD.pmc");
my $cursor = slurp("Cursor.pmc");
my $mangle = slurp("mangle.pl");
my $lazymap = slurp("LazyMap.pm");
my $bluerun = slurp("STD_blue_run");

$std =~ s/use Cursor;/BEGIN { Cursor->import(); };/ || die "bug";

$cursor =~ s/require 'mangle.pl';// || die "bug";
$cursor =~ s/use LazyMap (.*?);/BEGIN { LazyMap->import($1); };/ || die "bug";

my $bluerun_extract = "";
while($bluerun =~ /\n#MARK_for_elfg5\((.*?\n)#MARK_for_elfg5\)/gcs) {
  $bluerun_extract .= $1;
}

my $code = join("\n",map{ "\n#FILEMARK\n{package main;\nno strict;\n".$_."\n}\n" }
                ($mangle,$lazymap,$cursor,$std,$bluerun_extract));

unslurp("test.pl",$code);

my $code_string = $code;
$code_string =~ s/\\/\\\\/g;
$code_string =~ s/\'/\\\'/g;
$code_string = "'".$code_string."'";

my $parser = <<'END';
class Parser2 is Parser {

  method parse($p6_code,$claim_as_filename) {
    my $msg = "Parse error in: "~$claim_as_filename~"\n";
    my $dump = $.parse_code($p6_code);
    if not($dump) { die($msg) }
    fastundump($dump);
  };

  method parse_code ($code) is p5 {' #kludge
    use File::Temp qw{tempfile};
    my($fn,$filename) = tempfile(undef, UNLINK => 1);
    binmode($fn,":utf8");
    print $fn $code;
    close($fn);
    my $r = STD->parsefile($filename);
    return undef if !$r;
    $main::whole_file = $code;
    $main::whole_file .= " "; # -e "3" dump includes some _pos of 2
    my $ast = $r->to_dump0."\n";
    return $ast;
  '}

};

if not($*parser0) { $*parser0 = Parser2.new('is_for_active_runtime',1) }
$*parser1 = Parser2.new;
END

my $emitter = '
class EmitSimpleP5 {
  method prelude_extras1 {
'.$code_string.'
  }
}
';
my $runtime = <<'END';
_inline_p5('eval(EmitSimpleP5->prelude_extras1()); die $@ if $@;');
END

my $src = $parser.$emitter.$runtime;

unslurp("ParserGimme5.pm",$src);


sub slurp {
  my($filename)=@_;
  open(F,'<:utf8',$filename) or die "Can't open $filename: $!\n";
  my $text; local $/; $text = <F>; close F;
  $text;
}
sub unslurp {
  my($filename,$text)=@_;
  open(F,'>:utf8',$filename) or die "Can't open $filename: $!\n";
  print F $text; close F;
  undef;
}
