

package Pkg_re_tests;
use strict;

my $debug_warnings = 0;
my @tests = `cat t/rx_*`;
sub test6 {
    my($f)=@_;
    print "1..",0+@tests,"\n";
    for my $line (@tests) {
	chop($line);
	print "        \# $line\n";
	next if $line =~ /^\s*\#|^\s*$/;
	print STDERR "        \# $line\n" if $debug_warnings;
	my($pat,$str,$ok,$description,@excess)=split(/\t+/,$line);
	die "assert" if @excess || !defined $ok;
	
	my $re = $pat;
	my $mods = "";

        my $qr = eval{ $f->($mods,$re) };
	if (!defined $qr) {
            my $err = $@; $err =~ s/^/\# /m;
	    if($ok =~ /^\/(?!mob)/) { print "ok\n"; }
	    else {
              print "not ok \# Unexpected compilation failure.\n";
              print STDERR "$err\n";
            }
	    next;
	}
	if($ok =~ /^\/(?!mob)/) {
	    print "not ok  \# Expected compile to fail.\n";
	    next;
	}

	print STDERR "  eval \"$str\"\n" if $debug_warnings;
	my $strx = $str;
	$strx =~ s/(?<!\\)\\u(\w{4})/\\x{$1}/g;
	$strx =~ s/\"/\\\"/g;
	$strx = eval("\"$strx\"");
	# Special case bizarre test strings:
	$strx = '' if $str eq "''";
	$strx = "\t\n\r".' !"#$%&\'()*+,-./:;<=>?@[\]^`_{|}0123456789ABCDEFGHIJabcdefghij'
	    if $str eq '\t\n\r !"#$%&\'()*+,-./:;<=>?@[\]^`_{|}0123456789ABCDEFGHIJabcdefghij';

	my $m = $qr->($strx);


	if($ok eq 'y') {
	    if ($m) {
		print "ok\n";
	    } else {
		print "not ok \# Match failed.\n";
	    }
	}
	elsif($ok eq 'n') {
	    if ($m) {
		print "not ok \# Unexpected successful match.\n";
	    } else {
		print "ok\n";
	    }
	}
	elsif($ok =~ /^\/mob/) {
	    if(!$m) {
		print "not ok \# Match failed.\n";
	    } else {
		$ok =~ /^\/mob\s*([^:]*):\s*(.+)\/$/ or die "assert";
		my($subpart,$value)=($1,$2);
		my $path = join("",map{"->$_"} map{
		    if(/^\d+$/) {"[$_]"}
		    elsif(/^<(.+)>$/) {"{$1}"}
		    else {die "assert"}
		} split(/\s+/,$subpart));
		my $subm = eval("\$m$path");
		if($@ || !defined $subm) {
		    print "not ok \# Submatch $path nonexistant.\n";
		    next;
		}
		eval{$subm->from};
		if($@) {
		    print "not ok \# Submatch $path was not a Match.\n";
		    next;
		}
		$value =~ /^\<(.*) @ (\d+)>/ or die "assert";
		my($s,$at)=($1,$2);
		my $got_s = "$subm";
		my $got_at = $subm->from;
		my $got_ok = $got_s eq $s;
		if(!$got_ok && $s =~ /\\/) {
		    # some of the value "strings" seem to be regexs, eg, \w+
		    eval {$got_ok = 1 if $got_s =~ /\A$s\z/}
		}
		if($got_ok && $got_at == ($at+0)) {
		    print "ok\n";
		} else {
                    $got_at = "" if !defined $got_at;
		    print "not ok\n  \# Expected: >$s< at $at\n  \#      Got: >$got_s< at $got_at\n";
		}
	    }
	}
	else { die "bug" }
    }
}

1;
__END__
