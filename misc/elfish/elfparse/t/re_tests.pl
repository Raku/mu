
package Pkg_re_tests;
use strict;

our($a,$b,$bl); # this is questionable?

# verbatim from perl-5.9.2/t/op/regexp.t
our $bang = sprintf "\\%03o", ord "!"; # \41 would not be portable.
our $ffff  = chr(0xff) x 2;
our $nulnul = "\0" x 2;

sub match_or_undef_to_s { my $x = shift; if(defined($x)) { $x->match_string } else { "" } }

my $debug_warnings = 1;
my @tests = `cat t/re_tests`;
sub test {
    my($f)=@_;
    print "1..",0+@tests,"\n";
    for my $line (@tests) {
	chop($line);
	print "        \# $line\n";
	print STDERR "        \# $line\n" if $debug_warnings;
	my($pat,$str,$ok,$thing,$value,@rest)=split(/\t/,$line);
	$value = "" if !defined($value);

	if ($pat =~ /\+\)\+/) {
	    print "not ok \# skipped as problematic\n";
	    next;
	}

	my $re = $pat;
	my $mods = "";
	if ($re =~ /^\'/) {
	    $re =~ s/^\'//;
	    $re =~ s/\'([^\']*)$//;
	    $mods = $1;
	}
	if(1) { # Assume variables are already interpolated.
	    $re =~ s/\${bang}/$bang/eg;
	    $re =~ s/\${nulnul}/$nulnul/eg;
	    $re =~ s/\${ffff}/$ffff/eg;
	}
	if(1) { # Fake scope access.
	    $re =~ s/\$(a|b|bl)\b/"\$".__PACKAGE__.'::'.$1/eg;
	    $a = $b = $bl = undef;
	}
        my $qr = eval{ $f->($mods,$re) };
	if (!defined $qr) {
            my $err = $@; $err =~ s/^/\# /m;
	    if($ok =~ /c/) { print "ok\n"; }
	    else {
		print "not ok \# Unexpected compilation failure.\n";
                print STDERR "Unexpected compilation failure.\n$err\n";
                if($debug_warnings && $err =~ /Parse failed/) { # found a STD/gimme5 bug
                  print STDERR "UNEXPECTED PARSEFAIL FOR P5 RE:   $re   $mods\n";
                }
	    }
	    next;
	}
	if($ok =~ /c/) {
	    print "not ok  \# Expected compile to fail.\n";
	    next;
	}


	print STDERR "  eval \"$str\"\n" if $debug_warnings;
	my $strx = eval("\"$str\""); die "bug $@\n$str\n" if $@;
	$strx = $str if $str =~ /\\b/i;  # a\\b    a\b     y       $&      a\b
	my $m = $qr->($strx);
	my $expr = $thing;
	$expr =~ s/([^\\])\\\$([1&])/$1\\x{24}$2/g; # work around two uninteresting tests.
	$expr =~ s/\$&/\".\$m->match_string.\"/g;
	$expr =~ s/\$([0-9]+)/'".match_or_undef_to_s($m->postcircumfix__91_32_93('.($1-1).'))."'/ge;
	$expr =~ s/\$\-\[0\]/\".\$m->from.\"/g;
	$expr =~ s/\$\+\[0\]/\".\$m->to.\"/g;
	$expr =~ s/\$\-\[([0-9]+)\]/'".$m->postcircumfix__91_32_93('.($1-1).')->from."'/ge;
	$expr =~ s/\$\+\[([0-9]+)\]/'".$m->postcircumfix__91_32_93('.($1-1).')->to."'/ge;
	$expr =~ s/\@\-/\".join(" ",\$m->from,map{\$_->from}\@{\$m->match_array}).\"/g;
	$expr =~ s/\@\+/\".join(" ",\$m->to,map{\$_->to}\@{\$m->match_array}).\"/g;
	$expr = "\"$expr\"";
	if ($ok !~ /[ybB]/) {
	    if ($m->match_boolean) {
		print "not ok \# Unexpected successful match.\n";
	    } else {
		print "ok\n";
	    }
	} else {
	    if (!$m->match_boolean) {
		print "not ok \# Match failed.\n";
	    } else {
		print STDERR "  eval $expr\n" if $debug_warnings;
		my $res = eval($expr); print STDERR "#" x 70,"\n","# BUG $@# $expr\n" if $@;
		my $valuex = $value;
		if($value =~ /\${|\\n/){
		    $valuex = eval("\"$value\""); die "bug $@\n$value\n" if $@;
		}
		if ($res ne $valuex) {
		    print "not ok\n  \# Expected: >$valuex<\n  \#      Got: >$res<  from  $expr\n";
		} else {
		    print "ok\n";
		}
	    }
	}
    }
}

#do {
#    # $use_native_ui = 1;
#    test(sub{my($re,$mod)=@_;sub {my($s)=@_; eval("$s =~ /$re/$mod") }});
#    exit;
#} if 0;
#test(sub{sub{undef}}) if $0 =~ /^re\d\d/;

1;
__END__
