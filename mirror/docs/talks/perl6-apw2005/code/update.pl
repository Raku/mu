#!/usr/bin/perl
use strict;
use warnings;
use locale;
use utf8;
use POSIX qw(locale_h);

setlocale(LC_CTYPE,"de_DE.utf8");

sub slurp {
    my ($file) = @_;
    no warnings;
    open FH, "< $file" or die "can not open '$file' for reading";
    local $/;
    my $ret = <FH>;
    close FH;
    return $ret;
}

#sub syn {
#    my ($segil, $name, $space) = @_;
#    
#
#    $space = "" if $space =~ /\n/;
#    print "[$segil][$name][$space]\n";
#    # var
#    if (length $segil != 0) {
#	$segil =~ s/^([\$\%])/\\$1/;
#        return "\\synvar\{$segil$name$space}";
#	
#    # digits
#    } elsif ($name =~ m/\d+/) {
#        return "{$name$space}";
#
#    # func
#    } else {
#        return "$segil\\synfunc{$name$space}";
#    }
#}

sub printlatex {
    my ($chr, $col) = @_;

    if ($chr =~ /([\[\]\<\>\s\t\$\%_\}\&\#\{\\\^\~])/) {
	if ($chr =~ /\t/) {
	    print "\\ " x 4;
	    return;
	} elsif ( $chr eq "\\") {
	    print "\\textbackslash{}";
	    return;
	} elsif ($chr =~ /\^/) {
	    print "\\textasciicircum{}";
	    return;
	} elsif ($chr =~ /\~/) {
	    print "\\textasciitilde{}";
	    return;
	} elsif ($chr =~ /\</) {
	    print "\\textless{}";
	    return;
	} elsif ($chr =~ /\>/) {
	    print "\\textgreater{}";
	    return;
	} elsif ($chr =~ /([\[\]])/) {
#	    print "\\" if $col == 1;
	    print "{$1}";
	    return;
	}
	print "\\";
    }
    print "$chr";
}

sub process_file {
    my ($fname) = @_;
    print STDERR "processing $fname...\n";
    my $file = slurp($fname);
    # remove the description line
#    my $desc = $1 if $file =~ s/^#\s*(.*)\n//;
    my @file = split //, $file;

    open STDOUT, "> $fname.tex" or die "can not open '$fname.tex' for writing";

    my $inblock = 0;
    my $col = 0;
#    print '\frametitle{', "$desc}\n";
#    print "\\begin{block}{desc}\\texttt\n{";

    while ( defined(my $chr = shift @file) ) {
	++$col;
	
	if ( $chr =~ /([\'\"])/ ) {
	    my $end = $1;
	    print '\synstr{';
	    while (scalar @file) {
		++$col;
		printlatex $chr, $col;
		$chr = shift @file;
		last if $chr =~ /$end/;
	    }
	    print "$chr}";

	# comments
	} elsif ( $chr =~ /#/ ) {
	    # special comment? (embedded latex)
	    if ($file[0] eq '!') {
		shift @file;
	        while (scalar @file) {
		    $chr = shift @file;
		    last if $chr =~ /\n/;
		    print $chr;
		}
	        print "{}\n";
		next;
	    } elsif ($file[0] eq '=') {
		print '}\end{block}' if $inblock;
		$inblock = 1;
		print '\begin{block}{';
		shift @file;
	        while (scalar @file) {
		    $chr = shift @file;
		    last if $chr =~ /\n/;
		    print $chr;
		}
		print "}\\texttt{";
		next;
	    }
	    
	    # normal comment
	    print '\syncomment{';
	    while (scalar @file) {
		if ($chr eq "#" and $file[0] eq "!") {
		    ++$col;
		    shift @file;
		    while (scalar @file) {
			$chr = shift @file;
			last if $chr =~ /\n/;
			print $chr;
		    }
		    next;
		}
		printlatex $chr, $col;
		$chr = shift @file;
		last if $chr =~ /\n/;
	    }
	    print "} \\\\\n";
	    $col = 0;
	} elsif ( $chr =~ /\n/ ) {
	    print "\\ \\\\\n";
	    $col = 0;
	} else {
	    printlatex $chr, $col;
	}
    }
    print "}\n\\end{block}\n" if $inblock;
#    print "}";
    close STDOUT;
}

#    my ($fname) = @_;
#    print "processing $fname...\n";
#    my $file = slurp $fname;
#    my $desc = "$fname";

#    if ($file =~ s/^\#\s*(.*)\n//m) {
#	$desc = $1;
#    }
#    # backslash
#    $file =~ s/\\/\\textbackslash\{\}/g;
#    # segils
##    $file =~ s/([\$])/\\$1/gm;
#    # latex special chars
#    $file =~ s/([&\{\}])/\\$1/gm;
#    # syntax highlighting
#    $file =~ s/([\$\@\&\%]?)([a-zA-Z0-9_]+)(\s?)/&syn($1,$2,$3)/gme;
#    # comments
#    $file =~ s/#(.*)$/XXX$1YYY/gm;
#    
#    $file =~ s/ /\\ /g;
#    $file =~ s/\^/\\textasciicircum\{\}/g;
#    $file =~ s/\~/\\textasciitilde\{\}/g;
#    $file =~ s/^(.+)$/$1\\\\/gm;
#    $file =~ s/^\s*$/\\ \\\\/gm;
#    $file =~ s/\t/\\ \\ \\ \\ /gm;
#
#    chomp $file;
    
#    open FH, "> $fname.tex" or die "can not open '$fname.tex' for writing";
#    print FH <<"EOT";
#\\begin{block}{$desc}\\texttt
#{$file
#}
#\\end{block}
#EOT
#    close FH;

foreach my $file ( <*/*.pl> ) {
#foreach my $file ( @ARGV ) {
    next if $file eq "update.pl";
    process_file $file;
}
