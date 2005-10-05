class XML::SAX-0.01;

use File::Spec <catfile splitpath>;

our @.known_parsers;
my $parser_details = 'ParserDetails.ini';
# use 
# my constant $:parser_details = 'ParserDetails.ini';
# as soon as it works.

method load_parsers(Class $class: Str ?$dir is copy, Str ?$file is copy) returns Class {
	@.known_parsers = ();

	$dir //= catfile((splitpath(%*INC<XML/SAX.pm>))[1], 'SAX');
	$file //= $parser_details;

	my $fh = open(catfile($dir, $file), :r);
	unless ($fh) {
		XML::SAX.do_warn("could not open $file in $dir\n");
		return $class;
	}

	@.known_parsers = $class._parse_ini_file($fh);

	return $class;
}

method do_warn(Class $class: @args) { warn(@args); }

method _parse_ini_file(Class $class: IO $fh) {
	my @config;

	my $lineno = 0;
	for =$fh -> $line is copy {
		++$lineno;
		$line ~~ s:perl5/[#;].*$//;
		$line ~~ s:perl5/\s*$//;
		$line ~~ s:perl5/^\s*//;

		next if $line ~~ m:perl5/^$/;

		if ($line ~~ rx:perl5/^\[\s*(.*)\s*\]$/) {
			@config.push({Name => $0});
			next;
		} elsif ($line ~~ rx:perl5/^(.*?)\s*?=\s*(.*)$/) {
			@config.push({Name => ''}) unless +@config;
			@config[-1]{"Features"}{$0} = $1;
		} else {
			die "Invalid line in ini: $lineno\n>>> $line";
		}
	}

	return @config;
}
