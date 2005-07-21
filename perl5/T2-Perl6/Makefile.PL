# $Header: $

use strict;

use ExtUtils::MakeMaker;

my $module = "T2::Perl6";
my $file_base = $module;
$file_base =~ s/::/\//g;
$file_base = "./lib/$file_base";
my $pm_file = "$file_base.pm";
my $pod_file = "$file_base.pod";

my $doc_file = -e $pod_file ? $pod_file : $pm_file;

(my $dir = $module) =~ s/::/-/g;

#use lib "../Perl6-MetaModel/lib";

WriteMakefile(
	AUTHOR => 'Sam Vilain <samv@cpan.org>',
	NAME => $module,
	VERSION_FROM => $pm_file,
	INSTALLDIRS => 'perl',
	PREREQ_PM => {
		'T2' => 0.09,
		#'Perl6::MetaModel' => 0,
		'Class::Tangram' => 1.54,
	},
	dist => {
		PREOP=> "rm -f README; pod2text -80 < $doc_file > README; cp -f README $dir-\$(VERSION); "
	}
);
