use strict;
use warnings;

use lib 'inc';
use MyBuilder;

my $version = "0.01";#get_version();


my $builder = MyBuilder->new
  (
   module_name    => 'SMOP',
   license        => 'perl',
   dist_author    => ['Daniel Ruoso <daniel@ruoso.com>',
                      'Pawel Murias <pawelmurias@gmail.com>'],
   needs_compiler => 1,
   build_requires => {
#                      'File::Copy'         => '0',
#                      'Capture::Tiny'      => '0.06',
#                      'Config::AutoConf'   => '0.14',
#                      'ExtUtils::CBuilder' => '0.27',
#                      'Module::Build'      => '0.3603',
                     },
   add_to_cleanup => [
#                      'Text-BibTeX-*',
## NOT SURE YET        'btparse/src/bt_config.h',
#                      'btparse/src/*.so',
#                      'btparse/src/*.dylib',
#                      'btparse/src/*.dll',
#                      'btparse/src/*.o',
#                      'btparse/progs/*.o',
#                      'btparse/progs/dumpnames',
#                      'btparse/progs/bibparse',
#                      'btparse/progs/biblex',
#                      'btparse/tests/postprocess_test',
#                      'btparse/tests/read_test',
#                      'btparse/tests/simple_test',
#                      'btparse/tests/macro_test',
#                      'btparse/tests/case_test',
#                      'btparse/tests/name_test',
#                      'btparse/tests/purify_test',
                     ],
  );

## HACK  HACK  HACK  HACK
my $libdir = $builder->install_destination("bin");
if ($^O =~ /mswin32/i) {
    $libdir = $ENV{WINDIR} || 'c:\\WINDOWS\\'; ## HACK HACK HACK
} else {
    $libdir =~ s/\bbin\b/lib/;
}
#$builder->notes('btparse_version' => $version);
$builder->notes('lib_path' => $libdir);
$builder->add_build_element('usrlib');
$builder->install_path( 'usrlib' => $libdir );

$builder->create_build_script;

#sub get_version {
#    my $version = undef;
#    open PM, "lib/Text/BibTeX.pm" or die "Cannot open 'lib/Text/BibTeX.pm' for reading: $!\n";
#    while (<PM>) {
#        if (m!^our\s+\$VERSION\s*=\s*'([^']+)'!) {
#            $version = $1;
#            last;
#        }
#    }
#    close PM;
#    die "Could not find VERSION on your .pm file. Weirdo!\n" unless $version;
#}


