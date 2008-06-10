
package main;

require 5.008000;

use strict;
use warnings;
use ExtUtils::MakeMaker;

my %parms = (
    NAME                => 'yap6',
    AUTHOR              => 'Matthew Wilson <diakopter@gmail.com>',
    ABSTRACT            => 'An implementation of Perl 6 in Perl 5',
    VERSION_FROM        => 'Compiler.pm',
    PM                  => { 'Compiler.pm' => '$(INST_LIBDIR)/Perl6in5/Compiler.pm' },
    EXE_FILES           => [ 'yap6' ],
    PREREQ_PM => {
        'Log::Log4perl'         => 1.16,
        'JSON::XS'              => 2.2,
        'JSON'                  => 2.09,
        'Sub::Exporter'         => 0.979,
        'Getopt::Long'          => '2.37_01',
        'Params::Util'          => 0.33,
        'File::Slurp'           => 9999.13,
        'Data::Structure::Util' => 0.15,
        'Test::Output'          => 0.10,
        'Test::More'            => 0.80,
        'Moose'                 => 0.48,
        'App::Ack'              => 0, # because Perl 6 users ought to need this. :)
    },
    dist                => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
#    clean               => { FILES => 'yap6-1*' },
);

if ( $ExtUtils::MakeMaker::VERSION =~ /^\d\.\d\d$/ and $ExtUtils::MakeMaker::VERSION > 6.30 ) {
    $parms{LICENSE} = 'perl';
}

if ( $ExtUtils::MakeMaker::VERSION ge '6.36' ) {
    $parms{EXTRA_META} = <<EOF;
resources:
    homepage: http://www.perlsix.org/svn/pugs/browse/misc/yap6
    bugtracker: irc://irc.freenode.net/#perl6 - Ask for a commitbit, and add your failing test (case).
    license: http://dev.perl.org/licenses/
    Repository: http://svn.pugscode.org/pugs
    MailingList: http://groups.google.com/group/perl.perl6.compiler/topics
EOF
    }

WriteMakefile( %parms );
