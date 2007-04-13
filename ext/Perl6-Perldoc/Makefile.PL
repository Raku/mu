use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME                => 'Perl6::Perldoc',
    AUTHOR              => 'Damian Conway <DCONWAY@CPAN.org>',
    VERSION_FROM        => 'lib/Perl6/Perldoc.pm',
    ABSTRACT_FROM       => 'lib/Perl6/Perldoc.pm',
    EXE_FILES           => ['bin/perldoc2text','bin/perldoc2xhtml'],
    PL_FILES            => {},
    PREREQ_PM => {
        'Test::More' => 0,
        'version'    => 0,
        checkVersions([['YAML::Syck','0.63','0.70'],
                      ]),  # check some packages for maximum versions.
    },
    dist                => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean               => { FILES => 'Perl6-Perldoc-*' },
);

sub checkVersions { my @rtn; for (@{ $_[0] }){ my ($pkg,$min,$max) = @$_;
  next unless $pkg; (eval ( "require $pkg" ) && defined $max && eval( '$'.$pkg.'::VERSION' ) > $max &&
    warn "Warning: prerequisite $pkg too new!  Please downgrade to $max or earlier.\n" ) and
    (defined $min && push @rtn,($pkg => $min)); #Let MakeMaker handle any minima.
  }
  return @rtn;
}

1;