use v6;

use Test;

=pod

Basic tests of C<< %?CONFIG >>, the equivalent to
C<Config.pm>. Most of this is not yet even decided on,
so all of this test can become obsolete on Larrys whim C<:)>

Currently the test is hardcoded to check for the
following values in C<< %?CONFIG >>:

    archlib
    archname
    bin
    cc
    embedded
    exe_ext
    file_sep
    ghc
    installarchlib
    installprivlib
    installsitearch
    installsitelib
    pager
    prefix
    scriptdir
    installbin
    installman1dir
    installman3dir
    installscript
    installsitebin
    installsiteman1dir
    installsiteman3dir
    osname
    path_sep
    perl5_path
    perl_compiler
    perl_revision
    perl_subversion
    perl_version
    privlib
    pugs_revision
    pugs_version
    pugs_versnum
    pugspath
    regex_engine
    sitearch
    sitebin
    sitelib
    siteprefix
    sitescript
    sourcedir
    uname

=cut

my @config = <
    archlib
    archname
    bin
    cc
    embedded
    exe_ext
    file_sep
    ghc
    installarchlib
    installprivlib
    installsitearch
    installsitelib
    pager
    prefix
    scriptdir
    installbin
    installman1dir
    installman3dir
    installscript
    installsitebin
    installsiteman1dir
    installsiteman3dir
    osname
    path_sep
    perl5_path
    perl_compiler
    perl_revision
    perl_subversion
    perl_version
    privlib
    pugs_revision
    pugs_version
    pugs_versnum
    pugspath
    regex_engine
    sitearch
    sitebin
    sitelib
    siteprefix
    sitescript
    sourcedir
    uname
>;

plan @config+2;

diag "Running under $*OS";

ok( defined %?CONFIG, '%?CONFIG is defined' );
ok( %?CONFIG.keys() > 0, '%?CONFIG contains keys and values' );
for @config -> $entry {
    # diag $entry;
    ok( defined %?CONFIG<<$entry>>, '%?CONFIG{'~$entry~'} exists');
};
