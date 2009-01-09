use strict;
use lib "../..", "../../inc";
use inc::Module::Install (
    -d '../../misc/pX' ? (prefix => '../../inc') : ()
);
use File::Path;

name       ('Perl6-Doc');
version    (0.01);
license    ('perl');

install_script( 'p6doc' );
makemaker_args( PMLIBDIRS => [ grep { -d } glob("[A-Z]*") ]);

system "$^X Spec/update";

# On Win32 using nmake-1.5 the Makefile produced will die.
# This is a result of a heuristic in ExtUtils::MM_Any::split_command().
# There is a bug open on this
#
#   http://rt.cpan.org/Public/Bug/Display.html?id=20145
#
# Install a cheat until a real fix comes along.
my $holdme = \&ExtUtils::MM_Win32::max_exec_len;

*ExtUtils::MM_Win32::max_exec_len = sub {
    my $self = shift;
    $self->{_MAX_EXEC_LEN} ||= 1.67 * 1024;
};

# Do the needful for writing the Makefile.
WritePugs  (5);

# Remove our cheat so other modules get expected behavior.
*ExtUtils::MM_Win32::max_exec_len = $holdme;
