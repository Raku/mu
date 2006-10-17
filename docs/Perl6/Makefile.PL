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

WritePugs  (5);

# Work around a bug in ExtUtils::MM_Any::manifypods_target().
sub MY::manifypods_target {
    my($self) = shift;

    my $man1pods      = '';
    my $man3pods      = '';
    my $dependencies  = '';

    # populate manXpods & dependencies:
    foreach my $name (keys %{$self->{MAN1PODS}}) {
        $dependencies .= " \\\n\t$name";
    }

    foreach my $name (keys %{$self->{MAN3PODS}}) {
        $dependencies .= " \\\n\t$name"
    }

    my $manify = <<END;
manifypods : pure_all $dependencies
END

    my @man_cmds;
    foreach my $section (qw(1 3)) {
        my $pods = $self->{"MAN${section}PODS"};
        push @man_cmds, $self->split_command(<<CMD, %$pods);
	\$(NOECHO) \$(POD2MAN) --section=$section --perm_rw=\$(PERM_RW)
CMD
    }

    $manify .= "\t\$(NOECHO) \$(NOOP)\n" unless @man_cmds;
    $manify .= join '', map { "$_\n" } @man_cmds;

    return $manify;
}
