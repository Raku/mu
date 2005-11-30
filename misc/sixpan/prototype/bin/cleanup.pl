use strict;
BEGIN {
    use lib         qw[bin];
    require '_inc.pl';
    protoconf->import();
}

system(qq[rm -rf $Root ${Build_prefix}*])                       and die $?;

### set up the 'fakeroot'
### XXX might need to be a bit more elegant, and not blow it away every go
{   system( qq[rm -rf $Root] )                                  and die $?;
    system( qq[mkdir -p $Meta $Site $Tmpdir $Bindir $Builddir $Alternatives ] .
            qq[$Repodir] )                                      and die $?;
    system( qq[touch $Available $Altfile $Repoindex] )          and die $?;
}
