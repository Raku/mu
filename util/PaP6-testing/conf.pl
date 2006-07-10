use strict;
my $make_cmd;

if ( $^O eq 'MSWin32' ) {
    my $rc = system( "mingw32-make --version" );
    if ( $rc == 0 ) {
        $make_cmd = 'mingw32-make';
    } else {
        $make_cmd = 'nmake';
    }
} else {
    $make_cmd = 'make';
}

my ( $conf_cmd );
if ( -e '\usr\lib\icu' ) {
    $conf_cmd = 'perl Configure.pl --cc=gcc --icushared="c:\usr\lib\icu\lib\icudt.lib c:\usr\lib\icu\lib\icuuc.lib" --icuheaders="c:\usr\lib\icu\include" --icudatadir="c:\usr\lib\icu\data"';
} else {
    $conf_cmd = 'perl Configure.pl --cc=gcc --without-icu';
}

my $conf = [];

# Defaults:
# $req_ok = $cmd[-1]
# $name = $cmd

my $smoker_copy_sub = sub {
    my ( $cn, $state, $ver ) = @_;
    unless ( -d $cn->{src_add_dn} ) {
        print "src_add dir '$cn->{src_add_dn}' not found!\n" if $ver > 1;
        return 0;
    }
    my $fn = '.smoker.yml';
    unless ( copy( $cn->{src_add_dn} . '/' . $fn, $cn->{temp_dn} ) ) {
        unlink( $cn->{temp_dn} . '/' . $fn ) if -e $cn->{temp_dn} . '/' . $fn;
        print "Cant't copy '$fn' $!\n" if $ver > 1;
        return 0;
    }
    $fn = 'config.yml';
    if ( -e $cn->{src_add_dn} . '/' . $fn ) {
        unlink( $cn->{temp_dn} . '/' . $fn )  if -e $cn->{temp_dn} . '/' . $fn;
        unless ( copy( $cn->{src_add_dn} . '/' . $fn, $cn->{temp_dn} ) ) {
            print "Cant't copy '$fn' $!\n" if $ver > 1;
            return 0;
        }
    }
    return 1;
};


my $config_yml_rewrite_sub = sub {
    my ( $cn, $state, $ver ) = @_;

    my $fn = 'config.yml';
    if ( -e '../' . $cn->{src_add_dn} . '/' . $fn ) {
        unless ( copy( '../' . $cn->{src_add_dn} . '/' . $fn, './' ) ) {
            print "Cant't copy '$fn' $!\n" if $ver > 1;
            return 0;
        }
    }
    return 1;
};


push @$conf, {
    'name' => 'parrot',
    'repository' => 'https://svn.perl.org/parrot/trunk',

    'after_temp_copied' => $smoker_copy_sub,

    'commands' => [
        { 
            'name' => 'configure',
            'cmd' => $conf_cmd,
            'mt' => 10*60,
        },
        { 
            'name' => 'make',
            'cmd' => $make_cmd,
            'mt'  => 1*60*60,
        },
        { 
            'name' => 'make smoke',
            'cmd' => $make_cmd . ' smoke',
            'mt'  => 30*60,
            'after'  => sub {
                my ( $cn, $state, $ver ) = @_;
                my $rc = copy( 
                    'smoke.html',  
                    $state->{results_path_prefix} . 'smoke.html' 
                );
                print "make smoke - after return code: $rc\n" if $ver > 1;
                return $rc;
            },
        },
        {
            'name' => 'make smoke languages',
            'req_ok' => 'make',
            'mt'  => 15*60,
            'before'  => sub {
                my ( $cn, $state, $ver ) = @_;
                unless ( chdir 'languages' ) {
                    print "$!$@\n";
                    return 0;
                }
                return 1;
            },
            'cmd' => $make_cmd . ' smoke languages',
            'mt'  => 30*60,
            'after'  => sub {
                my ( $cn, $state, $ver ) = @_;
                my $rc = 
                    copy( 
                       'languages_smoke.html',  
                       $state->{results_path_prefix} . 'languages_smoke.html' 
                    ) 
                    && chdir( '..' )
                ;
                return $rc;
            },
        },
    ]
};

$conf_cmd = 'perl Makefile.PL';
$make_cmd = 'nmake';
push @$conf, {
    'skip' => 0,
    'name' => 'pugs',
    'repository' => 'http://svn.openfoundry.org/pugs/',
    'after_temp_copied' => $smoker_copy_sub,

    'commands' => [
        { 
            'name' => 'makefile',
            'cmd' => $conf_cmd,
            'mt' => 10*60,
        },
        { 
            'name' => 'make',
            'cmd' => $make_cmd,
            'mt'  => 1*60*60,
        },
        { 
            'before' => $config_yml_rewrite_sub,
            'name' => 'make smoke',
            'cmd' => $make_cmd . ' smoke',
            'mt'  => 30*60,
            'after'  => sub {
                my ( $cn, $state, $ver ) = @_;
                my $rc = copy( 
                    'smoke.html',  
                    $state->{results_path_prefix} . 'smoke.html' 
                );
                print "make smoke - after return code: $rc\n" if $ver > 1;
                return $rc;
            },
        },
    ]
};


return $conf;
