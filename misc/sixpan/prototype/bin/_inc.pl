package protoconf;

use Cwd;
use Config;
use YAML            qw[LoadFile Dump];
use Data::Dumper;

use base 'Exporter';
@EXPORT = qw[
            $Cwd            $Pms        $Root       $Meta       $Site
            $Ext            $Tmpdir     $Metactrl   $Metafile   $Available
            $Builddir       $Bindir     $Data       $Control    $Fileslist
            $Build_prefix   $Preinst    $Postinst   $Prerm      $Postrm
            $Alternatives   $Altfile    $Repoindex  $Repodir    $Metaext

            package_prefix  package_authority   package_name  package_version
            list_dependencies  dependency_satisfied   dependency_satisfied_by               
        ];


$Cwd            = cwd();
$Pms            = '_jib';                           # meta dir
$Root           = $Cwd . '/fakeroot';               # our makebelief / dir
$Meta           = $Root . '/meta';                  # dir to store metadata
$Metactrl       = $Meta . '/control';               # dir to store control MD
$Available      = $Meta . '/available';             # available packages MD
$Altfile        = $Meta . '/registered-alts';       # registered alternatives
$Alternatives   = $Meta . '/alternatives';          # Alternative link dir                                                    
$Metaext        = '.info';                          # Extension for meta files
$Metafile       = 'META' . $Metaext;                # file with MD in it
$Site           = $Root . $Config{installsitelib};  # site_perl dir
$Tmpdir         = $Root . '/tmp';                   # temp dir for install
$Bindir         = $Root . '/usr/local/bin';         # script dir
$Builddir       = $Root . '/_builddir';             # buildir on client side          
$Data           = 'data.tgz';                       # part of archive /w code
$Control        = 'control.tgz';                    # part with metadata
$Ext            = '.jib';                           # archive extension
$Fileslist      = 'files.list';                     # .packlist equiv
$Build_prefix   = 'root-';                          # builddir prefix
$Preinst        = 'PREINST.pl';                     # run before install
$Postinst       = 'POSTINST.pl';                    # run after install
$Prerm          = 'PRERM.pl';                       # run before uninstall
$Postrm         = 'POSTRM.pl';                      # run after uninstall                       
$Repodir        = $Root . '/jibs';                  # remote repo root
$Repoindex      = $Repodir . '/index';              # index file for the repo

### be strict from here on down
{   use strict;

    ### parse package names easily
    {   my $re = qr/^(\w+)      -   # the prefix
                     ([\w-]+?)  -   # the package name
                     ([\d.]+)   -   # the version
                     (\w+\+\S+) $   # the authority
                /smx;
                
        sub package_prefix      { $_[0] =~ $re; $1 }
        sub package_name        { $_[0] =~ $re; $2 }
        sub package_version     { $_[0] =~ $re; $3 }
        sub package_authority   { $_[0] =~ $re; $4 }
    }
    
    ### takes the metadata object of 1 package, and returns a list of
    ### its dependencies
    sub list_dependencies {
        my $meta    = shift or die "Meta object required";
        my $print   = shift || 0;   # print the parsed dependencies?
        my @deps    = $meta->{depends} ? @{ $meta->{depends} } : ();
        my @index   = LoadFile( $protoconf::Repoindex ); 
        my @avail   = LoadFile( $protoconf::Available );

        ### XXX move the AND to the subroutine
        if( $print and @deps ) {
            print "Translating (dependency for $meta->{package}):\n";
            print Dump( $meta->{depends} );
            #print "\n\nTo:\n";
            #print Dumper( $meta->{depends} );
            print "\n\nTo:\n";
            print _pp_depends( \@deps );
            print $/ . $/;
        }
        
        my @resolved = _parse_depends( \@deps, \@index, \@avail );

    }

    ### XXX merge with _pp_depends!      
    sub _parse_depends {
        my $deps    = shift;
        my $index   = shift or return;
        my $avail   = shift or return;

        my @maybe;
        ### AND context
        RESOLVE: for my $entry ( @$deps ) {
        
            ### plain old entry, we want to resolve this one
            if( ! ref $entry ) {
                
                ### do we have something that satisfies this dep already?
                for my $maybe (@$avail) {
                    next RESOLVE if dependency_satisfied_by($entry, $maybe);
                }

                my @found;
                ### find every package that satisfies this dependency
                for my $maybe (@$index) {
                    ### not the right package
                    next unless dependency_satisfied_by( $entry, $maybe );               

                    push @found, $maybe;
                }
               
                ### nothing satisfied this depenendency
                die "No package satisfies '$entry'\n" unless @found;
                
                ### XXX should be policy based
                my @sorted = sort { $b->{version} <=> $a->{version } } @found;

                push @maybe, $sorted[0];
                
        
            ### OR dependency
            } elsif ( UNIVERSAL::isa( $entry, 'ARRAY' ) ) {

                ### do we have something that satisfies this dep already?
                for my $sub_entry ( @$entry ) {
                    ### $avail, [] is NOT a typo -- rather than having
                    ### _parse_depends see if there's a suitable candidate
                    ### in the whole index, we only let it look at the 
                    ### installed packages. This way, we're sure that when
                    ### it returns a match, it found it in what we already
                    ### had, therefor we don't need to install it
                    ### This above section works for an OR search.
                    ### The [ ] is passed for the AND search, which will
                    ### then not be able to short circuit, as there are
                    ### no available modules on that check
                    eval { _parse_depends( [$sub_entry], $avail, [] ) }
                        and next RESOLVE;

                    
                }


                my @found;
                for my $sub_entry ( @$entry ) {

                    ### we found one that satisfied
                    if( @found = 
                        eval { _parse_depends( [$sub_entry], $index, $avail ) } 
                    ) {

                        ### add to our list and short circuit
                        push @maybe, $found[0];
                        last;
                        
                    ### OR dependency, so try the next                        
                    } elsif ( $@ =~ /^No package satisfies/ ) {
                        next;

                    ### some other error
                    } else {
                        die $@;
                    }                            
                }
    
            ### grouping    
            } elsif ( UNIVERSAL::isa( $entry, 'REF' ) ) {

                ### a mere context switch, just return what ever
                ### the recursive call returns
                push @maybe, _parse_depends( $$entry, $index, $avail );
    
            ### specific version
            } elsif ( UNIVERSAL::isa( $entry, 'HASH' ) ) {
                while (my($k,$v) = each %$entry ) {
                    my ($ver,$op) = reverse split /\s+/, $v;
                    $op ||= '>=';

                    ### do we have something that satisfies this dep already?
                    for my $maybe (@$avail) {
                        next unless dependency_satisfied_by($entry, $maybe);
                        
                        ### is the version good enough?
                        if ( eval "$maybe->{version} $op $ver" ) {
                            keys %$entry;   # reset keys!!!!!
                            next RESOLVE;
                        }
                    }

                    my @found;
                    ### find every package that satisfies this dependency
                    for my $maybe (@$index) {
                        ### not the right package
                        next unless dependency_satisfied_by( $k, $maybe );
    
                        ### not a good enough version
                        next unless eval "$maybe->{version} $op $ver";
                      
                        push @found, $maybe;
                    }

                    ### nothing satisfied this depenendency
                    unless( @found ) {
                        keys %$entry;   # reset keys!!!!!
                        die "No package satisfies '$k'\n" 
                    }

                    ### XXX should be policy based
                    my @sorted = 
                            sort { $b->{version} <=> $a->{version } } @found;

                    ### store the best match
                    push @maybe, $sorted[0];
                }
    
            } else {
                die "Illegal token: $deps\n";
            }
        }
        return @maybe;
    }

    sub _pp_depends {
        my $deps = shift;
        
        my $str = '';       
        return $str unless $deps;

        for my $entry ( @$deps ) {
            $str .= ' AND ';
    
            if( ! ref $entry ) {
                $str .= "$entry";
        
            } elsif ( UNIVERSAL::isa( $entry, 'ARRAY' ) ) {
                $str .= '(';
                $str .= join " OR ", map { _pp_depends( [$_] ) } @$entry;
                $str .= ')';
                
            } elsif ( UNIVERSAL::isa( $entry, 'REF' ) ) {
                $str .= _pp_depends( $$entry );
            } elsif ( UNIVERSAL::isa( $entry, 'HASH' ) ) {
                while (my($k,$v) = each %$entry) {
                    my ($ver,$op) = reverse split /\s+/, $v;
                    $op ||= '>=';
                    $str .= "$k $op $ver";
                }
            } else {
                die "Illegal token: $entry\n";
            }
        }
        
        $str =~ s/^ AND //;
        
        return $str;
    }
        
     
    ### takes a string with a wanted package and an optional available list
    ### and check if the package is available/provided by any installed pkg
    sub dependency_satisfied {
        my $wanted  = shift or die "Wanted package required";
        my @list    = @{ shift || [] };
        @list       = LoadFile( $protoconf::Available ) unless @list;
    
        ### XXX objectify properly
        ### XXX no | dependencies, no versions
        ### defaults to not satisfied, if we got dependencies
        my $satisfied = 0;
       
        ++$satisfied if grep {
                            $wanted eq $_
                        } map { 
                            ref $_->{provides} 
                                ? @ { $_->{provides} } 
                                : $_->{provides}
                                ;
                        } @list;
                        
        return $satisfied;
    }  

    ### takes a wanted string and a meta object, and checks if the meta object
    ### provides the wanted string
    sub dependency_satisfied_by {
        my($wanted, $meta) = @_ or die "Wanted & Meta required";
        
        return scalar grep { 
                        $wanted eq $_ 
                    } ref $meta->{provides} 
                        ? @ { $meta->{provides} } 
                        : $meta->{provides} || ();
    }      
}
