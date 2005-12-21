package JIB::Dependency;

use strict;
use warnings;

use Params::Check   qw[check];

use JIB::Constants;
use JIB::Config;

use base 'Object::Accessor';

=head2 $obj = JIB::Dependency->new_from_struct( struct => STRUCT )

=cut

sub new_from_struct {
    my $class = shift;
    my %hash  = @_;
    
    my $struct;
    my $tmpl = {
        struct => { required => 1, store => \$struct },
    };
    
    check( $tmpl, \%hash ) or error( Params::Check->last_error ), return;

    my $obj = $class->SUPER::new;
    $obj->mk_accessors( keys %$tmpl );
    
    $obj->struct( $struct );

    return $obj;
}

=head2 $str = $self->pretty_print

Pretty prints the dependency declaration from the META.info.
It will turn an entry like this:

depends:
  - p5-a
  -
    - p5-b: 3
    - p5-b: < 2
  - !perl/ref:
    =:
      - p5-c
      -
        - p5-e
        - p5-d

Into:

 p5-a AND (p5-b >= 3 OR p5-b < 2) AND p5-c AND (p5-e OR p5-d)

=cut

### XXX combine with the dependency checker?
sub pretty_print {
    my $self    = shift;
    my $struct  = $self->struct;
    
    return $self->_pp_from_struct( $struct );
}

sub _pp_from_struct {
    my $self    = shift;
    my $struct  = shift;
    my $str     = '';
    
    return $str unless $struct;

    for my $entry ( @$struct ) {
        $str .= ' AND ';

        if( ! ref $entry ) {
            $str .= "$entry";
    
        } elsif ( UNIVERSAL::isa( $entry, 'ARRAY' ) ) {
            $str .= '(';
            $str .= join " OR ", map { $self->_pp_from_struct( [$_] ) } @$entry;
            $str .= ')';
            
        } elsif ( UNIVERSAL::isa( $entry, 'REF' ) ) {
            $str .= $self->_pp_from_struct( $$entry );
        } elsif ( UNIVERSAL::isa( $entry, 'HASH' ) ) {
            while (my($k,$v) = each %$entry) {
                my ($ver,$op) = reverse split /\s+/, $v;
                $op ||= '>=';
                $str .= "$k $op $ver";
            }
        } else {
            error("Illegal token: $entry\n"), return;
        }
    }
    
    $str =~ s/^ AND //;
    
    return $str;
}    

1;

__END__

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


# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
