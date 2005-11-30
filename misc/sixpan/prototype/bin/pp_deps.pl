use strict;
use YAML            qw[LoadFile Dump];
use File::Basename  qw[basename];
use Data::Dumper;
use Cwd;

BEGIN {
    use lib         qw[bin];
    require '_inc.pl';
    protoconf->import();
}

@ARGV or die 'Need at least one package';

my %indexed = map { $_->{package} => $_ } LoadFile( $Repoindex ); 

for my $pkg (@ARGV ) {
    
    ### in case there's more than one package matching,
    ### sort by highest version
    ### XXX this should be a policy decision
    my @metas = sort { $b->{version} cmp $a->{version} }
                map  { $_->[1] } 
                grep {  package_prefix( $_->[1]->{package} ) . '-' .
                        package_name( $_->[1]->{package} ) eq $pkg } 
                map  { [ $_, $indexed{$_} ] } 
            keys %indexed;

    if( scalar @metas > 1 ) {
        warn "More than one result found, using highest versioned\n";
    }
    
    my $found = $metas[0];

    my @to_install = _recurse_resolve( list_dependencies( $found, 1 ) );

    print "Installing:\n";
    for my $obj (@to_install) {
        print "\t$obj->{package}\n";       
    }
    
}

sub _recurse_resolve {
    my @deps = @_;

    ### we need at least the deps provided, plus whatever /they/ need
    my @rv = @deps;
    for my $dep ( @deps ) {
        ### unshift, as they are prereqs for THIS dependency, so they
        ### must be installed first
        unshift @rv, _recurse_resolve( list_dependencies( $dep, 1 ) );
    }
    
    return @rv;
}    

__END__





    ### XXX can be custom file & nicer object & error checking
    my $struct  = LoadFile( "$srcdir/$Pms/$Metafile" ) 
                        or die "Could not read $Metafile";

    my $deps    = $struct->{depends};
    
    print "Translating:\n";
    print Dump( $deps );
    print "\n\nTo:\n";
    print join " AND ", map { pp_deps( $_ ) } @$deps;
    print $/ . $/;
}

sub pp_deps {
    my $deps = shift;
    
    my $str = '';
    return $str unless $deps;

    if( ! ref $deps ) {
        $str .= "$deps";

    } elsif ( UNIVERSAL::isa( $deps, 'ARRAY' ) ) {
        $str .= join " OR ", map { pp_deps( $_ ) } @$deps;
        
    } elsif ( UNIVERSAL::isa( $deps, 'REF' ) ) {
        $str .= '(' . pp_deps( $$deps ) . ')';
    } elsif ( UNIVERSAL::isa( $deps, 'HASH' ) ) {
        while (my($k,$v) = each %$deps) {
            my ($ver,$op) = reverse split /\s+/, $v;
            $op ||= '>=';
            $str .= "$k $op $ver";
        }
    } else {
        die "Illegal token: $deps\n";
    }
    
    return $str;
}    
