package Pugs::Emitter::Perl6::Perl5::Perl5Array;

# Compile-time Perl 5 array object - hardcoded, autoboxed  methods

use strict;
use warnings;

sub other_get {
    package Pugs::Emitter::Perl6::Perl5;
    use Data::Dumper;
    print Dumper( $_[1] );
    _emit( $_[1] );
}

sub new {
    my $self = $_[1];  # { name => '%hash5' }
    bless $self, $_[0];
    return $self;
}

sub name {
    $_[0]->{name}
}

sub ref { 
    return "'Array'";  # hardcoded 
}

sub isa { 
    my $self = $_[0];
    return $self->other_get( $_[1] ) . ' eq ' . "'Array'";  # hardcoded 
}

sub get {
    my $self = $_[0];
    return $self->name;
}

sub set {
    my $self = $_[0];
    return $self->name . ' = ' . $self->other_get( $_[1] );
}

sub str {
    # TODO
}

sub perl {
    # TODO
}
    
sub defined {
    # TODO
}

sub kv {
    # TODO
}

sub elems {
    'scalar ' . $_[0]->name;
}

1;

__END__



    if ( $n->{op1}{op} eq '=' ) {
        # print "{'='}: ", Dumper( $n );
        if ( exists $n->{exp1}{scalar} ) {
            #print "set $n->{exp1}{scalar}";
            return _var_set( $n->{exp1}{scalar} )->( _var_get( $n->{exp2} ) );
        }
        if ( exists $n->{exp1}{hash} ) {
            my $exp2 = $n->{exp2};
            $exp2 = $exp2->{exp1}
                if     exists $exp2->{'fixity'} 
                    && $exp2->{'fixity'} eq 'circumfix'
                    && $exp2->{'op1'}{'op'} eq '(';
            #print "{'='}: set hash ",Dumper($exp2);
            # Note - the AST is changed in-place here
            if ( exists $exp2->{'list'} ) {
                $exp2->{'list'} = [
                    map {
                        exists ( $_->{pair} ) 
                        ?   ( $_->{pair}{key},
                              $_->{pair}{value}
                            )
                        : $_
                    }
                    @{ $exp2->{'list'} }
                ];
            }
            return _emit( $n->{exp1} ) . 
                " = " . emit_parenthesis( $exp2 );
        }

sub postcircumfix {
    my $n = $_[0];
    #warn "postcircumfix: ", Dumper( $n );
    
    if ( $n->{op1}{op} eq '(' &&
         $n->{op2}{op} eq ')' ) {
        # warn "postcircumfix:<( )> ", Dumper( $n );
        # $.scalar(@param)
        return " " . _emit( $n->{exp1} ) . 
            '->' . emit_parenthesis( $n->{exp2} )
            if exists $n->{exp1}{scalar} &&
               $n->{exp1}{scalar} =~ /^\$\./;
    }
            
    if ( $n->{op1}{op} eq '<' &&
         $n->{op2}{op} eq '>' ) {
        my $name = _emit( $n->{exp1} );
        #$name =~ s/^\%/\$/;

        # $/<x>
        return " " . _emit( $n->{exp1} ) . 
            '->{ ' . _emit_angle_quoted( $n->{exp2}{angle_quoted} ) . ' }'
            if exists $n->{exp1}{scalar};

        # looks like a hash slice
        $name =~ s/^(?: \% | \$ ) / \@ /x;

        return $name . '{ ' . _emit_angle_quoted( $n->{exp2}{angle_quoted} ) . ' }';
    }

    if ( $n->{op1}{op} eq '{' &&
         $n->{op2}{op} eq '}' ) {
        my $name = _emit( $n->{exp1} );

        # $/{'x'}
        return " " . _emit( $n->{exp1} ) . 
            '->{' . _emit( $n->{exp2}{statements}[0] ) . '}'
            if exists $n->{exp1}{scalar};

        # die "trying to emit ${name}{exp}" unless $name =~ m/^\%/;
        #print "postcircumfix{} ",Dumper( $n->{exp2}{statements} );
        if (  exists $n->{exp2}{statements}[0]{list}
           )
        {
            # looks like a hash slice
            $name =~ s/^(?: \% | \$ ) / \@ /x;
        }
        else {
            $name =~ s/^\%/\$/;
        }
        return $name . 
            '{ ' . 
            join('}{', 
                map { 
                    _emit($_) 
                } @{$n->{exp2}{statements}} ) . 
            ' }';
    }

    return _not_implemented( $n, "postcircumfix" );
}


sub variable_declarator {
    my $n = $_[0];
    if ( $n->{'variable_declarator'} eq 'my' ||
         $n->{'variable_declarator'} eq 'our' ) {
        #die "not implemented 'attribute'",Dumper $n
        #    if @{$n->{attribute}};
        if  (  ref $n->{exp1}
            && exists $n->{exp1}{term}
            ) {
            $n->{exp1}{my} = $n->{'variable_declarator'};
            return _emit( $n->{exp1} );
        }
        return $n->{'variable_declarator'} . ' ' . _emit( $n->{exp1} );
    }

    if ( $n->{'variable_declarator'} eq 'constant' ) {
        my $name;
        for (qw( scalar hash array )) {
            $name = $n->{exp1}{$_} if exists $n->{exp1}{$_}
        }
        $name = _emit( $n->{exp1} ) unless $name;
        my $no_sigil = substr( $name, 1 );
        $_V6_ENV{$name}{get} = $_V6_ENV{$name}{set} = $no_sigil;
        return "use constant $no_sigil ";  # TODO - set initial value
    }

    if ( $n->{'variable_declarator'} eq 'state' ) {
        $id++;
        #print "State: $id $name ", Dumper( $n->{exp1} );
        my $name;
        for (qw( scalar hash array )) {
            $name = $n->{exp1}{$_} if exists $n->{exp1}{$_}
        }
        $name = _emit( $n->{exp1} ) unless $name;
        my $sigil = substr( $name, 0, 1 );
        $_V6_ENV{$name}{get} = $_V6_ENV{$name}{set} = 
            $sigil . '{$_V6_STATE{'.$id.'}}';
        return _emit( $n->{exp1} );
    }
    if ( $n->{'variable_declarator'} eq 'has' ) {
            # Moose: has 'xxx';
            # has $x;
            #warn "has: ",Dumper $n;
            
            my $name = _emit( $n->{exp1} );
            #my $name = _emit( $n->{exp1} );
            $name =~ s/^\$//;  # remove sigil
            
            my $raw_name;
            $raw_name = $n->{exp1}{scalar} if exists $n->{exp1}{scalar};
            $_V6_ENV{$raw_name}{set} = sub { 
                "\$_V6_SELF->" . substr($raw_name,2) . "(" . $_[0] . ")" 
            };
            # is rw?
            #warn Dumper @{$n->{attribute}};
            my $is_rw = grep { $_->[0]{bareword} eq 'is' &&
                               $_->[1]{bareword} eq 'rw' } @{$n->{attribute}};
            if ( $is_rw ) {
                $_V6_ENV{$raw_name}{set} = sub { 
                    "\$_V6_SELF->{'" . substr($raw_name,2) . "'} = " . $_[0] 
                }
            }
        
            my $attr = join( ', ', 
                map { 
                    join( ' => ', map { "'" . _emit($_) . "'" } @$_ )
                } @{$n->{attribute}}
            );

            return $n->{'variable_declarator'} . " '" . substr($raw_name,2) . "' => ( $attr )";
    }
}

1;
