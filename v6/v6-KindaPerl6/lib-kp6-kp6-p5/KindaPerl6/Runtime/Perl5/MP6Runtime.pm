# MP6 Runtime
package KindaPerl6::Grammar;
    sub space { 
        my $grammar = $_[0]; my $str = $_[1]; my $pos = $_[2]; 
        my $MATCH; 
        $MATCH = KindaPerl6::Perl5::Match->new( 
            'str' => $str,'from' => $pos,'to' => $pos, ); 
        $MATCH->bool(
            substr($str, $MATCH->to()) =~ m/^([[:space:]])/
            ? ( 1 + $MATCH->to( length( $1 ) + $MATCH->to() ))
            : 0
        );
        $MATCH;
    }
    sub digit { 
        my $grammar = $_[0]; my $str = $_[1]; my $pos = $_[2]; 
        my $MATCH; $MATCH = KindaPerl6::Perl5::Match->new( 
            'str' => $str,'from' => $pos,'to' => $pos, ); 
        $MATCH->bool(
            substr($str, $MATCH->to()) =~ m/^([[:digit:]])/
            ? ( 1 + $MATCH->to( length( $1 ) + $MATCH->to() ))
            : 0
        );
        $MATCH;
    }

    sub word { 
            my $grammar = $_[0]; my $str = $_[1]; my $pos = $_[2]; 
            my $MATCH; $MATCH = KindaPerl6::Perl5::Match->new( 
                'str' => $str,'from' => $pos,'to' => $pos, ); 
            $MATCH->bool(
                substr($str, $MATCH->to()) =~ m/^([[:word:]])/
                ? ( 1 + $MATCH->to( length( $1 ) + $MATCH->to() ))
                : 0
            );
            $MATCH;
    }
    sub backslash { 
            my $grammar = $_[0]; my $str = $_[1]; my $pos = $_[2]; 
            my $MATCH; $MATCH = KindaPerl6::Perl5::Match->new( 
                'str' => $str,'from' => $pos,'to' => $pos, ); 
            $MATCH->bool(
                substr($str, $MATCH->to(), 1) eq '\\'         # '
                ? ( 1 + $MATCH->to( 1 + $MATCH->to() ))
                : 0
            );
            $MATCH;
    }
        
    sub newline { 
        my $grammar = $_[0]; my $str = $_[1]; my $pos = $_[2]; 
        my $MATCH; $MATCH = KindaPerl6::Perl5::Match->new( 
            'str' => $str,'from' => $pos,'to' => $pos, ); 
        return $MATCH unless ord( substr($str, $MATCH->to()) ) == 10
            || ord( substr($str, $MATCH->to()) ) == 13;
        $MATCH->bool(
            substr($str, $MATCH->to()) =~ m/(?m)^(\n\r?|\r\n?)/
            ? ( 1 + $MATCH->to( length( $1 ) + $MATCH->to() ))
            : 0
        );
        $MATCH;
    }
    sub not_newline { 
        my $grammar = $_[0]; my $str = $_[1]; my $pos = $_[2]; 
        my $MATCH; $MATCH = KindaPerl6::Perl5::Match->new( 
            'str' => $str,'from' => $pos,'to' => $pos, 'bool' => 0 ); 
        return $MATCH if ord( substr($str, $MATCH->to()) ) == 10
            || ord( substr($str, $MATCH->to()) ) == 13;
        $MATCH->to( 1 + $MATCH->to );
        $MATCH->bool( 1 );
        $MATCH;
    }

1;

package Main;
    
    sub chars { length( $_[0] ) }
    sub newline { "\n" }
    sub quote   { '"' }
    sub singlequote { "'" }
    sub backslash { "\\" }
    sub isa { 
        my $ref = ref($_[0]);
           (  $ref eq 'ARRAY' 
           && $_[1] eq 'Array'
           )
        || (  $ref eq 'HASH' 
           && $_[1] eq 'Hash'
           )
        || (  $ref eq '' 
           && $_[1] eq 'Str'
           )
        || $ref eq $_[1]
        || (  ref( $_[1] ) 
           && $ref eq ref( $_[1] ) 
           )
    }

    sub perl {
        local $Data::Dumper::Terse    = 1;
        my $can = UNIVERSAL::can($_[0] => 'perl');
        if ($can) {
            $can->($_[0]);
        }
        else {
            Data::Dumper::Dumper($_[0]);
            #require Data::Dump::Streamer;
            #Data::Dump::Streamer($_[0]);
        }
    }
    
    sub yaml {
        my $can = UNIVERSAL::can($_[0] => 'yaml');
        if ($can) {
            $can->($_[0]);
        }
        else {
            require YAML::Syck;
            YAML::Syck::Dump($_[0]);
        }
    }
      
    sub join {
        my $can = UNIVERSAL::can($_[0] => 'join');
        if ($can) {
            $can->(@_);
        }
        else {
            join($_[1], @{$_[0]} );
        }
    }
    
        my %table = (
            '$' => '',
            '@' => 'List_',
            '%' => 'Hash_',
            '&' => 'Code_',
        );
    sub mangle_name {
        my ($sigil, $twigil, $name) = @_;
        #print "mangle: ($sigil, $twigil, $name)\n";
        $name =~ s/ ([^a-zA-Z0-9_:] | (?<!:):(?!:)) / '_'.ord($1).'_' /xge;
        my @name = split( /::/, $name );
        $name[-1] = $table{$sigil} . $name[-1];
        #print "name: @name \n";
        if  (  $twigil eq '*'
            && @name   == 1
            )
        {
            unshift @name, 'GLOBAL';
        }
        return '$' . join( '::', @name );   # XXX - no twigil
    }
    sub mangle_ident {
        my ($name) = @_;
        $name =~ s/ ([^a-zA-Z0-9_]) / '_'.ord($1).'_' /xge;
        return $name;
    }
    sub mangle_string {
        my $s = shift;
        $s =~ s/(\\|\')/\\$1/g;
        $s;
    }
    
    sub Dump {
        require Data::Dump::Streamer;
        Data::Dump::Streamer::Dump( @_ );
    }

1;
