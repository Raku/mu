# MP6 Runtime

$INC{'MiniPerl6/Perl5/Runtime.pm'} = 1;
$INC{'MiniPerl6/Perl5/Match.pm'} = 1;

use strict;
use warnings;


package MiniPerl6::Perl5::Match;

    no warnings 'recursion';
    use Data::Dumper;
    use Scalar::Util qw( refaddr blessed );

    my %_data;

    use overload (
        '@{}'    => \&array,
        '%{}'    => \&hash,
        'bool'   => sub { $_data{refaddr $_[0]}{bool} },
        '${}'    => \&scalar,
        '""'     => \&flat,
        '0+'     => \&flat,
        fallback => 1,
    );

    sub new {
        my $class = shift;
        my $obj = bless \$class, $class;
        $_data{ refaddr $obj } = { @_ };
        return $obj;
    }

    sub DESTROY {  
        delete $_data{ refaddr $_[0] };
    }

    sub data  {    $_data{refaddr $_[0]}           }

    sub from { @_ == 1 ? ( $_data{refaddr $_[0]}{from} ) : ( $_data{refaddr $_[0]}{from} = $_[1] ) };
    sub to   { @_ == 1 ? ( $_data{refaddr $_[0]}{to}   ) : ( $_data{refaddr $_[0]}{to}   = $_[1] ) };
    sub bool { @_ == 1 ? ( $_data{refaddr $_[0]}{bool} ) : ( $_data{refaddr $_[0]}{bool} = $_[1] ) };
    sub capture
             { @_ == 1 ? ( $_data{refaddr $_[0]}{capture} ) : ( $_data{refaddr $_[0]}{capture} = $_[1] ) };

    sub array {    
             $_data{refaddr $_[0]}->{match} 
        || ( $_data{refaddr $_[0]}->{match} = [] )
    }

    sub hash  {   
             $_data{refaddr $_[0]}->{named} 
        || ( $_data{refaddr $_[0]}->{named} = {} )
    }

    sub keys   { 
        CORE::keys   %{$_data{refaddr $_[0]}->{named}},
        0 .. $#{ $_[0]->array }
    }
    sub values { 
        CORE::values %{$_data{refaddr $_[0]}->{named}},
        @{ $_[0]->array }
    }
    sub kv {
        map { ( $_, $_[0]->{$_} ) } 
            $_[0]->keys 
    }
    sub elems  { 
        scalar $_[0]->keys
    }

    sub chars  { CORE::length $_[0]->str }

    sub flat {
        my $obj = $_data{refaddr $_[0]};
        my $cap = $obj->{capture};
        #print ref $cap;
        return $cap
            if defined $cap;
        return '' unless $obj->{bool};
        
        return '' if $_[0]->from > length( $obj->{str} );
        
        return substr( $obj->{str}, $_[0]->from, $_[0]->to - $_[0]->from );
    }

    sub str {
        "" . $_[0]->flat;
    }

    sub perl {
        local $Data::Dumper::Terse    = 1;
        local $Data::Dumper::Sortkeys = 1;
        local $Data::Dumper::Pad = '  ';
        return __PACKAGE__ . "->new( " . Dumper( $_[0]->data ) . ")\n";
    }

    # return the capture
    sub scalar {
        return \( $_[0]->flat );
    }

1;


package KindaPerl6::Grammar;

    no warnings 'uninitialized';

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
    
    sub print { print join( '', @_ ) }
    sub say   { print join( '', @_, "\n" ) }
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
        my ($sigil, $twigil, $name, $namespace) = @_;
        #print "mangle: ($sigil, $twigil, $name, [ @$namespace ] )\n" if $namespace;
        $name = CORE::join( '::', @$namespace, $name ) if $namespace;
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
        return '$' . CORE::join( '::', @name );   # XXX - no twigil
    }
    sub mangle_name_lisp {
        my ($sigil, $twigil, $name, $namespace) = @_;
        #print "mangle: ($sigil, $twigil, $name, [ @$namespace ] )\n" if $namespace;
        $name = CORE::join( '::', @$namespace, $name ) if $namespace;
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
        $name[-1] = 'kp6-' . $name[-1];
        return $name[-1] if @name == 1;
        return 'kp6-' . CORE::join( '::', @name );   # XXX - no twigil
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

    sub get_compiler_target_runtime {
        $ENV{KP6_TARGET_RUNTIME} || "KindaPerl6::Runtime::Perl5::Runtime";
    }

    sub expand_macro {
        # XXX TODO
        #print "# HERE! @_ \n";
        #GLOBAL::say( ::DISPATCH( $_[0], 'WHAT' ) );
    }

1;
