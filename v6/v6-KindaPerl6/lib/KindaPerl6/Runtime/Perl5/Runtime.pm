
use v5;
use strict 'vars';

use Data::Dumper;
use KindaPerl6::Runtime::Perl5::Match;
use KindaPerl6::Runtime::Perl5::MOP;
#use KindaPerl6::Runtime::Perl5::Type;
use KindaPerl6::Runtime::Perl5::Pad;
use KindaPerl6::Runtime::Perl5::Wrap;
package KindaPerl6::Runtime::Perl5::Runtime;
    #sub import {
    #    my ($package,) = caller();
    #    *{$package.'::dispatch'} = \&KindaPerl6::Runtime::Perl5::MOP::dispatch;
    #}
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
    
package GLOBAL;

    #require Exporter;
    use Data::Dumper;
    use Carp 'confess';
    #@ISA = qw(Exporter);
    our @EXPORT = qw( 
        print 
        say
        undef
        undefine
        defined
        true
        not
        sleep
        True
        False
        substr

        ternary_58__60__63__63__32__33__33__62_
        
        infix_58__60_eq_62_
        infix_58__60_ne_62_
        infix_58__60__61__61__62_
        infix_58__60__33__61__62_
        infix_58__60__38__38__62_
        infix_58__60__124__124__62_
        infix_58__60__126__62_
        infix_58__60__42__62_
        infix_58__60__43__62_
        infix_58__60__45__62_
        infix_58__60__47__62_
        
        prefix_58__60__64__62_        
        prefix_58__60__43__43__62_
    );
    
    # %*ENV
    $GLOBAL::Hash_ENV = bless \%ENV, 'Type_Perl5_Buf_Hash';
    
    ${"GLOBAL::Code_$_"} = \&{"GLOBAL::$_"} for @EXPORT;
    $GLOBAL::Code_import = ::DISPATCH( $::Code, 'new', 
        { code => \&{"GLOBAL::import"}, src => '&GLOBAL::import' } );

    sub init_global {
        #print "Init GLOBAL\n";
        for ( @EXPORT ) {
            #print "Init \$GLOBAL::Code_$_ \n";
            ${"GLOBAL::Code_$_"} = ::DISPATCH( $::Code, 'new', 
                { code => ${"GLOBAL::Code_$_"}, src => '&GLOBAL::'.$_ } ); 
        }
    }

    # XXX - obsolete - GLOBAL is looked up at compile-time
    sub import {
        #print "@_\n";
        my $pkg = _str( $_[0] ); 
        #print "IMPORT $pkg\n";
        ${"${pkg}::Code_$_"} = ::DISPATCH( $::Code, 'new', 
            { code => ${"GLOBAL::Code_$_"}, src => '&GLOBAL::'.$_ } ) 
            for @EXPORT;
    }

    sub _str {
        my $v = $_[0];
        eval { $v = ::DISPATCH( $v, 'str' )->{_value} } if ref($v); 
        print $@ if $@;
        $v;
    }
    sub _int {
        my $v = $_[0];
        eval { $v = ::DISPATCH( $v, 'int' )->{_value} } if ref($v); 
        print $@ if $@;
        $v;
    }

    sub print { 
        #print "print: ", Dumper(\@_);
        print join( '',  map { 
            _str($_); 
        } @_ );
        #print $@ if $@;
    }

    my $undef = ::DISPATCH( $::Undef, 'new', 0 );
    sub undef    { $undef }
    sub undefine { ::DISPATCH( $_[0], 'STORE', $undef ) }
    sub defined  { 
        #print "DEFINED? \n";
        return ::DISPATCH( $::Bit, 'new',0 )
            unless defined $_[0];
        ::DISPATCH( $_[0], 'defined' ) 
    } 
    sub true     { ::DISPATCH( $_[0], 'true' ) }  
    sub not      { ::DISPATCH( $::Bit, 'new', ! ( ::DISPATCH( $_[0], 'true' )->{_value} ) ) }  
    sub True     { ::DISPATCH( $::Bit, 'new',1 ) }  
    sub say      { GLOBAL::print( @_, "\n" );return True;}
    sub sleep    { CORE::sleep(_int($_[0]));return True;}
    sub False    { ::DISPATCH( $::Bit, 'new',0 ) }  
    sub TODO     {confess("TODO");}

    # TODO - macro
    #  ternary:<?? !!>
    sub ternary_58__60__63__63__32__33__33__62_ { 
        #print "ternary: ",caller(2), " $#_ $_[0], $_[1]\n";
        #print ::DISPATCH( $_[0], 'true' );
        ::DISPATCH( $_[0], 'true' )->{_value} ? $_[1] : $_[2] 
    }
    
    # TODO - macro
    sub infix_58__60__38__38__62_   { true($_[0]) && true($_[1]) && $_[1] }
    # TODO - macro
    sub infix_58__60__124__124__62_ { ::DISPATCH( $_[0], 'true' )->{_value} && $_[0] || ::DISPATCH( $_[1], 'true' )->{_value} && $_[1] }

    sub infix_58__60_eq_62_         
    { ::DISPATCH( $::Bit, 'new', (_str($_[0]) eq _str($_[1])) ? 1 : 0) }  # infix:<eq>
    sub infix_58__60_ne_62_         
    { ::DISPATCH( $::Bit, 'new', (_str($_[0]) ne _str($_[1])) ? 1 : 0) }  # infix:<ne>
    sub infix_58__60__61__61__62_   
    { ::DISPATCH( $::Bit, 'new', (_int($_[0]) == _int($_[1])) ? 1 : 0) }  # infix:<==>

    sub infix_58__60__33__61__62_ {  # infix:<!=>
        $::Bit->new(_int($_[0]) != _int($_[1]));
    }

    sub infix_58__60__126__62_      
    { ::DISPATCH( $::Str, 'new', _str( $_[0] ) . _str( $_[1] ) ) }  # infix:<~>
    sub infix_58__60__42__62_       
    { ::DISPATCH( $::Int, 'new', _int( $_[0] ) * _int( $_[1] ) ) }  # infix:<*>
    sub infix_58__60__43__62_       
    { ::DISPATCH( $::Int, 'new', _int( $_[0] ) + _int( $_[1] ) ) }  # infix:<+>
    sub infix_58__60__45__62_       
    { ::DISPATCH( $::Int, 'new', _int( $_[0] ) - _int( $_[1] ) ) }  # infix:<->

    sub infix_58__60__47__62_       
    { ::DISPATCH( $::Int, 'new', _int( $_[0] ) / _int( $_[1] ) ) }  # infix:</>   XXX - Num

    sub substr      
    { 
        #print " substr() parameters: ",
        #    join( ", " ,
        #        _str( $_[0] ), _int( $_[1] ), _int( $_[2] ), _str( $_[3] ) 
        #    )
        #;
        if ( $#_ == 1 ) {
            return ::DISPATCH( $::Str, 'new', 
                substr( 
                    _str( $_[0] ), _int( $_[1] ) 
                )
            ) 
        }
        if ( $#_ == 2 ) {
            return ::DISPATCH( $::Str, 'new', 
                substr( 
                    _str( $_[0] ), _int( $_[1] ), _int( $_[2] ) 
                )
            ) 
        }
        if ( $#_ == 3 ) {
            return ::DISPATCH( $::Str, 'new', 
                substr( 
                    _str( $_[0] ), _int( $_[1] ), _int( $_[2] ), _str( $_[3] ) 
                )
            ) 
        }
        die "Not enough arguments for substr";
    }  

    # prefix:<@>
    sub prefix_58__60__64__62_      { 
        ::DISPATCH( $_[0], 'array' )
    }       

    # prefix:<++>
    sub prefix_58__60__43__43__62_ {
        my $counter = $_[0];
        $counter->{_dispatch_VAR}(
            $counter, 'STORE', ::DISPATCH( $::Int, 'new', _int($counter) + 1)
        );
    }

package Main;
    
    sub chars { length( $_[0] ) }
    sub newline { "\n" }
    sub quote   { '"' }
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
        $s =~ s/\'/\\\'/g;
        $s;
    }
    
    sub Dump {
        require Data::Dump::Streamer;
        Data::Dump::Streamer::Dump( @_ );
    }


{
    GLOBAL::init_global;
}

1;

__END__

=pod

=head1 NAME 

KindaPerl6::Perl5::Runtime

=head1 DESCRIPTION

Provides runtime routines for the KindaPerl6-in-Perl5 compiled code

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 COPYRIGHT

Copyright 2007 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
