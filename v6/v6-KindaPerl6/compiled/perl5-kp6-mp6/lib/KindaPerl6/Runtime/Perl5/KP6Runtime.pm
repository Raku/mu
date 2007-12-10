
use v5;
use strict;

    $Main::Code_newline = ::DISPATCH( $::Code, 'new',
        { 
            code => sub { 
                    ::DISPATCH( $::Str, 'new', "\n") 
                }, 
            src => '&Main::newline' 
        } );

    $Main::Code_quote = ::DISPATCH( $::Code, 'new',
        { 
            code => sub { 
                    ::DISPATCH( $::Str, 'new', '"') 
                }, 
            src => '&Main::quote' 
        } );

    $Main::Code_singlequote = ::DISPATCH( $::Code, 'new',
        { 
            code => sub { 
                    ::DISPATCH( $::Str, 'new', "'") 
                }, 
            src => '&Main::singlequote' 
        } );

    $Main::Code_get_compiler_target_runtime = ::DISPATCH( $::Code, 'new',
        { code => sub { ::DISPATCH( $::Str, 'new', 'KindaPerl6::Runtime::Perl5::Runtime') }, src => '&Main::newline' } );

    # this is a string constant
    $Main::_V6_COMPILER_NAME = 
        ::DISPATCH($::Scalar, "new", { modified => {}, name => "$Main::V6_COMPILER_NAME" } );
    ::DISPATCH_VAR( $Main::_V6_COMPILER_NAME, 'STORE',
            ::DISPATCH( $::Str, 'new', 'Bootstrapped KP6')
        );

    $Main::Code_mangle_string = ::DISPATCH( $::Code, 'new',
        { 
            code => sub { 
                    my $s = shift;
                    $s = ::DISPATCH( $s, 'Str' )->{_value};
                    $s =~ s/(\\|\')/\\$1/g;
                    return ::DISPATCH( $::Str, 'new', $s );   
                }, 
            src => '&Main::mangle_string' 
        } );

        my %table = (
            '$' => '',
            '@' => 'List_',
            '%' => 'Hash_',
            '&' => 'Code_',
        );
    $Main::Code_mangle_name = ::DISPATCH( $::Code, 'new',
        { 
            code => sub { 
                    my ($sigil, $twigil, $name, $namespace) = @_;
                    
                    $sigil = ::DISPATCH( $sigil, 'Str' )->{_value};
                    $twigil = ::DISPATCH( $twigil, 'Str' )->{_value};
                    $name = ::DISPATCH( $name, 'Str' )->{_value};
                    $namespace = ::DISPATCH( $namespace, 'INDEX' )->{_value}{_array};
                    
                    $namespace = [ 
                        map { 
                                ::DISPATCH( $_, 'Str' )->{_value}
                            } 
                            @$namespace 
                        ];
                    
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
                    return ::DISPATCH( $::Str, 'new', 
                                '$' . CORE::join( '::', @name )   # XXX - no twigil
                        );   
                }, 
            src => '&Main::mangle_name' 
        } );

    $Main::Code_mangle_perl5rx_metasyntax = ::DISPATCH( $::Code, 'new',
        { 
            code => sub { 
                    my $s = shift;
                    $s = ::DISPATCH( $s, 'Str' )->{_value};
                    if ( $s =~ /\./ ) {
                        $s =~ s/(\.)/::_rule_/;   # '$KindaPerl6::Grammar.ws' -> ::_rule_ws
                    }
                    else {
                        $s = '_rule_' . $s;   # '$_rule_ws'
                    }
                    ::DISPATCH( $::Str, "new", $s );
                }, 
            src => '&Main::mangle_perl5rx_metasyntax' 
        } );

1;

__END__

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
