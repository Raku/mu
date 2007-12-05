
use v5;
use strict;

    $Main::Code_newline = ::DISPATCH( $::Code, 'new',
        { 
            code => sub { 
                    ::DISPATCH( $::Str, 'new', "\n") 
                }, 
            src => '&Main::newline' 
        } );

    $Main::Code_get_compiler_target_runtime = ::DISPATCH( $::Code, 'new',
        { code => sub { ::DISPATCH( $::Str, 'new', 'KindaPerl6::Runtime::Perl5::KP6Runtime') }, src => '&Main::newline' } );

    $Main::Code_V6_COMPILER_NAME = ::DISPATCH( $::Code, 'new', { code => sub { ::DISPATCH( $::Str, 'new', 'Bootstrapped KP6')}});

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
