use v6-alpha;
class Scope is Value {

    has $.vars;
    has $.outer;  # static or dynamic scope

    method inner {
        my $inner = Scope.new( outer => self, vars => {} );
        #$inner.outer = self;
        #$inner.vars  = {};
        return $inner;
    };

    method hash { self };  # Scope behaves like Hash

    method LOOKUP ( $key ) {
        #say "# lookup key $key in ", (self.vars).perl;
        if exists( (self.vars){$key} ) {
            #say "# found key";
            return (self.vars){$key};
        };
        #say "# not found in current pad";
        if defined self.outer {
            return (self.outer).LOOKUP( $key );
        };
        return undef;
    };

    method exists ( $key ) {
        if exists((self.vars){$key}) {
            return (self.vars){$key};
        };
        if defined( self.outer ) {
            return (self.outer).exists( $key );
        };
        return False;
    };

    method create ( $key ) {
        #say "# create key $key in ", (self.vars).perl;
        if exists( (self.vars){$key} ) {
            return (self.vars){$key};
        };
        (self.vars){$key} = undef;
    };

    # TODO !!!

    method perl {
        my $s = '{ ';
        for self.pairs -> $pair {
            $s = $s ~ ($pair.key).perl ~ ' => ' ~ ($pair.value).perl ~ ', ';
        };
        return $s ~ ' }'
    };
    method Str {
        ( ( self.pairs ).map( -> $pair { $pair.key ~ "\t" ~ $pair.value}) ).join( "\n" );
    };
    method keys {
        my $pairs = self.pairs;
        $pairs.map( -> $pair {$pair.key});
    };
    method values {
        my $pairs = self.pairs;
        $pairs.map( -> $pair {$pair.value});
    };
    method true { self.elems != 0 };
    method Int  { self.elems };
}

=begin

=head1 NAME

KindaPerl6::Runtime::Perl6::Scope - Lexical scope emulation

=head1 DESCRIPTION

This changes the AST to use lexical scope emulation for languages that
do not have such niceties natively.

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2007 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end
