use v6-alpha;
class GlobalScope is Scope {

    has $.vars;

    method outer {
        return undef;
    };

    method inner {
        my $inner = Scope.new( outer => self, vars => {} );
        return $inner;
    };

    method hash { self };  # Scope behaves like Hash

    method LOOKUP ( $key ) {
        if exists( (self.vars){$key} ) {
            return (self.vars){$key};
        };
        if exists( (self.vars){'GLOBAL::'~$key} ) {
            return (self.vars){'GLOBAL::'~$key};
        };
        return undef;
    };

    method exists ( $key ) {
        if exists((self.vars){$key}) {
            return True;
        };
        if exists( (self.vars){'GLOBAL::'~$key} ) {
            return True;
        };
        return False;
    };

}

=begin

=head1 NAME

KindaPerl6::Runtime::Perl6::GlobalScope - Global+Lexical scope emulation

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
