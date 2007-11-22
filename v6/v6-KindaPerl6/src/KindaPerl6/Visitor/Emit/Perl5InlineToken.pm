use v6-alpha;

# This visitor is a plugin for the perl5 emitter.
# It emits p6-regex as c code.


class KindaPerl6::Visitor::Emit::Perl5InlineToken {

    # 'Emit::Perl5' predefines all nodes, except 'Token'
    use KindaPerl6::Visitor::Emit::Perl5;
    use KindaPerl6::Visitor::Emit::TokenC;

    method visit ( $node ) {
        $node.emit_perl5_and_c;
    };
}

class CompUnit {
    method emit_perl5_and_c {
        '{package ' ~ $.name ~ ';use Inline C => Config,STRUCTS=>["match"];use Inline C => `cat "lib/KindaPerl6/Runtime/C/match.h"`.<<\'END\';' ~ Main::newline() ~ self.emit_c ~ Main::newline() ~ 'END' ~ Main::newline() ~ '}' ~ self.emit_perl5;
    }
}
class Token {
    method emit_perl5 {
        '::DISPATCH(::DISPATCH($::' ~ $KindaPerl6::Visitor::Emit::Perl5::current_compunit ~ ',"HOW"),"add_method",::DISPATCH($::Str,"new",\''~$.name~
        '\'),::DISPATCH($::Method,"new",sub {my $match = '~ Main::mangle_ident($KindaPerl6::Visitor::Emit::Perl5::current_compunit~'::'~$.name) ~ '(GLOBAL::_str($_[1]),GLOBAL::_int($_[2]));::DISPATCH($::Match,"new",{match_str=>$_[1],bool=>::DISPATCH($::Bit,"new",$match->boolean),from=>::DISPATCH($::Int,"new",$match->from),to=>::DISPATCH($::Int,"new",$match->to)})}))';
    }
}


=begin

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
