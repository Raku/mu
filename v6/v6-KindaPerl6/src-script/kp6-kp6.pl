module Main {
    use KindaPerl6::Runtime::Perl5::KP6Runtime;
    use KindaPerl6::Grammar;
    use KindaPerl6::Traverse;
    use KindaPerl6::Ast;
    use KindaPerl6::Grammar::Regex;
    use KindaPerl6::Runtime::Perl6::Compiler;
    use KindaPerl6::Runtime::Perl6::Grammar;

    use KindaPerl6::Visitor::ExtractRuleBlock;
    use KindaPerl6::Visitor::Token;
    use KindaPerl6::Visitor::MetaClass;
    use KindaPerl6::Visitor::Global;
    use KindaPerl6::Visitor::Emit::Perl5;
    use KindaPerl6::Visitor::Emit::AstPerl;

    my @visitors;
    @visitors.push(KindaPerl6::Visitor::ExtractRuleBlock.new());
    @visitors.push(KindaPerl6::Visitor::Token.new());
    @visitors.push(KindaPerl6::Visitor::MetaClass.new());
    @visitors.push(KindaPerl6::Visitor::Global.new())
    my $emit_p5 = KindaPerl6::Visitor::Emit::Perl5.new();
    $emit_p5.visitor_args = { secure => 1 };
    @visitors.push($emit_p5);

    my $code = slurp;

    COMPILER::env_init();

    # this should be used importing
    # Digest::MD5::md5_hex from perl5;
    $COMPILER::source_md5 = 'temporary_value';

    my $pos = 0;
    my $len = length $code;

    while ($len > $pos) {

        my $match = KindaPerl6::Grammar.comp_unit($code, $pos);
        my $ast = $match.result;
        if (!($ast.isa('CompUnit'))) {
            die 'AST IS:(' ~ $ast.result ~ ')';
        };

        say $ast.emit(KindaPerl6::Visitor::Emit::AstPerl.new());
        exit;

        my $res;
        for @visitors -> $visitor {
            $res = $ast.emit($visitor);
        };
        print $res;
        $pos = $pos + $match.to;
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
