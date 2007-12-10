
# running kp6-kp6.pl:
#
#  $ KP6_TARGET=KP6-BOOT perl Makefile.PL ; make
#  $ perl -I compiled/perl5-kp6-kp6/lib/ script/kp6-kp6.pl <<< 123

module Main {
    use KindaPerl6::Runtime::Perl5::KP6Runtime;
    use KindaPerl6::Grammar;
    use KindaPerl6::Traverse;
    use KindaPerl6::Ast;
    use KindaPerl6::Grammar::Regex;
    #use KindaPerl6::Runtime::Perl6::Compiler;
    use KindaPerl6::Runtime::Perl6::Grammar;

    use KindaPerl6::Visitor::ExtractRuleBlock;
    use KindaPerl6::Visitor::Token;
    use KindaPerl6::Visitor::MetaClass;
    use KindaPerl6::Visitor::Global;
    use KindaPerl6::Visitor::Emit::Perl5;
    use KindaPerl6::Visitor::Emit::Perl5Regex;
    use KindaPerl6::Visitor::Emit::AstPerl;

    my $emit_p5 = KindaPerl6::Visitor::Emit::Perl5.new();
    $emit_p5.visitor_args = { secure => 1 };

    # "kp6-boot" uses: MetaClass Global Emit::Perl5Regex
    my @visitors;
    @visitors.push(KindaPerl6::Visitor::MetaClass.new());
    @visitors.push(KindaPerl6::Visitor::Global.new())
    @visitors.push(KindaPerl6::Visitor::Emit::Perl5Regex.new())

    my $code = slurp;

    COMPILER::env_init();

    # this should be used importing
    # Digest::MD5::md5_hex from perl5;
    $COMPILER::source_md5 = 'temporary_value';

    $_ = $code;
    my $match = KindaPerl6::Grammar.parse();
    my $parsed = $match.result;
    for $parsed.values -> $ast {
        
        say $ast.perl;  # emit(KindaPerl6::Visitor::Emit::AstPerl.new());
        #say "running visitors";

        my $res = $ast;
        for @visitors -> $visitor {
            #say "Visitor: $visitor";
            $res = $res.emit($visitor);
            #say "Result: $res";
        };
        print $res;
        
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
