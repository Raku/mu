package Pugs::Emitter::Grammar::Perl5;

our $VERSION = '0.25';

#use Smart::Comments;
use strict;
use warnings;
use Pugs::Emitter::Rule::Perl5::Ratchet;

sub emit {
    my $ast = shift;
    ## $ast
    my ($name, $stmts) = each %$ast;
    my $p5_methods = '';
    ### $name
    for my $stmt (@$stmts) {
        my $regex = $stmt->();
        my $type = $regex->{type};
        ## $regex
        if ($type eq 'block') {
            $p5_methods .= <<"_EOC_";
# Code block from grammar spec
$regex->{value}

_EOC_
            next;
        }
        ### struct: $regex->{name}
        ## regex AST: $regex->{ast}
        my $params = {};
        if ($type eq 'rule') {
            $params->{sigspace} = 1;
        }
        my $body;
        if ($type eq 'regex') {
            $body = Pugs::Emitter::Rule::Perl5::emit(
                'Pugs::Grammar::Rule',
                $regex->{ast},
            )
        } else {
            $body = Pugs::Emitter::Rule::Perl5::Ratchet::emit(
                'Pugs::Grammar::Rule',
                $regex->{ast},
                $params,
            );
        }
        $body =~ s/^/    /gm;
        $p5_methods .= <<_EOC_;
# $regex->{type} $regex->{name}
*$regex->{name} =
$body;

_EOC_
    }
    return <<"_EOC_";
package $name;

use base 'Pugs::Grammar::Base';
use Pugs::Runtime::Match;
use Pugs::Runtime::Regex;

$p5_methods

1;
_EOC_
}

1;
__END__

=head1 NAME

Pugs::Emitter::Grammar::Perl5 - Perl 5 emitter for grammar ASTs

=head1 SYNOPSIS

    use Pugs::Compiler::Grammar;
    use Pugs::Emitter::Grammar::Perl5;

    my $ast = Pugs::Grammar::Rule->grammar(q{

        grammar MyLang;

        token def {
            <type> <?ws> <var_list> <?ws>? ';'
        }

        token type { int | float | double | char }

        token var_list { <ident> <?ws>? [ ',' <?ws>? <ident> ]* }

    })->();
    my $perl5 = Pugs::Emitter::Grammar::Perl5::emit($ast);
    print $perl5;

=head1 FUNCTIONS

=over

=item C<< $perl5 = Pugs::Emitter::Grammar::Perl5::emit($ast) >>

Generate Perl 5 source code from the grammar AST returned
by L<Pugs::Grammar::Rule>'s grammar parser.

=back

=head1 AUTHOR

The Pugs contributors E<lt>perl6-compiler@perl.orgE<gt>.

=head1 COPYRIGHT

Copyright (c) 2007 by Agent Zhang and others.

This program is free software; you can redistribute it
and/or modify it under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=head1 SEE ALSO

L<Pugs::Compiler::Grammar>, L<compile_p6grammar.pl>.

