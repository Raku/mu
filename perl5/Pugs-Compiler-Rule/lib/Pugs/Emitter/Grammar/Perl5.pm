package Pugs::Emitter::Grammar::Perl5;

our $VERSION = '0.27';

#use Smart::Comments;
use strict;
use warnings;
use Pugs::Emitter::Rule::Perl5::Ratchet;

# for safe mode
sub _prune_actions {
    my ($ast) = @_;
    while (my ($key, $node) = each %$ast) {
        next if $key =~ /^_/ or !ref $node;
        #warn $key;
        if ($key eq 'closure') {
            #die "Found closures!";
            next if ref $node ne 'HASH';
            my $code = $node->{closure};
            if ($code and !ref $code and $code =~ /\w+/) {
                die "ERROR: code blocks not allowed in safe mode: \"$code\"\n";
            }
        }
        if (ref $node) {
            my $ref = ref $node;
            if ($ref eq 'HASH') {
                _prune_actions($node); 
            } elsif ($ref eq 'ARRAY') {
                for my $child (@$node) {
                    if (ref $child and ref $child eq 'HASH') {
                        _prune_actions($child);
                    }
                }
            }
        }
    }
}

sub emit {
    my $ast = shift;
    my $opts = shift;
    $opts ||= {};
    ## $ast
    my ($name, $stmts) = each %$ast;
    my $p5_methods = '';
    ### $name
    for my $stmt (@$stmts) {
        my $regex = $stmt->();
        my $type = $regex->{type};
        ## $regex
        if ($type eq 'block') {
            my $code = $regex->{value};
            if ($opts->{safe_mode} && $code =~ /\w+/) {
                die "ERROR: verbatim Perl 5 blocks not allowed in safe mode: \"$code\"\n";
            }
            $p5_methods .= <<"_EOC_";
# Code block from grammar spec
$code

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

        my $ast = $regex->{ast};
        if ($opts->{safe_mode}) {
            _prune_actions($ast);
        }

        if ($type eq 'regex') {
            $body = Pugs::Emitter::Rule::Perl5::emit(
                'Pugs::Grammar::Rule',
                $ast,
            )
        } else {
            $body = Pugs::Emitter::Rule::Perl5::Ratchet::emit(
                'Pugs::Grammar::Rule',
                $ast,
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
    # bootstrap the regex parser itself:
    my $prefix = $name eq 'Pugs::Grammar::Rule' ?
        "#" : '';
    return <<"_EOC_";
package $name;

${prefix}use base 'Pugs::Grammar::Base';

use Pugs::Runtime::Match;
use Pugs::Runtime::Regex;
use Pugs::Runtime::Tracer ();

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

Copyright (c) 2007 by Agent Zhang (E<lt>agentzh@agentzh.orgE<gt>) and others.

This program is free software; you can redistribute it
and/or modify it under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=head1 SEE ALSO

L<Pugs::Compiler::Grammar>, L<compile_p6grammar.pl>.

