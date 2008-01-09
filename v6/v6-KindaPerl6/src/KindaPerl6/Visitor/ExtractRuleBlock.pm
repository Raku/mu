
use v6-alpha;

class KindaPerl6::Visitor::ExtractRuleBlock {

    my $count;
    method visit ( $node, $node_name, $path ) {

        #say "Global ",$node_name;

        if    ( $node_name eq 'Rule::Block' )
        {
            use Data::Dumper;
            my $comp_unit := $path[0-1];
            $count := $count + 1;
            my $name := '__rule_block' ~ $count ~ '_' ~ $COMPILER::source_md5;
            push @(($comp_unit.body).body), Method.new(block=> Lit::Code.new(
                    body => ($node.closure).body,
                    sig => Sig.new(
                        invocant   => '',
                        positional => [
                            Lit::SigArgument.new(
                                key           => Var.new(
                                    namespace   => [],
                                    name        => 'MATCH',
                                    twigil      => '',
                                    sigil       => '$',
                                ),
                                value         => undef,
                                type          => '',
                                is_multidimensional => Val::Bit.new( bit => '0', ),
                                is_slurpy     => Val::Bit.new( bit => '0', ),
                                is_optional   => Val::Bit.new( bit => '0', ),
                                is_named_only => Val::Bit.new( bit => '0', ),
                                is_copy       => Val::Bit.new( bit => '0', ),
                                is_rw         => Val::Bit.new( bit => '0', ),
                                has_default   => Val::Bit.new( bit => '0', ),
                            ),
                        ],
                    ),
                    pad => Pad.new(
                        lexicals => [
                            Decl.new(
                                decl => 'my',
                                var  => Var.new(
                                    namespace => [],
                                    name      => '_',
                                    twigil    => '',
                                    sigil     => '@',
                                ),
                                type => '',
                            ),
                            Decl.new(
                                decl => 'my',
                                var  => Var.new(
                                    namespace => [],
                                    name      => 'MATCH',
                                    twigil    => '',
                                    sigil     => '$',
                                ),
                                type => '',
                            )
                        ],
                    ),
                    state => {},
                  ), name=>$name);
            push @(($node.closure).body), Return.new(result=> Val::Buf.new(buf=>'sTrNgE V4l'));
            $node.closure($name);
            return $node;
        };
        0;
    };

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
