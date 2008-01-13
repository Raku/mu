
use v6-alpha;

=begin

This visitor desugars OO into Object Meta Model calls.

=end

class KindaPerl6::Visitor::MetaClass {

    method visit ( $node, $node_name ) {
        if    ( $node_name eq 'CompUnit')
        {
            if ($node.unit_type eq 'module') {
                return;
            }
            my $module := [ ];

            # calls GLOBAL::import
            #push @$module, Apply.new(
            #    code      => Var.new( 'sigil' => '&', 'twigil' => '', 'name' => 'import' ),
            #    arguments => [
            #        Val::Buf.new( buf => $node.name ),
            #    ],
            #);

            # role or class/grammar?
            if $node.unit_type eq 'role' {
                push @$module, Call.new(
                    'hyper'     => '',
                    'arguments' => [
                        Val::Buf.new( buf => $node.name ),
                    ],
                    'method'   => 'new',
                    'invocant' => Proto.new( name => 'KindaPerl6::Role' ),
                );
            }
            else {
                my $metaclass := 'Class';
                my $trait;
                if ($node.traits) {
                    for @($node.traits) -> $trait {
                        if $trait[0] eq 'meta' {
                            $metaclass := $trait[1];
                        }
                    };
                };
                my $metaobject := Call.new(
                    'hyper'     => '',
                    'arguments' => [
                        Val::Buf.new( buf => $node.name ),
                    ],
                    'method'   => 'new',
                    'invocant' => Proto.new( name => $metaclass ),
                );
                # 'If' == avoid redefining the prototype object
                my $body := $node.body;
                my $pad;
                if ($body) {
                    $pad := $body.pad;
                }
                push @$module,
                    If.new(
                        cond      =>
                           Apply.new(
                                arguments => [ Proto.new( name => $node.name ) ],
                                code => Var.new( name => 'VAR_defined', twigil => '', sigil => '&', namespace => [ ] ),
                            ),
                        body      =>
                            Lit::Code.new(
                                body => [ ],
                                sig   =>
                                  Sig.new( invocant => '', positional => [], ),
                                pad   => $pad,
                                state => { },
                            ),
                        otherwise =>
                            Lit::Code.new(
                                body => [
                                    Bind.new(
                                        'parameters' => Proto.new( name => $node.name ),
                                        'arguments'  => Call.new(
                                            'invocant' => $metaobject,
                                            'method'   => 'PROTOTYPE',
                                            'hyper'    => '',
                                        ),
                                    )
                                ],
                                sig   =>
                                  Sig.new( invocant => '', positional => [], ),
                                pad   => $pad,
                                state => { },
                            ),
                    );
            };
            my $trait;
            if ($node.traits) {
            for @($node.traits) -> $trait {
                if $trait[0] eq 'does' {
                    # Bar->HOW->add_role('bar');
                    push @$module, Call.new(
                        'hyper'     => '',
                        'arguments' => [
                            Val::Buf.new( buf => $trait[1] ),
                        ],
                        'method'    => 'add_role',
                        'invocant'  => Call.new(
                            'hyper'     => '',
                            'arguments' => [ ],
                            'method'    => 'HOW',
                            'invocant'  => Proto.new(
                                    name => $node.name
                                )
                        ),
                    );
                }
                else {
                if $trait[0] eq 'is' {
                    # Bar->HOW->add_parent('bar');
                    push @$module, Call.new(
                        'hyper'     => '',
                        'arguments' => [
                            Call.new(
                                'hyper'     => '',
                                'arguments' => [ ],
                                'method'    => 'HOW',
                                'invocant'  => Proto.new(
                                    name => $trait[1]
                                )
                            ),
                        ],
                        'method'    => 'add_parent',
                        'invocant'  => Call.new(
                            'hyper'     => '',
                            'arguments' => [ ],
                            'method'    => 'HOW',
                            'invocant'  => Proto.new(
                                   name => $node.name
                                )
                        ),
                    );
                }
                else {
                    if $trait[0] eq 'meta' {
                        # handled above
                    } else {
                        die "unknown class trait: ", $trait[0];
                    }
                };
                };
            };
            };
            my $item;
            if ($node) {
            if ($node.body) {
            if (($node.body).body) {
            for @(($node.body).body) -> $item {

                # METHOD
                if   $item.isa( 'Method' )
                {
                    # Bar->HOW->add_method('bar' => Method->new( 'method' => sub { 789 } ) );
                    push @$module, Call.new(
                        'hyper'     => '',
                        'arguments' => [
                            Val::Buf.new( buf => $item.name ),

                            $item,
                            # create Method
                            # Call.new(
                            #    'hyper'     => '',
                            #    'arguments' => [
                            #        Method.new(
                            #            name  => '',
                            #            block => $item.block,
                            #        ),
                            #    ],
                            #    'method'    => 'new',
                            #    'invocant'  => Proto.new(
                            #        name => 'Method',
                            #    ),
                            # ),

                        ],
                        'method'    => 'add_method',
                        'invocant'  => Call.new(
                            'hyper'     => '',
                            'arguments' => [ ],
                            'method'    => 'HOW',
                            'invocant'  => Proto.new(
                                    name => $node.name
                                )
                        ),
                    );
                };

                # ACCESSOR
                if    ( $item.isa( 'Decl' ) )
                   && ( $item.decl eq 'has' )
                {
                    # Bar->HOW->add_attribute($attribute_name, $attribute_meta_object)
                    push @$module, Call.new(
                        'hyper'     => '',
                        'arguments' => [
                                    Val::Buf.new(
                                        buf => ($item.var).name,
                                    )

#                            Call.new(
#                                'hyper'     => '',
#                                'arguments' => [
#                                    Val::Buf.new(
#                                        buf => ($item.var).name,
#                                    )
#                                ],
#                                'method'    => 'new',
#                                'invocant'  => Proto.new(
#                                    name => 'Class::MOP::Attribute'
#                                ),
#                            )

                        ],
                        'method'    => 'add_attribute',
                        'invocant'  => Call.new(
                            'hyper'     => '',
                            'arguments' => [ ],
                            'method'    => 'HOW',
                            'invocant'  => Proto.new(
                                    name => $node.name
                                )
                        ),
                    );
                };

                # Class variables
                # TODO

            };
            };};};
            # Everything else
            my $item;
            if ($node) {
            if ($node.body) {
            if (($node.body).body) {
            for @(($node.body).body) -> $item {
                if    $item.isa( 'Method' )
                  ||  (  ( $item.isa( 'Decl' ) )
                      && ( $item.decl eq 'has' )
                      )
                { }
                else
                {
                    if (!$module) {
                        $module := [ ];
                    }
                    push @$module, $item;
                }
            };
            };
            };
            my $body := $node.body;
            my $pad;
            if ($body) {
                $pad := $body.pad;
            };
            return CompUnit.new(
                unit_type => 'module',
                name => $node.name,
                body => Lit::Code.new(
                    pad   => $pad,
                    state => { },
                    sig   => Sig.new( 'invocant' => undef, 'positional' => [ ] ),
                    body  => $module,
                ),
            );
            };

        };
        return;
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
