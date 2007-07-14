
use v6-alpha;

=begin

This visitor desugars OO into Object Meta Model calls.

=end

class KindaPerl6::Visitor::MetaClass {

    method visit ( $node, $node_name ) {
        if    ( $node_name eq 'CompUnit' )
        {
            my $module := [ ];

            # calls GLOBAL::import
            #push @$module, ::Apply(
            #    code      => ::Var( 'sigil' => '&', 'twigil' => '', 'name' => 'import' ),
            #    arguments => [ 
            #        ::Val::Buf( buf => $node.name ),
            #    ],
            #);
            
            # role or class/grammar/module ?
            if $node.unit_type eq 'role' {
                push @$module, ::Call(
                    'hyper'     => '',
                    'arguments' => [
                        ::Val::Buf( buf => $node.name ),  
                    ],
                    'method'   => 'new',
                    'invocant' => ::Val::Buf( buf => 'KindaPerl6::Role' ),  
                );
            }
            else {
                push @$module, ::Call(
                    'hyper'     => '',
                    'arguments' => [
                        ::Val::Buf( buf => $node.name ),  
                    ],
                    'method'   => 'new',
                    'invocant' => ::Val::Buf( buf => 'Class' ),  
                );
            };

            for @($node.traits) -> $trait {
                if $trait[0] eq 'does' {
                    # Bar->HOW->add_role('bar');
                    push @$module, ::Call(
                        'hyper'     => '',
                        'arguments' => [
                            ::Val::Buf( buf => $trait[1] ), 
                        ],
                        'method'    => 'add_role',
                        'invocant'  => ::Call(
                            'hyper'     => '',
                            'arguments' => [ ],
                            'method'    => 'HOW',
                            'invocant'  => ::Val::Buf(
                                    buf => $node.name
                                )
                        ),
                    ); 
                }
                else {
                if $trait[0] eq 'is' {
                    # Bar->HOW->add_parent('bar');
                    push @$module, ::Call(
                        'hyper'     => '',
                        'arguments' => [
                            ::Val::Buf( buf => $trait[1] ), 
                        ],
                        'method'    => 'add_parent',
                        'invocant'  => ::Call(
                            'hyper'     => '',
                            'arguments' => [ ],
                            'method'    => 'HOW',
                            'invocant'  => ::Val::Buf(
                                    buf => $node.name
                                )
                        ),
                    ); 
                }
                else {
                    die "unknown class trait: ", $trait[0];
                };
                };
            };
                        
            for @(($node.body).body) -> $item {

                # METHOD
                if   $item.isa( 'Method' )
                {
                    # Bar->HOW->add_method('bar' => Method->new( 'method' => sub { 789 } ) );
                    push @$module, ::Call(
                        'hyper'     => '',
                        'arguments' => [
                            ::Val::Buf( buf => $item.name ), 
                            
                            # create Method
                            ::Call(
                                'hyper'     => '',
                                'arguments' => [
                                    ::Method(
                                        name  => '',
                                        block => $item.block,
                                    ),
                                ],
                                'method'    => 'new',
                                'invocant'  => ::Val::Buf(
                                    buf => 'Method',
                                ),
                            ),

                        ],
                        'method'    => 'add_method',
                        'invocant'  => ::Call(
                            'hyper'     => '',
                            'arguments' => [ ],
                            'method'    => 'HOW',
                            'invocant'  => ::Val::Buf(
                                    buf => $node.name
                                )
                        ),
                    ); 
                };

                # ACCESSOR
                if    ( $item.isa( 'Decl' ) )
                   && ( $item.decl eq 'has' ) 
                {
                    # Bar->HOW->add_attribute($attribute_name, $attribute_meta_object)
                    push @$module, ::Call(
                        'hyper'     => '',
                        'arguments' => [
                                    ::Val::Buf(
                                        buf => ($item.var).name,
                                    )
                                                
#                            ::Call(
#                                'hyper'     => '',
#                                'arguments' => [
#                                    ::Val::Buf(
#                                        buf => ($item.var).name,
#                                    )
#                                ],
#                                'method'    => 'new',
#                                'invocant'  => ::Val::Buf(
#                                    buf => 'Class::MOP::Attribute'
#                                ),
#                            )

                        ],
                        'method'    => 'add_attribute',
                        'invocant'  => ::Call(
                            'hyper'     => '',
                            'arguments' => [ ],
                            'method'    => 'HOW',
                            'invocant'  => ::Val::Buf(
                                    buf => $node.name
                                )
                        ),
                    ); 
                };
                
                # Class variables
                # TODO

            };
            
            # Everything else
            for @(($node.body).body) -> $item {
                if    $item.isa( 'Method' )
                  ||  (  ( $item.isa( 'Decl' ) )
                      && ( $item.decl eq 'has' ) 
                      )
                { }
                else
                {
                    push @$module, $item;
                }
            };
            
            return ::CompUnit( 
                unit_type => 'module',
                name => $node.name, 
                body => ::Lit::Code(
                    pad   => ($node.body).pad,
                    state => { },
                    sig   => ::Sig( 'invocant' => undef, 'positional' => [ ], 'named' => { } ),
                    body  => $module,
                ),
            );
            
        };
        return;
    };

}
