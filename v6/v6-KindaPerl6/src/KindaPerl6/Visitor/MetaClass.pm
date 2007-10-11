
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
                    'invocant' => ::Proto( name => 'KindaPerl6::Role' ),  
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
                my $metaobject := ::Call(
                    'hyper'     => '',
                    'arguments' => [
                        ::Val::Buf( buf => $node.name ),  
                    ],
                    'method'   => 'new',
                    'invocant' => ::Proto( name => $metaclass ),  
                );
                # 'If' == avoid redefining the prototype object
                my $body := $node.body;
                my $pad;
                if ($body) {
                    $pad := $body.pad;
                }
                push @$module, 
                    ::If(
                        cond      => 
                           ::Apply(
                                arguments => [ ::Proto( name => $node.name ) ],
                                code => ::Var( name => 'VAR_defined', twigil => '', sigil => '&', namespace => [ ] ),
                            ),
                        body      => '',
                        otherwise => 
                            ::Lit::Code(
                                body => [ 
                                    ::Bind(
                                        'parameters' => ::Proto( name => $node.name ),  
                                        'arguments'  => ::Call(
                                            'invocant' => $metaobject,
                                            'method'   => 'PROTOTYPE',
                                            'hyper'    => '',
                                        ),
                                    )
                                ],
                                sig   =>
                                  ::Sig( named => {}, invocant => '', positional => [], ),
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
                            'invocant'  => ::Proto(
                                    name => $node.name
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
                            ::Call(
                                'hyper'     => '',
                                'arguments' => [ ],
                                'method'    => 'HOW',
                                'invocant'  => ::Proto(
                                    name => $trait[1]
                                )
                            ),
                        ],
                        'method'    => 'add_parent',
                        'invocant'  => ::Call(
                            'hyper'     => '',
                            'arguments' => [ ],
                            'method'    => 'HOW',
                            'invocant'  => ::Proto(
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
                    push @$module, ::Call(
                        'hyper'     => '',
                        'arguments' => [
                            ::Val::Buf( buf => $item.name ), 
                            
                            $item,
                            # create Method
                            # ::Call(
                            #    'hyper'     => '',
                            #    'arguments' => [
                            #        ::Method(
                            #            name  => '',
                            #            block => $item.block,
                            #        ),
                            #    ],
                            #    'method'    => 'new',
                            #    'invocant'  => ::Proto(
                            #        name => 'Method',
                            #    ),
                            # ),

                        ],
                        'method'    => 'add_method',
                        'invocant'  => ::Call(
                            'hyper'     => '',
                            'arguments' => [ ],
                            'method'    => 'HOW',
                            'invocant'  => ::Proto(
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
#                                'invocant'  => ::Proto(
#                                    name => 'Class::MOP::Attribute'
#                                ),
#                            )

                        ],
                        'method'    => 'add_attribute',
                        'invocant'  => ::Call(
                            'hyper'     => '',
                            'arguments' => [ ],
                            'method'    => 'HOW',
                            'invocant'  => ::Proto(
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
            return ::CompUnit( 
                unit_type => 'module',
                name => $node.name,
                body => ::Lit::Code(
                    pad   => $pad,
                    state => { },
                    sig   => ::Sig( 'invocant' => undef, 'positional' => [ ], 'named' => { } ),
                    body  => $module,
                ),
            );
            };

        };
        return;
    };

}
