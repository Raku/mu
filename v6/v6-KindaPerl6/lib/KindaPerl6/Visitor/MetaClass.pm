
use v6-alpha;

=begin

This visitor desugars OO into Object Meta Model calls.

=end

class KindaPerl6::Visitor::MetaClass {

    method visit ( $node, $node_name ) {
        if    ( $node_name eq 'CompUnit' )
        {

#         Class::MOP::Class->create('Bar' => (
#             version      => '0.01',
#             superclasses => [ 'Foo' ],
#             attributes => [
#                 Class::MOP:::Attribute->new('$bar'),
#                 Class::MOP:::Attribute->new('$baz'),
#             ],
#             methods => {
#                 calculate_bar => sub { ... },
#                 construct_baz => sub { ... }
#             }
#         ));

            my $module := [ ];
            push @$module, ::Call(
                'hyper'     => '',
                'arguments' => [
                    ::Val::Buf( buf => $node.name ),
                ],
                'method'   => 'create',
                'invocant' => ::Val::Buf(
                        buf => 'Class::MOP::Class'
                ),
            );

            for @(($node.body).body) -> $item {

                # METHOD
                if   $item.isa( 'Method' )
                {
                    # Bar->meta->add_method('bar' => sub { 789 });
                    push @$module, ::Call(
                        'hyper'     => '',
                        'arguments' => [
                            ::Val::Buf( buf => $item.name ),
                            ::Sub(
                                name  => '',
                                sig   => $item.sig,
                                block => $item.block,
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
                    # Bar->meta->add_attribute($attribute_name, $attribute_meta_object)
                    push @$module, ::Call(
                        'hyper'     => '',
                        'arguments' => [
                                                
                            ::Call(
                                'hyper'     => '',
                                'arguments' => [
                                    ::Val::Buf(
                                        buf => ($item.var).name,
                                    )
                                ],
                                'method'    => 'new',
                                'invocant'  => ::Val::Buf(
                                    buf => 'Class::MOP:::Attribute'
                                ),
                            )

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
            
            return ::Module( 
                name => $node.name, 
                body => ::Lit::Code(
                    pad   => { },
                    state => { },
                    sig   => ::Sig( 'invocant' => undef, 'positional' => [ ], 'named' => { } ),
                    body  => $module,
                ),
            );
            
        };
        return;
    };

}
