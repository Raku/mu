
use v6-alpha;

grammar MiniPerl6::Grammar {

our $Class_name;

token comp_unit {
    <?opt_ws> [\; <?opt_ws> | <''> ]
    [ <'use'> <?ws> <'v6-'> <ident> <?opt_ws> \; <?ws>  |  <''> ]
    
    [ <'class'> | <'grammar'> ]  <?opt_ws> <full_ident> <?opt_ws> 
    <'{'>
        { $Class_name := ~$<full_ident> }
        <?opt_ws>
        <exp_stmts>
        <?opt_ws>
    <'}'>
    <?opt_ws> [\; <?opt_ws> | <''> ]

    {
        my $name := $$<full_ident>;
        my @body := $$<exp_stmts>;
        #say 'CompUnit was compiled into: ', ($$<exp_stmts>).perl;
        
        my $a := @body;
        my $item;

        my $methods;
        for @$a -> $item {
            if   $item.isa( 'Method' )
            {
                push @$methods, ::Lit::Object( 
                    'fields' => [
                        [
                        ::Val::Buf( 'buf' => 'superclasses' ),
                        ::Lit::Array( array => [] )
                        ],
                        [
                        ::Val::Buf( 'buf' => 'name' ),
                        ::Val::Buf( 'buf' => $item.name ),
                        ],
                        [
                        ::Val::Buf( 'buf' => 'definition' ),
                        $item
                        ],
                     ],
                     'class' => 'MO::Compile::Method::Simple'
                );
            };
        };

        my $attributes;
        for @$a -> $item {
            if    ( $item.isa( 'Decl' ) )
               && ( $item.decl eq 'has' ) 
            {
                push @$attributes, ::Lit::Object( 
                     'fields' => [
                        [
                        ::Val::Buf( 'buf' => 'name' ),
                        ::Val::Buf( 'buf' => ($item.var).name ),
                        ],
                     ],
                     'class' => 'MO::Compile::Attribute::Simple'
                );
            };
        };

        # Class variables
        for @$a -> $item {
            if    ( $item.isa( 'Decl' ) )
               && (  ( $item.decl eq 'our' )
                  || ( $item.decl eq 'my'  )
                  ) 
               && ( ($item.var).twigil eq '.' )
            {
                push @$methods, ::Lit::Object( 
                    'fields' => [
                        [
                        ::Val::Buf( 'buf' => 'superclasses' ),
                        ::Lit::Array( array => [] )
                        ],
                        [
                        ::Val::Buf( 'buf' => 'name' ),
                        ::Val::Buf( 'buf' => ($item.var).name ),
                        ],
                        [
                        ::Val::Buf( 'buf' => 'definition' ),
                        ::Method(
                                'sig' => ::Sig( 
                                        'named'      => { },
                                        'invocant'   => 
                                            ::Var( 'name' => 'self', 'twigil' => '', 'sigil' => '$' ),
                                        'positional' => [ ]
                                ),
                                'name' => '',
                                'block' => [
                                    # if @_[0] { $var := @_[0] } else { $var } 
                                    ::If(
                                      'cond' => ::Index(
                                                    'obj' => 
                                                        ::Var( 'name' => '_', 'twigil' => '', 'sigil' => '@' ),
                                                    'index' => ::Val::Int( 'int' => '0' )
                                                 ),
                                      'body' => [
                                            ::Bind(
                                                'parameters' => 
                                                  ::Var( 'name' => '_DOT_' ~ ($item.var).name, 'twigil' => '', 'sigil' => '$' ),
                                                'arguments' => ::Index(
                                                    'obj' => 
                                                        ::Var( 'name' => '_', 'twigil' => '', 'sigil' => '@' ),
                                                    'index' => ::Val::Int( 'int' => '0' )
                                                 )
                                            )
                                      ],
                                      'otherwise' => [ 
                                         
                                          ::Var( 'name' => '_DOT_' ~ ($item.var).name, 'twigil' => '', 'sigil' => '$' )
                                      ]
                                    )
                                ],
                        ),
                        ],
                     ],
                     'class' => 'MO::Compile::Method::Simple'
                );
            };
        };

        # Create the "new" method
        push @$methods, ::Lit::Object( 
                    'fields' => [
                        [
                        ::Val::Buf( 'buf' => 'superclasses' ),
                        ::Lit::Array( array => [] )
                        ],
                        [
                        ::Val::Buf( 'buf' => 'name' ),
                        ::Val::Buf( 'buf' => 'new' ),
                        ],
                        [
                        ::Val::Buf( 'buf' => 'definition' ),
                        ::Method(
                                'sig' => ::Sig( 
                                        'named'      => { },
                                        'invocant'   => 
                                            ::Var( 'name' => 'self', 'twigil' => '', 'sigil' => '$' ),
                                        'positional' => [ ]
                                ),
                                'name' => '',
                                'block' => [
                                  # $__class_box.create_instance( array @_ )
                                  ::Call(
                                    'hyper'     => '',
                                    'arguments' => [
                                        ::Apply(
                                                'arguments' => [
                                                    ::Var(
                                                            'name'   => '_',
                                                            'twigil' => '',
                                                            'sigil'  => '@'
                                                    )
                                                ],
                                                'code' => 'array'
                                        )
                                    ],
                                    'method'   => 'create_instance',
                                    'invocant' => ::Var(
                                            'name'   => '__class_box',
                                            'twigil' => '',
                                            'sigil'  => '$'
                                    )
                                  )
                                ],
                            ),
                        ],
                     ],
                     'class' => 'MO::Compile::Method::Simple'
        );

        my $module;
        for @$a -> $item {
            if    ( $item.isa( 'Decl' ) )
               && (  ( $item.decl eq 'our' )
                  || ( $item.decl eq 'my'  )
                  ) 
               && ( ($item.var).twigil eq '.' )
            {
                push @$module, ::Decl( 
                        'decl' => $item.decl,
                        'var'  => ::Var( 'name' => '_DOT_' ~ ($item.var).name, 'twigil' => '', 'sigil' => '$' ),
                        'type' => $item.type
                )
            }
            else {
            if    (  ( $item.isa( 'Decl' ) )
                      && ( $item.decl eq 'has' ) 
                  )
               || ( $item.isa( 'Method' ) )
            {
                # done
            }
            else
            {
                push @$module, $item;
            };
            };
        };

        push @$module, ::Use( mod => 'MO::Compile::Method::Simple' );
        push @$module, ::Use( mod => 'MO::Compile::Class::MI' );
        push @$module, ::Use( mod => 'MO::Compile::Attribute::Simple' );
        push @$module, ::Decl( 
                        'decl' => 'my',
                        'var'  => ::Var( 'name' => '__class_box', 'twigil' => '', 'sigil' => '$' ),
                        'type' => ''
                );
        push @$module, ::Bind( 
                'parameters' => ::Decl( 
                        'decl' => 'my',
                        'var'  => ::Var( 'name' => '__base', 'twigil' => '', 'sigil' => '$' ),
                        'type' => ''
                ),
                'arguments' => ::Lit::Object( 
                        'fields' => [
                                [
                                ::Val::Buf( 'buf' => 'instance_methods' ),
                                ::Lit::Array( array => $methods )
                                ],
                                [
                                ::Val::Buf( 'buf' => 'attributes' ),
                                ::Lit::Array( array => $attributes )
                                ]
                        ],
                     'class' => 'MO::Compile::Class::MI'
                ),
        );
        push @$module, ::Apply(
                'arguments' => [
                  ::Val::Buf( 'buf' => $name ),
                  ::Var( 'name' => '__base', 'twigil' => '', 'sigil' => '$' ),
                ],
                'code' => 'Main::register_class'
        );        
        push @$module, ::Bind(        
                parameters => ::Var(
                        'name'   => '__class_box',
                        'twigil' => '',
                        'sigil'  => '$'
                ),
                arguments => ::Apply(
                   'arguments' => [
                         ::Var(
                                'name'   => '__base',
                                'twigil' => '',
                                'sigil'  => '$'
                          ),
                   ],
                   'code' => 'Main::box_class'
                )
        );

        return ::Module( name => $name, body => $module );
    }
};

}

