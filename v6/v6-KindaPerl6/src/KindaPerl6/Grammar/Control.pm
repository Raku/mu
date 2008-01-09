
use v6-alpha;

grammar KindaPerl6::Grammar {


token control {
    | <ctrl_return> { make $$<ctrl_return> }   # make 123;
    | <ctrl_leave>  { make $$<ctrl_leave>  }   # last; break;
    | <if>     { make $$<if>     }   # 1 ?? 2 !! 3
    | <unless> { make $$<unless> }   # !1 ?? 2 !! 3
    | <when>   { make $$<when>   }   # when 3 { ... }
    | <for>    { make $$<for>    }   # $x.map(-> $i {...})
    | <while>  { make $$<while>  }   # while ... { ... }
    | <apply>  { make $$<apply>  }   # $obj($arg1, $arg2)
 #  | <call>   { make $$<call>   }   # $obj.method($arg1, $arg2)
};

token block1 {
    \{  <.opt_ws>
        {
            COMPILER::add_pad();
        }
        <exp_stmts>
        <.opt_ws>
    \}
        {
            my $env := COMPILER::current_pad();
            COMPILER::drop_pad();
            make Lit::Code.new(
                    pad   => $env,
                    state => { },
                    sig   => Sig.new( 'invocant' => undef, 'positional' => [ ] ),
                    body  => $$<exp_stmts>,
                );
        }
};

token block2 {
    <block1>
    { make $$<block1> }
};

token if {
    if <.ws>  <exp>  <.opt_ws>
    <block1>
    [
        <.opt_ws>
        else <.opt_ws>
        <block2>
        {
            make If.new(
                'cond'      => $$<exp>,
                'body'      => $$<block1>,
                'otherwise' => $$<block2>,
            );
        }
    |
        {
            make If.new(
                'cond' => $$<exp>,
                'body' => $$<block1>,
                'otherwise' => undef,
             )
        }
    ]
};

token unless {
    unless <.ws>  <exp>  <.opt_ws>
    <block1>
    [
        <.opt_ws>
        else <.opt_ws>
        <block2>
        {
            make If.new(
                'cond'      => $$<exp>,
                'body'      => $$<block2>,
                'otherwise' => $$<block1>,
            );
        }
    |
        {
            make If.new(
                'cond' => $$<exp>,
                'body' => undef,
                'otherwise' => $$<block1>,
             )
        }
    ]
};

token when {
    when <.ws> <exp_seq> <.opt_ws> <block1>
    {
        make When.new(
            'parameters' => $$<exp_seq>,
            'body'       => $$<block1>,
            ) }
};

token for {
    for <.ws> <exp> <.opt_ws> <arrow_sub>
    {
            make Call.new(
                hyper     => '',
                arguments => [ $$<arrow_sub> ],
                method   => 'map',
                invocant => $$<exp>,
            );
    }
};

token while {
    while <.ws> <exp> <.ws> <block1>
    {
        make While.new(
            'cond' => $$<exp>,
            'body' => $$<block1>,
            ) }
};

token ctrl_leave {
    leave
    { make Leave.new() }
};

token ctrl_return {
    return <.ws> <exp>
    { make Return.new( 'result' => $$<exp> ) }
    |
    return
    { make Return.new( 'result' => Val::Undef.new() ) }
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
