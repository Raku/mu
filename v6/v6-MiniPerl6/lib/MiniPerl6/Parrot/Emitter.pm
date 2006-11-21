use v6-alpha;

class CompUnit {
    has $.name;
    has %.attributes;
    has %.methods;
    has @.body;
    method emit {
        
        # TODO sub new { shift; bless { @_ }, "' ~ $.name ~ '" }' ~ Main::newline() ~
        
        my $a := @.body;
        my $s :=   
            '.namespace [ "' ~ $.name ~ '" ] ' ~ Main::newline() ~
            '.sub "__onload" :load' ~ Main::newline() ~
            '  .local pmc self' ~ Main::newline() ~
            '  newclass self, "' ~ $.name ~ '"' ~ Main::newline();
        for @$a -> $item {
            if $item.isa( 'Decl' ) {
                $s := $s ~ $item.emit;
            }
        };
        $s := $s ~
            '.end' ~ Main::newline ~ Main::newline();

        for @$a -> $item {
            if $item.isa( 'Decl' ) {
                # already done - ignore
            }
            else {
              if   $item.isa( 'Sub' ) 
                || $item.isa( 'Method' )
              {
                $s := $s ~ $item.emit;
              }
              else {
                $s := $s ~ '.sub _ :anon :load :init' ~ Main::newline() ~
                    $item.emit ~
                    '.end' ~ Main::newline() ~ Main::newline();
              }
            }
        };
        return $s;
    }
}

#  .namespace [ 'Main' ]
#  .sub _ :anon :load :init
#    print "hello"
#  .end


class Val::Int {
    has $.int;
    method emit {
        '  $P0 = new .Integer' ~ Main::newline ~
        '  $P0 = ' ~ $.int ~ Main::newline
    }
}

class Val::Bit {
    has $.bit;
    method emit {
        '  $P0 = new .Integer' ~ Main::newline ~
        '  $P0 = ' ~ $.bit ~ Main::newline
    }
}

class Val::Num {
    has $.num;
    method emit {
        '  $P0 = new .Float' ~ Main::newline ~
        '  $P0 = ' ~ $.num ~ Main::newline
    }
}

class Val::Buf {
    has $.buf;
    method emit {
        '  $P0 = new .String' ~ Main::newline ~
        '  $P0 = \'' ~ $.buf ~ '\'' ~ Main::newline
    }
}

class Val::Undef {
    method emit {
        '  $P0 = new .Undef' ~ Main::newline
    }
}

class Val::Object {
    has $.class;
    has %.fields;
    method emit {
        die 'Val::Object - not used yet';
        'bless(' ~ %.fields.perl ~ ', ' ~ $.class.perl ~ ')';
    }
}

class Lit::Seq {
    has @.seq;
    method emit {
        die 'Lit::Seq - not used yet';
        '(' ~ (@.seq.>>emit).join('') ~ ')';
    }
}

class Lit::Array {
    has @.array;
    method emit {
        my $a := @.array;
        my $s := 
            '  save $P1' ~ Main::newline() ~
            '  $P1 = new .ResizablePMCArray' ~ Main::newline();
        for @$a -> $item {
            $s := $s ~ $item.emit;
            $s := $s ~ 
            '  push $P1, $P0' ~ Main.newline;
        };
        my $s := $s ~ 
            '  $P0 = $P1' ~ Main::newline() ~
            '  restore $P1' ~ Main::newline();
        return $s;
    }
}

class Lit::Hash {
    has @.hash;
    method emit {
        my $a := @.hash;
        my $s := 
            '  save $P1' ~ Main::newline() ~
            '  save $P2' ~ Main::newline() ~
            '  $P1 = new .Hash' ~ Main::newline();
        for @$a -> $item {
            $s := $s ~ ($item[0]).emit;
            $s := $s ~ 
            '  $P2 = $P0' ~ Main.newline;
            $s := $s ~ ($item[1]).emit;
            $s := $s ~ 
            '  set $P1[$P2], $P0' ~ Main.newline;
        };
        my $s := $s ~ 
            '  $P0 = $P1'   ~ Main::newline() ~
            '  restore $P2' ~ Main::newline() ~
            '  restore $P1' ~ Main::newline();
        return $s;
    }
}

class Lit::Code {
    method emit {
        die 'Lit::Code - not used yet';
    }
}

class Lit::Object {
    has $.class;
    has @.fields;
    method emit {
        # ::Type( 'value' => 42 )
        my $fields := @.fields;
        my $str := '';        
        $str := 
            '  save $P1' ~ Main::newline() ~
            '  save $S2' ~ Main::newline() ~
            '  $P1 = new "' ~ $.class ~ '"' ~ Main::newline();
        for @$fields -> $field {
            $str := $str ~ 
                ($field[0]).emit ~ Main::newline() ~
                '  $S2 = $P0'    ~ Main::newline() ~
                ($field[1]).emit ~ Main::newline() ~
                '  setattribute $P1, $S2, $P0' ~ Main::newline();
        };
        $str := $str ~ 
            '  $P0 = $P1'   ~ Main::newline() ~
            '  restore $S2' ~ Main::newline() ~
            '  restore $P1' ~ Main::newline();
        $str;
    }
}

class Index {
    has $.obj;
    has $.index;
    method emit {
        my $s := 
            '  save $P1'  ~ Main::newline();
        $s := $s ~ $.obj.emit;
        $s := $s ~ 
            '  $P1 = $P0' ~ Main.newline();
        $s := $s ~ $.index.emit;
        $s := $s ~ 
            '  $P0 = $P1[$P0]' ~ Main.newline();
        my $s := $s ~ 
            '  restore $P1' ~ Main::newline();
        return $s;
    }
}

class Lookup {
    has $.obj;
    has $.index;
    method emit {
        my $s := 
            '  save $P1'  ~ Main::newline();
        $s := $s ~ $.obj.emit;
        $s := $s ~ 
            '  $P1 = $P0' ~ Main.newline;
        $s := $s ~ $.index.emit;
        $s := $s ~ 
            '  $P0 = $P1[$P0]' ~ Main.newline;
        my $s := $s ~ 
            '  restore $P1' ~ Main::newline();
        return $s;
    }
}

class Var {
    has $.sigil;
    has $.twigil;
    has $.name;
    method emit {
        # Normalize the sigil here into $
        # $x    => $x
        # @x    => $List_x
        # %x    => $Hash_x
        # &x    => $Code_x
        my $table := {
            '$' => '$',
            '@' => '$List_',
            '%' => '$Hash_',
            '&' => '$Code_',
        };
           ( $.twigil eq '.' )
        ?? ( 
             '  getattribute $P0, self, "' ~ $.name ~ '"' ~ Main::newline() 
           )
        !!  (    ( $.name eq '/' )
            ??   ( $table{$.sigil} ~ 'MATCH' )
            !!   ( $table{$.sigil} ~ $.name )
            )
    };
    method name {
        $.name
    };
}

class Bind {
    has $.parameters;
    has $.arguments;
    method emit {
        if $.parameters.isa( 'Lit::Array' ) {

            #  [$a, [$b, $c]] := [1, [2, 3]]

            my $a := $.parameters.array;
            my $b := $.arguments.array;
            my $str := 'do { ';
            my $i := 0;
            for @$a -> $var {
                my $bind := ::Bind( 'parameters' => $var, 'arguments' => ($b[$i]) );
                $str := $str ~ ' ' ~ $bind.emit ~ '; ';
                $i := $i + 1;
            };
            return $str ~ $.parameters.emit ~ ' }';
        };
        if $.parameters.isa( 'Lit::Hash' ) {

            #  {:$a, :$b} := { a => 1, b => [2, 3]}

            # XXX TODO - this is *not* right

            my $a := $.parameters.hash;
            my $b := $.arguments.hash;
            my $str := 'do { ';
            my $i := 0;
            for @$a -> $var {
                my $bind := ::Bind( 'parameters' => $var[0], 'arguments' => ($b[$i])[1] );
                $str := $str ~ ' ' ~ $bind.emit ~ '; ';
                $i := $i + 1;
            };
            return $str ~ $.parameters.emit ~ ' }';
        };
        $.arguments.emit ~
        '  ' ~ $.parameters.emit ~ ' = $P0' ~ Main::newline();
    }
}

class Proto {
    has $.name;
    method emit {
        ~$.name
    }
}

class Call {
    has $.invocant;
    has $.hyper;
    has $.method;
    has @.arguments;
    has $.hyper;
    method emit {
        if     ($.method eq 'perl')
            || ($.method eq 'yaml')
            || ($.method eq 'say' )
            || ($.method eq 'join')
            || ($.method eq 'chars')
            || ($.method eq 'isa')
        {
            if ($.hyper) {
                return
                    '[ map { Main::' ~ $.method ~ '( $_, ' ~ ', ' ~ (@.arguments.>>emit).join('') ~ ')' ~ ' } @{ ' ~ $.invocant.emit ~ ' } ]';
            }
            else {
                return
                    'Main::' ~ $.method ~ '(' ~ $.invocant.emit ~ ', ' ~ (@.arguments.>>emit).join('') ~ ')';
            }
        };

        my $meth := $.method;
        if  $meth eq 'postcircumfix:<( )>'  {
             $meth := '';
        };

        my $call := '->' ~ $meth ~ '(' ~ (@.arguments.>>emit).join('') ~ ')';
        if ($.hyper) {
            '[ map { $_' ~ $call ~ ' } @{ ' ~ $.invocant.emit ~ ' } ]';
        }
        else {
            $.invocant.emit ~ $call;
        };

    }
}

class Apply {
    has $.code;
    has @.arguments;
    method emit {

        my $code := $.code;

        if $code eq 'say'        {
            return
                (@.arguments.>>emit).join( '  print $P0' ~ Main::newline ) ~
                '  print $P0' ~ Main::newline ~
                '  print "\n"' ~ Main::newline
        };
        if $code eq 'print'      {
            return
                (@.arguments.>>emit).join( '  print $P0' ~ Main::newline ) ~
                '  print $P0' ~ Main::newline 
        };
        if $code eq 'array'      { return '@{' ~ (@.arguments.>>emit).join(' ')    ~ '}' };

        if $code eq 'prefix:<~>' { return '("" . ' ~ (@.arguments.>>emit).join(' ') ~ ')' };
        if $code eq 'prefix:<!>' { return '('  ~ (@.arguments.>>emit).join(' ')    ~ ' ? 0 : 1)' };
        if $code eq 'prefix:<?>' { return '('  ~ (@.arguments.>>emit).join(' ')    ~ ' ? 1 : 0)' };

        if $code eq 'prefix:<$>' { return '${' ~ (@.arguments.>>emit).join(' ')    ~ '}' };
        if $code eq 'prefix:<@>' { return '@{' ~ (@.arguments.>>emit).join(' ')    ~ '}' };
        if $code eq 'prefix:<%>' { return '%{' ~ (@.arguments.>>emit).join(' ')    ~ '}' };

        if $code eq 'infix:<~>'  { return '('  ~ (@.arguments.>>emit).join(' . ')  ~ ')' };
        if $code eq 'infix:<+>'  { return (@.arguments.>>emit).join('')  ~ Main::newline() };
        if $code eq 'infix:<->'  { return '('  ~ (@.arguments.>>emit).join(' - ')  ~ ')' };

        if $code eq 'infix:<&&>' { return '('  ~ (@.arguments.>>emit).join(' && ') ~ ')' };
        if $code eq 'infix:<||>' { return '('  ~ (@.arguments.>>emit).join(' || ') ~ ')' };
        if $code eq 'infix:<eq>' { return '('  ~ (@.arguments.>>emit).join(' eq ') ~ ')' };
        if $code eq 'infix:<ne>' { return '('  ~ (@.arguments.>>emit).join(' ne ') ~ ')' };

        if $code eq 'infix:<==>' { return '('  ~ (@.arguments.>>emit).join(' == ') ~ ')' };
        if $code eq 'infix:<!=>' { return '('  ~ (@.arguments.>>emit).join(' != ') ~ ')' };

        if $code eq 'ternary:<?? !!>' { 
            return 
                ( ::If( cond => @.arguments[0],
                        body => [@.arguments[1]],
                        otherwise => [@.arguments[2]] 
                ) ).emit;
        };

        $.code ~ '(' ~ (@.arguments.>>emit).join(', ') ~ ')';
        # '(' ~ $.code.emit ~ ')->(' ~ @.arguments.>>emit.join(', ') ~ ')';
    }
}

class Return {
    has $.result;
    method emit {
        $.result.emit ~ 
        '  .return( $P0 )' ~ Main::newline();
    }
}

class If {
    has $.cond;
    has @.body;
    has @.otherwise;
    my $label := 100;
    method emit {
        $label := $label + 1;
        my $id := $label;
        return
            $.cond.emit ~ 
            '  unless $P0 goto ifelse' ~ $id ~ Main::newline() ~
                (@.body.>>emit).join('') ~ 
            '  goto ifend' ~ $id ~ Main::newline() ~
            'ifelse' ~ $id ~ ':' ~ Main::newline() ~
                (@.otherwise.>>emit).join('') ~ 
            'ifend'  ~ $id ~ ':'  ~ Main::newline();
    }
}

class For {
    has $.cond;
    has @.body;
    has @.topic;
    my $label := 100;
    method emit {
        my $cond := $.cond;
        $label := $label + 1;
        my $id := $label;
        if   $cond.isa( 'Var' )
          && $cond.sigil eq '@'
        {
            $cond := ::Apply( code => 'prefix:<@>', arguments => [ $cond ] );
        };
        '' ~ 
        $cond.emit ~
        '  $P1 = new .Iterator, $P0' ~ Main::newline() ~
        ' test_iter'  ~ $id ~ ':' ~ Main::newline() ~
        '  unless $P1 goto iter_done'  ~ $id ~ Main::newline() ~
        '  $P2 = shift $P1' ~ Main::newline() ~
        (@.body.>>emit).join('') ~
        '  goto test_iter'  ~ $id ~ Main::newline() ~
        ' iter_done'  ~ $id ~ ':' ~ Main::newline() ~
        # TODO - $.topic.emit ~ 
        ''; 
    }
}

class Decl {
    has $.decl;
    has $.type;
    has $.var;
    method emit {
        my $decl := $.decl;
        my $name := $.var.name;
           ( $decl eq 'has' )
        ?? ( '  addattribute self, "' ~ $name ~ '"' ~ Main::newline() )
        !! $.decl ~ ' ' ~ $.type ~ ' ' ~ $.var.emit;
    }
}

class Sig {
    has $.invocant;
    has $.positional;
    has $.named;
    method emit {
        ' print \'Signature - TODO\'; die \'Signature - TODO\'; '
    };
    method invocant {
        $.invocant
    };
    method positional {
        $.positional
    }
}

class Method {
    has $.name;
    has $.sig;
    has @.block;
    method emit {
        # TODO - signature binding
        my $sig := $.sig;
        # say "Sig: ", $sig.perl;
        my $invocant := $sig.invocant;
        # say $invocant.emit;
        my $pos := $sig.positional;
        my $str := '';
        my $i := 1;
        for @$pos -> $field {
            $str := $str ~ 'my ' ~ $field.emit ~ ' = $_[' ~ $i ~ ']; ';
            $i := $i + 1;
        };
        'sub ' ~ $.name ~ ' { ' ~
          'my ' ~ $invocant.emit ~ ' = $_[0]; ' ~
          $str ~
          (@.block.>>emit).join('') ~ 
        ' }'
    }
}

class Sub {
    has $.name;
    has $.sig;
    has @.block;
    method emit {
        # TODO - signature binding
        my $sig := $.sig;
        # say "Sig: ", $sig.perl;
        ## my $invocant := $sig.invocant;
        # say $invocant.emit;
        my $pos := $sig.positional;
        my $str := '';
        my $i := 0;
        for @$pos -> $field {
            $str := $str ~ 'my ' ~ $field.emit ~ ' = $_[' ~ $i ~ ']; ';
            $i := $i + 1;
        };
        '.sub \'' ~ $.name ~ '\'' ~ Main::newline ~ 
          ## 'my ' ~ $invocant.emit ~ ' = $_[0]; ' ~
          $str ~
          (@.block.>>emit).join('') ~ 
        '.end' ~ Main::newline ~ Main::newline()
    }
}

class Do {
    has @.block;
    method emit {
        (@.block.>>emit).join('') 
    }
}

class Use {
    has $.mod;
    method emit {
        '  .include "' ~ $.mod ~ '"' ~ Main::newline()
    }
}

=begin

=head1 NAME

MiniPerl6::Perl5::Emit - Code generator for MiniPerl6-in-Perl5

=head1 SYNOPSIS

    $program.emit  # generated Perl5 code

=head1 DESCRIPTION

This module generates Perl5 code for the MiniPerl6 compiler.

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2006 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end
