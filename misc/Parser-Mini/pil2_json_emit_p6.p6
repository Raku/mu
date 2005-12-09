=pod

PIL2-JSON simple Perl 6 code emitter

  ../../pugs -CPIL2-JSON -e ' say "hello" ' | \
    ../../pugs pil2_json_emit_p6.p6

  #! /usr/bin/pugs
  use v6;
  &*END () {  }
  (&say("hello"));

Other examples:

  ../../pugs -Cpil2-json -e ' my Int $x; ($x~"a")( "a",1,$x,$x+1); { say 1 } ' | \
    ../../pugs pil2_json_emit_p6.p6

The code created by this example has syntax errors, but I'm not sure if these things are
really forbidden in p6:

  ../../pugs -Cpil2-json -e 'my ($x,$y)=(1,2); sub infix:<aaa>($a,$b){$a+1} 1 aaa 2;' | \
    ../../pugs pil2_json_emit_p6.p6

Not supported yet:

  - parameter typing (half implemented, need some work)
  - Classes (untested)

=cut

use v6;

my $debug_traverse = 0;

# tokenizer

my $tokens = m:g:perl5 {(\"(?:\\\\|\\"|.)*?\"|[\:\,\=\{\(\[\}\)\]]|\w+)};

# JSON parser - creates an Array [of Array]* of Str

sub parse (@start, $token, @end, @_ is rw) {
    state %tok = (
        hash =>  sub (@_ is rw) {
                my Array $a;
                loop {
                    return $a if @_[0] eq '}';
                    push $a, parse( <<>>, 'pair', <<>>, @_ );
                    return $a if @_[0] ne ',';
                    @_.shift; 
                }
            },
        list =>  sub (@_ is rw) {
                my Array $a;
                loop {
                    return $a if @_[0] eq ']';
                    push $a, parse( <<>>, 'item', <<>>, @_ );
                    return $a if @_[0] ne ',';
                    @_.shift; 
                }
            },
        pair =>  sub (@_ is rw) {
                my $key = ~@_.shift;
                @_.shift eq ':' or die "Expected ':'";
                my $value = parse( <<>>, 'item', <<>>, @_ );
                [ $key, $value ];
            },
        item =>  sub (@_ is rw) {
                if @_[0] eq '{' { 
                    return parse( << { >>, 'hash', << } >>, @_ ) 
                };
                if @_[0] eq '[' { 
                    return parse( << [ >>, 'list', << ] >>, @_ ) 
                };
                ~@_.shift;
            },
    );
    for @start { @_.shift eq $_ or die "Expected $_" }; 
    my $ret = %tok{$token}( @_ );
    for @end   { @_.shift eq $_ or die "Expected $_" }; 
    $ret;
}

sub traverse_ast ( $tree ) {
    state $depth = 0;
    
    sub dbg ( *@s ) { 
        return unless $debug_traverse;
        say '  ' x $depth, @s 
    }
    
    if $tree.ref ne 'Array' {
        dbg "  # -- unknown: <$tree>";
        return; 
    }
    if $tree[0].ref eq 'Array' {
        #dbg "# [";
        my @ret;
        push @ret, traverse_ast( $_ ) for $tree;
        #dbg "# ]";
        return ~@ret;
    }

    $depth++;
    dbg "# $tree[0] start";
    my $ret;

    if $tree[0] eq '"PIL_Environment"' {
        my %pad = $tree[1];  # keys: "pilGlob", "pilMain"
        dbg "# keys: ",%pad.keys;
        dbg "# pilGlob:    "; my $global = traverse_ast ( %pad<"pilGlob"> );
        dbg "# pilMain:    "; my $main =   traverse_ast ( %pad<"pilMain"> );
        $ret = emit_Main( $global, $main );
    }	
    elsif $tree[0] eq '"PExp"' | '"PExpr"' {
        $ret = traverse_ast ( $tree[1][0][1] );
    }	
    elsif $tree[0] eq '"PVal"' | '"PInt"' | '"PLit"' {
        $ret = traverse_ast ( $tree[1][0][1] );
    }	
    elsif $tree[0] eq '"PStmt"' {
        $ret = emit_Stmt( traverse_ast ( $tree[1][0][1] ) );
    }	
    elsif $tree[0] eq '"PStmts"' {
        my %pad = $tree[1];  # keys: 
        dbg "# keys: ",%pad.keys;
        $ret =  traverse_ast ( %pad<"pStmt"> );
        $ret ~= traverse_ast ( %pad<"pStmts"> );
    }	
    elsif $tree[0] eq '"PNil"' {
        $ret = '';
    }	
    elsif $tree[0] eq '"PVar"' {
        $ret = emit_Variable( $tree[1][0][1] );
    }
    elsif $tree[0] eq '"VInt"' {
        $ret = emit_Int( $tree[1][0] );
    }	
    elsif $tree[0] eq '"VStr"' {
        $ret = emit_Str( $tree[1][0] );
    }	
    elsif $tree[0] eq '"VRat"' {
        $ret = emit_Rat( 
            $tree[1][0][0][1][0],
            $tree[1][0][0][1][1] );
    }	
    elsif $tree[0] eq '"pLV"' | '"pLit"' {
        # XXX
        $ret = traverse_ast ( $tree[1][0] );
    }	
    elsif $tree[0] eq '"pExpr"' {
        # XXX
        $ret = traverse_ast ( $tree[1][0][1] );
    }	
    elsif $tree[0] eq '"PNoop"' {
        $ret = '';
    }
    elsif $tree[0] eq '"MkTParam"' {
        my %pad = $tree[1];  # keys: "tpDefault""tpParam"
        dbg "# keys: ",%pad.keys;
        $ret = emit_parameter_with_default(
            traverse_ast( %pad<"tpParam"> ), traverse_ast( %pad<"tpDefault"> ) 
        );
    }
    elsif $tree[0] eq '"MkParam"' {
        my %pad = $tree[1];  # keys: "isInvocant""isLValue""isLazy""isNamed""isOptional"
                             #       "isWritable""paramContext""paramDefault""paramName"
        dbg "# keys: ",%pad.keys;
        $ret = emit_parameter( 
            %pad<"paramName">,
            %pad<<"isInvocant" "isLValue" "isLazy" "isNamed" "isOptional" "isWritable">>, 
            %pad<"paramContext">,   # TODO
            %pad<"paramDefault">,   # TODO
        );
    }
    elsif $tree[0] eq '"PPos"' {
        my %pad = $tree[1];  # keys: "pExp""pNode""pPos"
        dbg "# keys: ",%pad.keys;
        $ret = traverse_ast( %pad<"pNode"> );
    }
    elsif $tree[0] eq '"PAssign"' {
        my %pad = $tree[1];  # keys: "pLHS""pRHS"
        dbg "# keys: ",%pad.keys;
        dbg "# Assign to:      "; my $to =      traverse_ast ( %pad<"pLHS"> );
        dbg "# Assign from:    "; my $from =    traverse_ast ( %pad<"pRHS"> );
        $ret = emit_Assign( $to, $from );
    }
    elsif $tree[0] eq '"PBind"' {
        my %pad = $tree[1];  # keys: "pLHS""pRHS"
        dbg "# keys: ",%pad.keys;
        dbg "# Assign to:      "; my $to =      traverse_ast ( %pad<"pLHS"> );
        dbg "# Assign from:    "; my $from =    traverse_ast ( %pad<"pRHS"> );
        $ret = emit_Bind( $to, $from );
    }
    elsif $tree[0] eq '"PPad"' {
        my %pad = $tree[1];  # keys: "pScope", "pSyms", "pStmts"
        dbg "# keys: ",%pad.keys;

        my $scope =      %pad<"pScope">[0][0];
        dbg "# Scope:      $scope";  # "SMy"  TODO - what are the other scopes?

        #say %pad<"pSyms">.perl;
        my @symbols = %pad<"pSyms">.map:{ $_[0] };
        dbg "# Symbols:    @symbols[]";

        dbg "# Statements: "; my $statements = traverse_ast ( %pad<"pStmts"> );
        $ret = emit_Pad( $scope, @symbols, $statements );
    }	
    elsif $tree[0] eq '"PCode"' {
        my %pad = $tree[1];  # keys: "pBody""pIsMulti""pLValue""pParams""pType"
        dbg "# keys: ",%pad.keys;  
        dbg "# Body:       "; my $body =     traverse_ast ( %pad<"pBody"> );
        dbg "# IsMulti:    "; my $is_multi = %pad<"pIsMulti">;
        dbg "# LValue:     "; my $lvalue =   %pad<"pLValue">;
        dbg "# Parameters: "; my @params =   traverse_ast ( %pad<"pParams"> );
        dbg "# Type:       "; my $type =     traverse_ast ( %pad<"pType"> );
        $ret = emit_Code( $body, $is_multi, $lvalue, @params, $type );
    }	
    elsif $tree[0] eq '"PSub"' {
        my %pad = $tree[1];  # keys: "pSubBody""pSubIsMulti""pSubLValue""pSubName""pSubParams""pSubType"
        dbg "# keys: ",%pad.keys;
        dbg "# Body:       "; my $body =     traverse_ast ( %pad<"pSubBody"> );
        dbg "# IsMulti:    "; my $is_multi = %pad<"pSubIsMulti">;
        dbg "# LValue:     "; my $lvalue =   %pad<"pSubLValue">;
        dbg "# Parameters: "; 
        my @params;
        for %pad<"pSubParams"> {
            push @params, traverse_ast ( $_ );
        }
        dbg "# Type:       "; my $type =     traverse_ast ( %pad<"pSubType"> );
        dbg "# Name:       "; my $name =     emit_Variable( %pad<"pSubName"> );
        $ret = emit_Sub( $name, $body, $is_multi, $lvalue, @params, $type );
    }
    elsif $tree[0] eq '"PApp"' {   
        my %app = $tree[1];  # keys: "pArgs" "pCxt" "pFun" "pInv"
        dbg "# keys: ",%app.keys;
        #dbg "# App: ",%app.perl;
        my @args;
        for %app<"pArgs"> {
            push @args, traverse_ast ( $_[0][1][0][1] );
        }
        dbg "# Arguments: ", @args.elems;
        dbg "# Function:  "; my $function = traverse_ast ( %app<"pFun"> ); 
        dbg "# Context:   "; my $context =  traverse_ast ( %app<"pCxt"> );
        dbg "# Invocant:  "; my $invocant = traverse_ast ( %app<"pInv"> );
        $ret = emit_App( $function, @args, $context, $invocant );
    }
    else {
        dbg "# -- unknown node";
        $ret = " # ??? $tree[0]\n";
        $ret ~= traverse_ast( $_ ) for $tree;
    }

    dbg "# $tree[0] as_string: $ret";
    dbg "# $tree[0] end";
    $depth--;
    $ret;
}

# -- Language specific

sub emit_Main ( $global, $main ) {
    "#! /usr/bin/pugs\n" ~
    "use v6;\n" ~
    $global ~ $main
}
sub emit_Stmt ( $s ) { $s ~ '; ' }
sub emit_Code ( $body, $is_multi, $lvalue, @params, $type ) {
    '{ ' ~ $body ~ ' }'
}	
sub emit_Assign( $to, $from ) { $to ~ ' = ' ~ $from }
sub emit_Bind( $to, $from )   { $to ~ ' := ' ~ $from }
sub emit_Sub ( $name, $body, $is_multi, $lvalue, @params, $type ) {
    " $name (" ~ @params.join(", ") ~ ") { " ~ $body ~ " \}\n"
}	
sub emit_App ( $function, @args, $context, $invocant ) {   
    "(" ~ $function ~ "(" ~  @args.join(", ") ~ ")" ~ ")"
}
sub emit_Pad ( $scope, @symbols, $statements ) {
    "\{\n" ~ @symbols.map:{ "my " ~ emit_Variable($_) ~ ";\n" } ~ "$statements\n\}\n";
}
sub emit_Variable ( $s is copy ) {
    # rewrite PIL2 '"&infix:+"' to p6 '&infix:<+>'
    # but don't re-quote '&main::zz'
    $s ~~ s:perl5/^"(.*)"$/$0/;
    $s ~~ s:perl5{\&(.+fix:)([^:].*)}{&$0:<$1>}; 

    # XXX fix corner cases like 'infix:<:>' and '&main::infix:<aaa>'
    $s ~~ s:perl5{(fix::<)(.*?)>$}{fix:<$1>};   
    $s ~~ s:perl5{fix::$}{fix:<:>};   

    $s
}
sub emit_Int ( $s ) { $s }
sub emit_Str ( $s ) { $s }
sub emit_Rat ( $a, $b ) { "($a / $b)" }
sub emit_parameter( $name, *@param ) {
    return emit_Variable( $name ); # XXX incomplete!
    return '"<<param_not_implemented_yet-' ~ @param.join('+') ~ '>>"';  # TODO
}
sub emit_parameter_with_default( $param, $default ) {
    if $default eq '' { $param }
    else { $param ~ ' = ' ~ $default }
}

# -- Main program

# slurp stdin - xinming++ 
my $pil2 = ~$*IN.slurp;

my @b = $pil2 ~~ $tokens;
# say "Tokens: ", @b.join('><');

my $ast = parse( << { >>, 'hash', << } >>, @b );
# say $ast.perl;

my $program = traverse_ast( $ast );

say $program;
