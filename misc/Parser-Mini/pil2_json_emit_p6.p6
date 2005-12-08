=pod

PIL2-JSON simple Perl 6 code emitter

  ../../pugs -CPIL2-JSON -e ' say "hello" ' | \
    ../../pugs pil2_json_emit_p6.p6

  (&say("hello"));

  ../../pugs -Cpil2-json -e ' my Int $x; ($x~"a")( "a",1,$x,$x+1); { say 1 } ' | \
    ../../pugs pil2_json_emit_p6.p6

=cut

use v6;

# tokenizer

my $tokens =
    m:g:perl5 {(\"(?:\\\\|\\"|.)*?\"|[\:\,\=\{\(\[\}\)\]]|\w+)};

# JSON parser 
# outputs a p6 tree = Hash of Array|Hash|Scalar ...

sub parse (@start, $token, @end, @_ is rw) {
    state %tok = (
        token => sub (@_ is rw) { 
                ~ @_.shift;     # '~' stringifies "Match" object
            },
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
                my $key = parse( <<>>, 'token', << : >>, @_ );
                # say " Key $key";
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
                parse( <<>>, 'token', <<>>, @_ );
            },
    );
    # say " parse: @start[] <$token> @end[]";
    for @start { @_.shift eq $_ or die "Expected $_" }; 
    # say " Tail @_[]";
    my $ret = %tok{$token}( @_ );
    for @end   { @_.shift eq $_ or die "Expected $_" }; 
    $ret;
}

#my @pil2 = =<>;
#my $pil2 = @pil2.join('');

# slurp stdin - xinming++ 
my $pil2 = ~list(=$*IN);

my @b = $pil2 ~~ $tokens;
# say "Tokens: ", @b.join('><');
my $ast = parse( << { >>, 'hash', << } >>, @b );
# say $ast.perl;

  # XXX pugs bug ??? - 'my $function' doesn't work recursively,
  #    causing an infinite loop when compiling things like: ($x~"a")(); 
  # We will use an array as a workaround:
  my @function; 

state $depth = 0;
sub dbg ( *@s ) {
    say '  ' x $depth, @s;
}

sub traverse_stmts ( $tree ) {

        my $ret;

        if $tree.ref ne 'Array' {
            dbg "  # <$tree>";
            return $tree;
        }

        if $tree[0].ref eq 'Array' {
            #dbg "# [";
            my @ret;
            push @ret, traverse_stmts( $_ ) for $tree;
            #dbg "# ]";
            return ~@ret;
        }

        # $tree[0].ref is an identifier

        #say $tree.perl;
        $depth++;
        dbg "# $tree[0] start";

        if $tree[0] eq '"PExp"' | '"PExpr"' {
            $ret = traverse_stmts ( $tree[1][0][1] );
        }	
        elsif $tree[0] eq '"PStmt"' {
            $ret = traverse_stmts ( $tree[1][0][1] );
            $ret ~= '; ';
# XXX ??? Changes output completely - lazy evaluation related ???
        }	
        elsif $tree[0] eq '"PStmts"' {
            #dbg $tree[1].perl;
            $ret = traverse_stmts ( $tree[1] );
        }	
        elsif $tree[0] eq '"PNil"' {
            $ret = '';
        }	

        elsif $tree[0] eq '"PVal"' | '"PInt"' {
            $ret = traverse_stmts ( $tree[1][0][1] );
        }	
        elsif $tree[0] eq '"PVar"' {
            $ret = unquote( traverse_stmts ( $tree[1][0][1] ) );
        }	

        elsif $tree[0] eq '"VInt"' {
            dbg "  <$tree[1][0]>";
            $ret = $tree[1][0];
        }	
        elsif $tree[0] eq '"VStr"' {
            dbg "  <$tree[1][0]>";
            $ret = $tree[1][0];
        }	
        elsif $tree[0] eq '"VRat"' {
            #dbg $tree.perl;
            my $a = $tree[1][0][0][1][0];
            my $b = $tree[1][0][0][1][1];
            dbg "  <$a / $b>";
            $ret = "($a / $b)";
        }	

        elsif $tree[0] eq '"pStmts"' | '"pStmt"' | '"pExpr"' | '"pLV"' {
            $ret = traverse_stmts ( $tree[1][0][1] );
        }	
        elsif $tree[0] eq '"MkPos"' {
            $ret = '';
        }

        elsif $tree[0] eq '"PPad"' {
            my %pad = $tree[1];  # keys: "pScope", "pSyms", "pStmts"

            dbg "# keys: ",%pad.keys;
            dbg "Scope:      ", traverse_stmts ( %pad<"pScope"> );
            dbg "Symbols:    ", traverse_stmts ( %pad<"pSyms"> );
            dbg "Statements: ", traverse_stmts ( %pad<"pStmts"> );
            $ret = " ... pad";
        }	

        elsif $tree[0] eq '"PCode"' {
            my %pad = $tree[1];  # keys: "pBody""pIsMulti""pLValue""pParams""pType"

            dbg "# keys: ",%pad.keys;  #
            dbg "Body:       ";
            my $body = traverse_stmts ( %pad<"pBody"> );

            dbg "IsMulti:    ", traverse_stmts ( %pad<"pIsMulti"> );
            dbg "LValue:     ", traverse_stmts ( %pad<"pLValue"> );
            dbg "Parameters: ", traverse_stmts ( %pad<"pParams"> );
            dbg "Type:       ", traverse_stmts ( %pad<"pType"> );

            $ret = '{ ' ~ $body ~ ' }';
            dbg "# Code as_string: $ret";
        }	

        elsif $tree[0] eq '"PApp"' {   
            my %app = $tree[1];  # keys: "pArgs" "pCxt" "pFun" "pInv"
            #dbg "# App: ",%app.perl;

            @function[$depth] = %app<"pFun">; 
            @function[$depth] = @function[$depth][0]; 

            my @args;
            for %app<"pArgs"> {
                push @args, $_[0][1][0][1];
            }

            dbg "# App start ";
            dbg "# keys: ",%app.keys;
            #dbg "# raw:  ",$tree[1].perl;

            dbg "# Function:  ";
            my $str_function = traverse_stmts ( @function[$depth] );

            my @str_args;
            dbg "# Arguments: ", @args.elems;
            for @args {
                push @str_args, traverse_stmts ( $_ );
            };

            dbg "# Context:   ", traverse_stmts ( %app<"pCxt"> );
            dbg "# Invocant:  ", traverse_stmts ( %app<"pInv"> );

            #traverse_stmts( $tree[1] );
            $ret = "(" ~ $str_function ~ "(" ~  @str_args.join(", ") ~ ")" ~ ")";
            dbg "# App as_string: $ret";
        }
        else {
            dbg "# -- unknown node";
            $ret ~= traverse_stmts( $_ ) for $tree;
        }

        dbg "# $tree[0] end";
        $depth--;

        return $ret;
}

sub unquote ( $str ) {
    my ($ret) = $str ~~ m:perl5/^"(.*)"$/;
    #say "unquote $str = $ret";
    return ~$ret;
}

my $program = traverse_stmts( $ast );

say $program;

# TODO 
# - process END, ...

