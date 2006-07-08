# PIL2-JSON simple tokenizer, parser, and code emitter
# ../../pugs -CPIL2-JSON -e ' say "hello" ' | ../../pugs pil2_json_emit.p6

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
my $pil2 = ** $*IN.slurp;

my @b = $pil2 ~~ $tokens;
# say "Tokens: ", @b.join('><');
my $ast = parse( << { >>, 'hash', << } >>, @b );
# say $ast.perl;

sub print_tree ( $tree ) {
    state $depth = 0;
    $depth++;

    if $tree.ref eq 'Array' {

        return if $tree[0] eq '"pPos"';  # ignore position info

        for $tree.values {
            print_tree( $_ );
        }
        say '  ' x ( $depth - 1 ), '-';
    }
    else {
        say '  ' x $depth, "< $tree >";
    }

    $depth--;
}

sub traverse_stmts ( $tree ) {
    if $tree.ref eq 'Array' {
        #say $tree.perl;
        if $tree[0] eq '"PApp"' {   
            state $depth = 0;
            $depth++;
            my $tab = '  ' x $depth;

            say "$tab < App start >";
            my %app = $tree[1];  # keys: "pArgs" "pCxt" "pFun" "pInv"
            #say %app.keys;
            say "$tab     Function:  %app<"pFun">";
            say "$tab     Arguments: %app<"pArgs">";
            say "$tab     Context:   %app<"pCxt">";
            say "$tab     Invocant:  %app<"pInv">";
            # print_tree( $tree[1] )
            traverse_stmts( $tree[1] );
            say "$tab < App end >";

            $depth--;
        }
        else {
            traverse_stmts( $_ ) for $tree
        }
    }
}

traverse_stmts( $ast );

# TODO - process BEGIN, END, ...
