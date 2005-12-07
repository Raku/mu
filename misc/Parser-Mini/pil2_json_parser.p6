# PIL2 simple tokenizer & parser
# ../../pugs -CPIL2-JSON -e ' say "hello" ' | ../../pugs pil2_tokenizer.p6

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
                my Hash $h;
                my Pair $p;
                loop {
                    return $h if @_[0] eq '}';
                    $p = parse( <<>>, 'pair', <<>>, @_ );
                    $h{$p.key} = $p.value;
                    return $h if @_[0] ne ',';
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
                ( $key => $value );
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

my @pil2 = =<>;
my $pil2 = @pil2.join('');

# my $pil2 = =<>.slurp;   ???

my @b = $pil2 ~~ $tokens;
# say "Tokens: ", @b.join('><');
my $ast = parse( << { >>, 'hash', << } >>, @b );
say $ast.perl;
