# a simple p6 parser
# by fglock

use v6;

# globals

my $line;
my @tokens;
my @post_tokens;
my $tab_depth; 
my $line_number;

my $debug_tree = 1;   # show tree while it is built

# ---

sub tab {
    return '    ' x $tab_depth;
}

sub error {
    print "# $line_number: $line\n";
    print "# *** $_[0]\n";
    die "\n";
}

sub token {
    state $empty_line;
    loop {
        if @tokens {
            if @tokens[0] ~~ '#' {
                # skip comment
                @tokens = ();
                next;
            }
            # print "<",@tokens[0],"> ";
            return shift @tokens;
        }
        $line = =<>;
        unless defined $line {
            return shift @post_tokens if @post_tokens;
            error "end of file";
        }
        $line_number++;                
        # print "# $line_number: $line\n";
        @tokens = $line ~~ m:g:perl5{(\w+|\s+|.+?)};   # \b doesn't work ???
        @tokens = @tokens.map:{ ~$_ };  # force stringify ???
        # say "tokens: ", @tokens.join('|');
    }
}

sub optional_space {
    my $word;
    loop {
        $word = token;
        next if $word ~~ m:perl5/^\s/;
        unshift @tokens, $word;
        return;
    }
}

sub sentence {
    print tab(), "sentence(\n" if $debug_tree;
    $tab_depth++;
    my @ret;
    my $word;
    #print "# Start sentence\n";
    loop {
        $word = token;
        # print "<$word> ";
        if ( $word ~~ ';' ) {            
            $tab_depth--;
            print tab(), ")sentence,\n" if $debug_tree;
            return @ret;
        };
        if ( $word ~~ '(' ) {
            # print "#  paren\n";
            unshift @tokens, $word;
            push @ret, [ parenthesis() ];
            next;
        };
        if ( $word ~~ '{' ) {
            # print "#  start block\n";
            unshift @tokens, $word;
            push @ret, [ block() ];
            next;
        };
        if ( $word ~~ ')' || $word ~~ '}' ) {
            # print "#  end paren|block\n";
            unshift @tokens, $word;
            $tab_depth--;
            print tab(), ")sentence,\n" if $debug_tree;
            return @ret;
        };
        push @ret, $word;
        print tab(), "<$word>\n" if $debug_tree;
    }
}

sub parenthesis {
    print tab(), "paren(\n" if $debug_tree;
    $tab_depth++;
    my @ret;
    my $word = token;
    $word ~~ '(' err error "not a <(> [$word]\n";
    loop {
        $word = token;
        if $word ~~ ')' {            
            $tab_depth--;
            print tab(), ")paren,\n" if $debug_tree;
            return @ret;
        };
        unshift @tokens, $word;
        push @ret, [ sentence() ];
    }
}

sub block {
    print tab(), "block{\n" if $debug_tree;
    $tab_depth++;
    my @ret;
    my $word = token;
    # print "token1<$word> ";
    $word ~~ '{' err error "not a <{> [$word]\n";
    loop {
        $word = token;
        # print "token2<$word> ";
        if $word ~~ m:perl5/^(class|method|submethod|sub|multi|macro)$/ {
            print tab(), "define(\n" if $debug_tree;
            $tab_depth++;
            my %block;
            
            # multi sub|method
            if $word ~~ 'multi' {
                optional_space;
                my $word2 = token;
                if $word2 ~~ 'method' || $word2 ~~ 'sub' {
                    $word ~= ' ' ~ $word2;
                }
                else {
                    push @tokens, $word2;
                }
            };
            %block<thing> = $word;
            print tab(), "thing = $word,\n" if $debug_tree;
            
            optional_space;
            $word = token;
            if $word ~~ '*' { 
                %block<namespace_modifier> = $word;
                print tab(), "namespace_modifier = $word,\n" if $debug_tree;
            } 
            else { 
                unshift @tokens, $word 
            };

            $word = token;
            if $word ~~ '(' || $word ~~ '{' { 
                unshift @tokens, $word 
            } 
            else { 
                %block<name> = $word;
                print tab(), "name = $word,\n" if $debug_tree;
            };
            
            $word = token;
            unshift @tokens, $word;
            if $word ~~ '(' {
                print tab(), "param = \n" if $debug_tree;
                $tab_depth++;
                %block<param> = [ parenthesis() ];
                $tab_depth--;
            }
        
            #print "<< ", @tokens , " -- $line >>\n";
            print tab(), "block = \n" if $debug_tree;
            $tab_depth++;
            optional_space;
            %block<block> = [ block() ];
            $tab_depth--;
          
            push @ret, \%block;
            $tab_depth--;
            print tab(), "}define,\n" if $debug_tree;
            next;
        }; # class
            
        if ( $word ~~ '}' ) {
            #print "# End block $tab_depth [$word]\n";            
            $tab_depth--;
            print tab(), "}block,\n" if $debug_tree;
            return @ret;
        };

        if ( $word ~~ '{' ) {
            # print "#  bare block\n";
            unshift @tokens, $word;
            push @ret, [ block() ];
            next;
        };
        
        if ( $word ~~ m:perl5/^\s/ ) {
            # spaces
            next;
        }

        unshift @tokens, $word;
        push @ret, [ sentence() ];
    }
}

# main

$line = '';
@tokens = ( '{' );
@post_tokens = ( '}' );
$tab_depth = 0; 
$line_number = -1;
my @tree = block( 'main block' );

print @tree.perl;
