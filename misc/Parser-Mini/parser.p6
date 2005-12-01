# a simple p6 parser
# by fglock

#use strict;

# globals

my $line;
my @tokens;
my @post_tokens;
my $tab_depth; 
my $line_number;

my $debug_tree = 0;   # show tree while it is built

# ---

sub tab {
    return '    ' x $tab_depth;
}

sub error {
    printf "# %04d: %s\n", $line_number, $line;
    print "# *** $_[0]\n";
    die "\n";
}

sub token {
    loop {
        return shift @tokens if @tokens;

        unless ( $line = <> ) {
            return shift @post_tokens if @post_tokens;
            error "end of file";
        }

        $line_number++;
        chomp $line;
        if ( $line =~ /^#/ || $line =~ /^$/ ) { 
            next; 
        };
        # printf "# %04d: %s\n", $line_number, $line;
        @tokens = split( /\b/, $line );
    }
}

sub optional_space {
    my $word;
    while(1) {
        $word = token;
        $word =~ s/^\s+//;
        next if $word eq '';
        unshift @tokens, $word;
        return;
    }
}

sub sentence {
    my @param = @_;  # not used
    #$tab_depth++;
    print tab(), "sentence(\n" if $debug_tree;
    $tab_depth++;

    my @ret;
    
    my $word;
    #$word = token;
    #print "# Start sentence [@param]\n";

    while(1) {
        $word = token;
        # print "<$word> ";

        if ( $word =~ /^(.*?\S)(\s.*?)$/ ) {
            # split on inner space left from the simple tokenizer, like in ' = {'
            $word = $1;
            unshift @tokens, $2;
        }
          
        if ( $word =~ /^\s*\;/ ) {
            #print "# End sentence [@param] [$word]\n";
            $word =~ s/^\s*\;//;
            unshift @tokens, $word if $word ne '';
            
            $tab_depth--;
            print tab(), ")sentence,\n" if $debug_tree;
            #$tab_depth--;
            return @ret;
        };

        if ( $word =~ /^\s*\(/ ) {
            # print "#  paren\n";
            unshift @tokens, $word;
            push @ret, [ parenthesis( 'bare paren' ) ];
            #print "# continue sentence: \n";
            next;
        };
        
        if ( $word =~ /^\s*\{/ ) {
            # print "#  start block\n";
            unshift @tokens, $word;
            push @ret, [ block( 'bare block' ) ];
            #print "# continue sentence: \n";
            next;
        };
        
        if ( $word =~ /^\s*\)/ ) {
            # print "#  end paren\n";
            unshift @tokens, $word;
            $tab_depth--;
            print tab(), ")sentence,\n" if $debug_tree;
            #$tab_depth--;
            return @ret;
        };
        
        if ( $word =~ /^\s*\}/ ) {
            # print "#  end block\n";
            unshift @tokens, $word;
            $tab_depth--;
            print tab(), ")sentence,\n" if $debug_tree;
            #$tab_depth--;
            return @ret;
        };
        
        push @ret, $word;
        print tab(), "'$word'\n" if $debug_tree;

    }
      
}

sub parenthesis {
    my @param = @_;  # not used
    #$tab_depth++;
    print tab(), "paren(\n" if $debug_tree;
    $tab_depth++;

    my @ret;
    
    my $word;
    $word = token;
    #print "# Start paren $tab_depth [@param] [$word]\n";
    $word =~ s/^\s*\(// or error "not a <(> [$word]\n";
    unshift @tokens, $word if $word ne '';
    while(1) {
        $word = token;
        # print " [ $word ] ";
            
        if ( $word =~ /^\s*\)/ ) {
            #print "# End paren $tab_depth [@param] [$word]\n";
            $word =~ s/^\s*\)//;
            unshift @tokens, $word if $word ne '';
            
            $tab_depth--;
            print tab(), ")paren,\n" if $debug_tree;
            #$tab_depth--;
            return @ret;
        };

        if ( $word =~ /^\s*\(/ ) {
            # print "#  paren\n";
            unshift @tokens, $word;
            push @ret, [ parenthesis( 'bare paren' ) ];
        };

        unshift @tokens, $word;
        push @ret, [ sentence( $word ) ];
        
    }
}

sub block {
    my @param = @_;  # not used
    
    my $word;
    $word = token;
    #print "# Start block $tab_depth [@param] [$word]\n";
    print tab(), "block{\n" if $debug_tree;
    $tab_depth++;
    
    my @ret;

    $word ~~ s:perl5/^\s*{// 
        err error "not a <\{> [$word]\n";
    unshift @tokens, $word if $word ne '';
    loop {
        $word = token;
        # print " [ $word ] ";
        if ( $word =~ m:perl5/^(class|method|submethod|sub|multi|macro)$/ ) {
            print tab(), "define(\n" if $debug_tree;
            $tab_depth++;

            my %block;
           
            # multi sub|method
            if ( $word eq 'multi' ) {
                optional_space;
                my $word2 = token;
                if ( $word2 eq 'method' || $word2 eq 'sub' ) {
                    $word .= ' ' . $word2;
                }
                else {
                    push @tokens, $word2;
                }
            };
            $block{thing} = $word;
              
            print tab(), "thing = $word,\n" if $debug_tree;
            
            optional_space;
            $word = token;
            my $namespace;
            if ($word eq '*') { $namespace = $word } 
                else { unshift @tokens, $word };
            print tab(), "namespace_modifier = $namespace,\n" if $debug_tree;
            $block{namespace_modifier} = $namespace;

            $word = token;
            my $name;
            if ($word =~ /(\(|\{)/) { unshift @tokens, $word } 
                else { $name = $word };
            print tab(), "name = $name,\n" if $debug_tree;
            $block{name} = $name;
            
            $word = token;
            unshift @tokens, $word;
            if ( $word =~ /^\s*\(/ ) {
                print tab(), "param = \n" if $debug_tree;
                $tab_depth++;
                $block{param} = [ parenthesis( 'parameter paren' ) ];
                $tab_depth--;
            }
        
            print tab(), "block = \n" if $debug_tree;
            $tab_depth++;
            $block{block} = [ block( $1, $name ) ];
            $tab_depth--;
          

            push @ret, \%block;

            $tab_depth--;
            print tab(), "}define,\n" if $debug_tree;
            next;
        }; # class
            
        if ( $word =~ /^\s*}/ ) {
            #print "# End block $tab_depth [@param] [$word]\n";
            $word =~ s/^\s*\}//;
            unshift @tokens, $word if $word ne '';
            
            $tab_depth--;
            print tab(), "}block,\n" if $debug_tree;
            return @ret;
        };

        if ( $word =~ /^\s*{/ ) {
            # print "#  bare block\n";
            unshift @tokens, $word;
            push @ret, [ block( 'bare block' ) ];
            next;
        };
        
        if ( $word =~ /^\s+$/ ) {
            # spaces
            next;
        }

        unshift @tokens, $word;
        push @ret, [ sentence( $word ) ];

    }
}

# main

$line = '';
@tokens = ( '{' );
@post_tokens = ( '}' );
$tab_depth = 0; 
$line_number = -1;
my @tree = block( 'main block' );

use Data::Dumper;
$Data::Dumper::Indent = 1;
print Dumper( \@tree );
