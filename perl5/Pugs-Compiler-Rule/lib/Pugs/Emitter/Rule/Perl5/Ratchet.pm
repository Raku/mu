package Pugs::Emitter::Rule::Perl5::Ratchet;

# p6-rule perl5 emitter for ":ratchet" (non-backtracking)
# see: RuleInline.pl, RuleInline-more.pl for a program prototype

# XXX - cleanup unused nodes

use strict;
use warnings;
use Data::Dump::Streamer;
$Data::Dump::Streamer::Indent = 1;

our $direction = "+";  # XXX make lexical
our $sigspace = 0;

our $count = 1000 + int(rand(1000));
sub id { 'I' . ($count++) }

# XXX - reuse this sub in metasyntax()
sub call_subrule {
    my ( $subrule, $tab, @param ) = @_;
    $subrule = "\$grammar->" . $subrule unless $subrule =~ / :: | \. | -> /x;
    $subrule =~ s/\./->/;   # XXX - source filter
    return 
        "$tab     $subrule( \$s, { p => \$pos, args => {" . join(", ",@param) . "} }, \$_[3] )";
}

sub call_constant {
    return " 1 # null constant\n"
        unless length($_[0]);
    my $len = length( $_[0] );
    my $const;
    #print "Const: [$_[0]] ". length($_[0])."\n";
    if ( $_[0] eq "\\" ) {
        $const = "chr(".ord("\\").")";
    }
    elsif ( $_[0] eq "'" ) {
        $const = "chr(".ord("'").")"
    }
    else {
        $const = "'$_[0]'"
    }
    return
    "$_[1] ( ( substr( \$s, \$pos, $len ) eq $const ) 
$_[1]     ? do { \$pos $direction= $len; 1 }
$_[1]     : 0
$_[1] )";
}

sub call_perl5 {
    my $const = $_[0];
    return
    "$_[1] ( ( substr( \$s, \$pos ) =~ m/^$const/s )  
$_[1]     ? do { \$pos $direction= length \$&; 1 }
$_[1]     : 0
$_[1] )";
}

sub emit {
    my ($grammar, $ast, $param) = @_;
    # runtime parameters: $grammar, $string, $state, $arg_list
    # rule parameters: see Runtime::Rule.pm
    local $sigspace = $param->{sigspace};   # XXX - $sigspace should be lexical
    return 
        "sub {\n" . 
        "  my \$grammar = \$_[0];\n" .
        "  my \$s = \$_[1];\n" .
        "  my \$pos = \$_[3]{p};\n" .
        #"  print \"match arg_list = \$_[1]\n\";\n" .
        #"  print \"match arg_list = \@{[\%{\$_[1]} ]}\n\" if defined \$_[1];\n" .
        "  \$pos = 0 unless defined \$pos;   # TODO - .*? \$match \n" .
        #"  print \"match pos = \$pos\n\";\n" .
        "  my \%index;\n" . 
        "  my \@match;\n" .
        "  my \%named;\n" .
        #"  my \$from = \$pos;\n" .
        "  my \$bool = 1;\n" .
        "  my \$capture;\n" .
        "  my \$quantified;\n" .
        "  my \$m = bless \\{ \n" .
        "    str => \\\$s, from => \\(0+\$pos), to => \\(\$pos), \n" .
        "    bool => \\\$bool, match => \\\@match, named => \\\%named, capture => \\\$capture, \n" .
        "  }, 'Pugs::Runtime::Match::Ratchet';\n" .
        "  \$bool = 0 unless\n" .
        emit_rule( $ast, ' ' ) . ";\n" .
        "  return \$::_V6_MATCH_ = \$m;\n" .
        "}\n";
}

sub emit_rule {
    my $n = $_[0];
    my $tab = $_[1] . '  ';
    die "unknown node: ", Dump( $n )
        unless ref( $n ) eq 'HASH';
    #print "NODE ", Dump($n);
    my ($k) = keys %$n;
    my $v = $$n{$k};
    #my ( $k, $v ) = each %$n;
    # XXX - use real references
    no strict 'refs';
    #print "NODE ", Dump($k), ", ", Dump($v);
    my $code = &$k( $v, $tab );
    return $code;
}

#rule nodes

sub non_capturing_group {
    return emit_rule( $_[0], $_[1] );
}        
sub quant {
    my $term = $_[0]->{'term'};
    my $quantifier = $_[0]->{quant};
    #print "QUANT: ",Dump($_[0]);
    $quantifier = '' unless defined $quantifier;
    # TODO: fix grammar to not emit empty quantifier
    my $tab = ( $quantifier eq '' ) ? $_[1] : $_[1] . "  ";
    my $ws = metasyntax( '?ws', $tab );
    my $ws3 = ( $sigspace && $_[0]->{ws3} ne '' ) ? " &&\n$ws" : '';
    my $rul = emit_rule( $term, $tab );
    $rul = "$ws &&\n$rul" if $sigspace && $_[0]->{ws1} ne '';
    $rul = "$rul &&\n$ws" if $sigspace && $_[0]->{ws2} ne '';
    #print $rul;
    return $rul 
        if $quantifier eq '';
    # *  +  ?
    # TODO: *? +? ??
    # TODO: *+ ++ ?+
    # TODO: quantifier + capture creates Array
    return 
        "$_[1] do { my \$quantified = 1; (\n$rul\n" .
        "$_[1] ||\n" .
        "$_[1]   1\n" .
        "$_[1] ) }$ws3"
        if $quantifier eq '?';
    return 
        "$_[1] do { my \$quantified = 1; while (\n$rul) {}; 1 }$ws3"
        if $quantifier eq '*';
    return
        "$_[1] do { my \$quantified = 1;\n" . 
        "$_[1] (\n$rul\n" .
        "$_[1] &&\n" .
        "$_[1]   do { while (\n$rul) {}; 1 }\n" .
        "$_[1] ) }$ws3"
        if $quantifier eq '+';
    die "quantifier not implemented: $quantifier";
}        
sub alt {
    my @s;
    for ( @{$_[0]} ) { 
        my $tmp = emit_rule( $_, $_[1].'  ' );
        push @s, $tmp if $tmp;   
    }
    return 
        "$_[1] do {
$_[1]   my \$pos1 = \$pos;
$_[1]   do {
" . join( "\n$_[1]   } || do { \$pos = \$pos1;\n", @s ) . "
$_[1]   }
$_[1] }";
}        
sub concat {
    my @s;
    for ( @{$_[0]} ) { 
        my $tmp = emit_rule( $_, $_[1] );
        push @s, $tmp if $tmp;   
    }
    @s = reverse @s if $direction eq '-';
    return "$_[1] (\n" . join( "\n$_[1] &&\n", @s ) . "\n$_[1] )";
}        
sub code {
    return "$_[1] $_[0]\n";  
}        
sub dot {
    if ( $direction eq '+' ) {
        "$_[1] do { \$pos < length( \$s ) ? ++\$pos : 0 }"
    }
    else {
        "$_[1] do { \$pos >= 0 ? do{ --\$pos; 1 } : 0 }"
    }
}

sub preprocess_hash {
    my ( $h, $key ) = ( $_[0]->{hash}, $_[0]->{key} );
    # returns AST depending on $h
    if ( ref( $h->{$key} ) eq 'CODE') {
        return " do { \$hash->{'$key'}->(); 1; } ";
    } 
    if ( ref( $h->{$key} ) =~ /Pugs::Compiler::/ ) {
        return " return \$hash->{'$key'}->match( \$s, \$grammar, { p => \$pos } ) ";
    }
    # fail is number != 1 
    if ( $h->{$key} =~ /^(\d+)$/ ) {
        return " 0 " unless $1 == 1;
        return " 1 " ;
    }
    # subrule
    return " do { warn \"uncompiled subrule: $h->{$key} - not implemented \" } ";
}

sub variable {
    my $name = "$_[0]";
    my $value = undef;
    # XXX - eval $name doesn't look up in user lexical pad
    # XXX - what &xxx interpolate to?
    
    if ( $name =~ /^\$/ ) {
        # $^a, $^b
        if ( $name =~ /^ \$ \^ ([^\s]*) /x ) {
            my $index = ord($1)-ord('a');
            #print "Variable #$index\n";
            #return "$_[1] constant( \$_[7][$index] )\n";
            
            my $code = 
            "    ... sub { 
                #print \"Runtime Variable args[\", join(\",\",\@_) ,\"] \$_[7][$index]\\n\";
                return constant( \$_[7][$index] )->(\@_);
            }";
            $code =~ s/^/$_[1]/mg;
            return "$code\n";
        }
        else {
            $value = eval $name;
        }
    }
    
    $value = join('', eval $name) if $name =~ /^\@/;
    if ( $name =~ /^%/ ) {
        my $id = '$' . id();
        my $code = "
          do {
            our $id;
            unless ( $id ) {
                my \$hash = " . 
                ( $name =~ /::/ 
                    ? "\\$name" 
                    : "Pugs::Runtime::Rule::get_variable( '$name' )"
                ) . 
                ";
                my \$ast = {
                    alt => [
                        map  {{ 
                            concat => [
                                { constant => \$_ },
                                { preprocess_hash => { hash => \$hash, key => \$_ }, },
                            ] }}
                        sort { length \$b <=> length \$a } keys \%\$hash
                    ]
                };
                #print 'ast: ', Data::Dump::Streamer::Dump( \$ast );
                my \$code = Pugs::Emitter::Rule::Perl5::Ratchet::emit( \$grammar, \$ast );
                #print 'code: ', \$code;
                $id = eval \$code;
            }
            my \$match = $id->( \$grammar, \$s, { p => \$pos, args => {} }, \$_[3] );
            my \$bool = (!\$match != 1);
            \$pos = \$match->to if \$bool;
            #print !\$match[-1], ' ', Dump \$match[-1];
            \$bool;
          }";
        #print $code;
        return $code;
    }
    die "interpolation of $name not implemented"
        unless defined $value;

    return call_constant( $value, $_[1] );
}
sub special_char {
    my $char = substr($_[0],1);
    for ( qw( r n t e f w d s ) ) {
        return call_perl5(   "\\$_",  $_[1] ) if $char eq $_;
        return call_perl5( "[^\\$_]", $_[1] ) if $char eq uc($_);
    }
    return call_constant( $char, $_[1] );
}
sub match_variable {
    my $name = $_[0];
    my $num = substr($name,1);
    #print "var name: ", $num, "\n";
    my $code = 
    "    ... sub { 
        my \$m = Pugs::Runtime::Match->new( \$_[2] );
        return constant( \"\$m->[$num]\" )->(\@_);
    }";
    $code =~ s/^/$_[1]/mg;
    return "$code\n";
}
sub closure {
    my $code = $_[0]; 
    
    if ( ref( $code ) ) {
        if ( defined $Pugs::Compiler::Perl6::VERSION ) {
            # perl6 compiler is loaded
            my $perl5 = Pugs::Emitter::Perl6::Perl5::emit( 'grammar', $code, 'self' );
            return 
                "do { 
                    \$::_V6_MATCH_ = \$m; 
                    \$capture = sub { $perl5 }->();
                    \$bool = 1;
                    return \$m;
                }" if $perl5 =~ /return/;
            return 
                "do { 
                    \$::_V6_MATCH_ = \$m; 
                    sub { $perl5 }->();
                    1;
                }";
        }        
    }
    
    # XXX XXX XXX - source-filter - temporary hacks to translate p6 to p5
    # $()<name>
    $code =~ s/ ([^']) \$ \( \) < (.*?) > /$1 \$_[0]->[$2] /sgx;
    # $<name>
    $code =~ s/ ([^']) \$ < (.*?) > /$1 \$_[0]->{$2} /sgx;
    # $()
    $code =~ s/ ([^']) \$ \( \) /$1 \$_[0]->() /sgx;
    # $/
    $code =~ s/ ([^']) \$ \/ /$1 \$_[0] /sgx;
    #print "Code: $code\n";
    
    return 
        "$_[1] ( sub $code->( \$m ) || 1 )" 
        unless $code =~ /return/;
        
    return
        "$_[1] ( ( \$capture = sub $code->( \$m ) ) 
$_[1]   && return \$m )";
}
sub capturing_group {
    my $program = $_[0];

    $program = emit_rule( $program, $_[1].'      ' )
        if ref( $program );
    my $rnd = id();
    return "$_[1] do{ 
$_[1]     my \$hash = do {
$_[1]       my \$bool = 1;
$_[1]       my \$from = \$pos;
$_[1]       my \@match;
$_[1]       my \%named;
$_[1]       my \$capture;
$_[1]       my \$quantified;
$_[1]       \$bool = 0 unless
" .             $program . ";
$_[1]       { str => \\\$s, from => \\\$from, match => \\\@match, named => \\\%named, bool => \$bool, to => \\(0+\$pos), capture => \\\$capture }
$_[1]     };
$_[1]     my \$bool = \$hash->{'bool'};
$_[1]     \$index{$rnd} = \$#match+1 unless defined \$index{$rnd};
$_[1]     if ( \$quantified ) {
$_[1]       if ( \$bool ) {
$_[1]         push \@{ \$match[\$index{$rnd}] }, bless \\\$hash, 'Pugs::Runtime::Match::Ratchet';
$_[1]       }
$_[1]       else {
$_[1]         \@{ \$match[\$index{$rnd}] } = () 
$_[1]           if ! defined \$match[\$index{$rnd}];
$_[1]       }
$_[1]     }
$_[1]     else {
$_[1]       if ( ! defined \$match[\$index{$rnd}] ) {
$_[1]         \$match[\$index{$rnd}] = bless \\\$hash, 'Pugs::Runtime::Match::Ratchet';
$_[1]       }
$_[1]       elsif ( ref( \$match[\$index{$rnd}] ) ne 'ARRAY' ) {
$_[1]         \$match[\$index{$rnd}] = [ \$match[\$index{$rnd}], bless \\\$hash, 'Pugs::Runtime::Match::Ratchet' ];
$_[1]       }
$_[1]       else {
$_[1]         push \@{ \$match[\$index{$rnd}] }, bless \\\$hash, 'Pugs::Runtime::Match::Ratchet';
$_[1]       }
$_[1]       #unshift \@{ \$match[\$index{$rnd}] } unless \$bool;
$_[1]     }
$_[1]     \$bool;
$_[1] }";
}        
sub named_capture {
    my $name    = $_[0]{ident};
    my $program = $_[0]{rule};
    my $flat    = $_[0]{flat};
    $program = emit_rule( $program, $_[1].'        ' )
        if ref( $program );
    # TODO - repeated captures create an Array

    my($try_match, $gen_match, $post_match);
    if ( $flat ) {
        $try_match = <<"."
$_[1]     my \$bool = 1;
$_[1]     \$bool = 0 unless
.
.            $program . ";\n";
        $gen_match = "\$match[-1]";
        $post_match = "\$#match--;";
    } else {
        $try_match = <<"." ;
$_[1]     my \$hash = do {
$_[1]       my \$bool = 1;
$_[1]       my \$from = \$pos;
$_[1]       my \@match;
$_[1]       my \%named;
$_[1]       my \$capture;
$_[1]       \$bool = 0 unless
$program;
$_[1]       { str => \\\$s, from => \\\$from, match => \\\@match, named => \\\%named, bool => \\\$bool, to => \\(0+\$pos), capture => \\\$capture }
$_[1]     };
$_[1]     my \$bool = \${\$hash->{'bool'}};
.
        $gen_match = "bless \\\$hash, 'Pugs::Runtime::Match::Ratchet'";
        $post_match = "";
    }

    return "$_[1] do{ 
$try_match
$_[1]     if ( \$bool ) {
$_[1]       my \$match = $gen_match;
$_[1]       if ( \$quantified ) {
$_[1]         \$named{'$name'} = [] if ! defined \$named{'$name'};
$_[1]         push \@{\$named{'$name'}}, \$match;
$_[1]       } else {
$_[1]         if ( ! defined \$named{'$name'} ){
$_[1]           \$named{'$name'} = \$match;
$_[1]         } elsif ( ref ( \$named{'$name'} ) ne 'ARRAY' ){
$_[1]           \$named{'$name'} = [\$named{'$name'}, \$match];
$_[1]         } else {
$_[1]           push \@{ \$named{'$name'} }, \$match;
$_[1]         }
$_[1]       }
$_[1]     }
$_[1]     $post_match
$_[1]     \$bool;
$_[1] }";
}
sub negate {
    my $program = $_[0]{rule};
    $program = emit_rule( $program, $_[1].'        ' )
        if ref( $program );
    return "$_[1] do{ 
$_[1]     my \$pos1 = \$pos;
$_[1]     do {
$_[1]       my \$pos = \$pos1;
$_[1]       my \$from = \$pos;
$_[1]       my \@match;
$_[1]       my \%named;
$_[1]       my \$capture;
$_[1]       \$bool = " . $program . " ? 0 : 1;
$_[1]       \$bool;
$_[1]     };
$_[1] }";
}
sub before {
    my $program = $_[0]{rule};
    $program = emit_rule( $program, $_[1].'        ' )
        if ref( $program );
    return "$_[1] do{ 
$_[1]     my \$pos1 = \$pos;
$_[1]     do {
$_[1]       my \$pos = \$pos1;
$_[1]       my \$from = \$pos;
$_[1]       my \@match;
$_[1]       my \%named;
$_[1]       my \$capture;
$_[1]       \$bool = 0 unless
" .             $program . ";
$_[1]       \$bool;
$_[1]     };
$_[1] }";
}
sub not_before {
    warn '<!before ...> not implemented';
    return;
}
sub after {
    local $direction = "-";
    my $program = $_[0]{rule};
    $program = emit_rule( $program, $_[1].'        ' )
        if ref( $program );
    return "$_[1] do{ 
$_[1]     my \$pos1 = \$pos;
$_[1]     do {
$_[1]       my \$pos = \$pos1 - 1;
$_[1]       my \$from = \$pos;
$_[1]       my \@match;
$_[1]       my \%named;
$_[1]       my \$capture;
$_[1]       \$bool = 0 unless
" .             $program . ";
$_[1]       \$bool;
$_[1]     };
$_[1] }";
}
sub not_after {
    warn '<!after ...> not implemented';
    return;
}
sub colon {
    my $str = $_[0];
    return "$_[1] # : no-op\n"
        if $str eq ':';
    return "$_[1] ( \$pos >= length( \$s ) ) \n" 
        if $str eq '$';
    return "$_[1] ( \$pos == 0 ) \n" 
        if $str eq '^';
    die "'$str' not implemented";
}
sub constant {
    call_constant( @_ );
}

use vars qw( %char_class );
BEGIN {
    %char_class = map { $_ => 1 } qw( 
alpha
alnum
ascii
blank
cntrl
digit
graph
lower
print
punct
space
upper
word
xdigit
);
}

sub metasyntax {
    # <cmd>
    my $cmd = $_[0];   
    my $prefix = substr( $cmd, 0, 1 );
    if ( $prefix eq '@' ) {
        # XXX - wrap @array items - see end of Pugs::Grammar::Rule
        # TODO - param list
        return 
            "$_[1] do {\n" . 
            "$_[1]    my \$match;\n" . 
            "$_[1]    for my \$subrule ( $cmd ) {\n" . 
            "$_[1]        \$match = " . 
                call_subrule( '$subrule', '', () ) . ";\n" .
            "$_[1]        last if \$match;\n" . 
            "$_[1]    }\n" .
            "$_[1]    my \$bool = (!\$match != 1);\n" . 
            "$_[1]    \$pos = \$match->to if \$bool;\n" . 
            "$_[1]    \$bool;\n" . 
            "$_[1] }";
    }
    if ( $prefix eq '$' ) {
        if ( $cmd =~ /::/ ) {
            # call method in fully qualified $package::var
            # ...->match( $rule, $str, $grammar, $flags, $state )  
            # TODO - send $pos to subrule
            return 
                "$_[1]         do {\n" .
                "$_[1]           push \@match,\n" . 
                "$_[1]             $cmd->match( \$s, \$grammar, {p => \$pos}, undef );\n" .
                "$_[1]           \$pos = \$match[-1]->to;\n" .
                "$_[1]           !\$match[-1] != 1;\n" .
                "$_[1]         }"
        }
        # call method in lexical $var
        # TODO - send $pos to subrule
        return 
                "$_[1]         do {\n" .
                "$_[1]           my \$r = Pugs::Runtime::Rule::get_variable( '$cmd' );\n" . 
                "$_[1]           push \@match,\n" . 
                "$_[1]             \$r->match( \$s, \$grammar, {p => \$pos}, undef );\n" .
                "$_[1]           \$pos = \$match[-1]->to;\n" .
                "$_[1]           !\$match[-1] != 1;\n" .
                "$_[1]         }"
    }
    if ( $prefix eq q(') ) {   # single quoted literal ' 
        $cmd = substr( $cmd, 1, -1 );
        return call_constant( $cmd, $_[1] );
    }
    if ( $prefix eq q(") ) {   # interpolated literal "
        $cmd = substr( $cmd, 1, -1 );
        warn "<\"...\"> not implemented";
        return;
    }
    if ( $prefix =~ /[-+[]/ ) {   # character class 
        if ( $prefix eq '-' ) {
               $cmd = '[^' . substr($cmd, 2);
        } 
        elsif ( $prefix eq '+' ) {
               $cmd = substr($cmd, 2);
        }
        # XXX <[^a]> means [\^a] instead of [^a] in perl5re
        $cmd =~ s/\.\./-/g;    # <[1..4]> -> [1-4]
        return call_perl5($cmd, $_[1]);
    }
    if ( $prefix eq '?' ) {   # non_capturing_subrule / code assertion
        $cmd = substr( $cmd, 1 );
        if ( $cmd =~ /^{/ ) {
            warn "code assertion not implemented";
            return;
        }
        return
            "$_[1] do { my \$match =\n" .
            call_subrule( $cmd, $_[1] . "          " ) . ";\n" .
            "$_[1]      my \$bool = (!\$match != 1);\n" .
            "$_[1]      \$pos = \$match->to if \$bool;\n" .
            "$_[1]      \$bool;\n" .
            "$_[1] }";
    }
    # if ( $prefix eq '!' ) {   # negated_subrule / code assertion 
    #    $cmd = substr( $cmd, 1 );
    #    if ( $cmd =~ /^{/ ) {
    #        warn "code assertion not implemented";
    #        return;
    #    }
    #    return 
    #        "$_[1] ... negate( '$_[0]', \n" .
    #        call_subrule( $_[0], $_[1]."  " ) .
    #        "$_[1] )\n";
    # }
    if ( $cmd eq '.' ) {
            warn "<$cmd> not implemented";
            return;
    }
    if ( $prefix =~ /[_[:alnum:]]/ ) {  
        # "before" and "after" are handled in a separate rule
        if ( $cmd eq 'cut' ) {
            warn "<$cmd> not implemented";
            return;
        }
        if ( $cmd eq 'commit' ) {
            warn "<$cmd> not implemented";
            return;
        }
        if ( $cmd eq 'prior' ) {
            warn "<$cmd> not implemented";
            return;
        }
        if ( $cmd eq 'null' ) {
            warn "<$cmd> not implemented";
            return;
        }
        if ( exists $char_class{$cmd} ) {
            # XXX - inlined char classes are not inheritable, but this should be ok
            return
                "$_[1] ( ( substr( \$s, \$pos, 1 ) =~ /[[:$cmd:]]/ ) 
$_[1]     ? do { $direction$direction\$pos; 1 }
$_[1]     : 0
$_[1] )";
        }
        # capturing subrule
        # <subrule ( param, param ) >
        my ( $subrule, $param_list ) = split( /[\(\)]/, $cmd );
        $param_list = '' unless defined $param_list;
        my @param = split( ',', $param_list );
        # TODO - send $pos to subrule
        return named_capture(
            { ident => $subrule, 
              rule => 
                "$_[1]         do {\n" . 
                "$_[1]           push \@match,\n" . 
                    call_subrule( $subrule, $_[1]."        ", @param ) . ";\n" .
                "$_[1]           my \$bool = (!\$match[-1] != 1);\n" .
                "$_[1]           \$pos = \$match[-1]->to if \$bool;\n" .
                #"print !\$match[-1], ' ', Dump \$match[-1];\n" .
                "$_[1]           \$bool;\n" .
                "$_[1]         }",
              flat => 1
            }, 
            $_[1],    
        );
    }
    die "<$cmd> not implemented";
}

1;
