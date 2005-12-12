=pod

PIL2-JSON Parrot-like code emitter
by fglock

  ../../pugs -CPIL2-JSON -e ' say "hello "; say 1 + 1 ' | \
    ../../pugs pil2_json_emit_pir.p6

  #! /usr/bin/parrot
  ... TODO
  
=cut

use v6;

# -- Language specific

my $id = "V000";

sub emit_Main ( $global, $main ) {
    "#! /usr/bin/parrot\n" ~
    $global ~ 
    ".sub \"main\" @ANON\n" ~
    $main ~
    ".end\n"
}
sub emit_Stmt ( $s ) { $s }
sub emit_Code ( $body, $is_multi, $lvalue, @params, $type ) {
    "    # ??? - block \{ \n" ~ 
    $body ~ 
    "    # end block"
}	
sub emit_Assign( $to, $from ) { $from ~ ' ' ~ $to ~ " # ??? assign =\n" }
sub emit_Bind( $to, $from )   { $from ~ ' ' ~ $to ~ " # ??? bind :=\n" } 
sub emit_Sub ( $name, $body, $is_multi, $lvalue, @params, $type ) {
    if @params.elems > 1 {
        for 0 .. @params.elems-2 -> $i {
            # if there are many invocants, separate them with ',' instead of ':'
            if @params[$i+1] ~~ m:perl5{:$} {
                @params[$i] ~~ s:perl5{:$}{,};
            }
        }
    }
    my $param_list = @params.join(" ");
    $param_list ~~ s:perl5{,$}{};  # remove last ','
    
    ".sub $name\n" ~
    "    # TODO - param list (" ~ $param_list ~ ")\n" ~ 
    $body ~ 
    ".end\n"
}	
sub emit_App ( $function, @args, $context, $invocant ) {   
    my $p_fun = $id++;
    my $ret =
    "    .local pmc $p_fun\n" ~
    "    $p_fun = new .PerlUndef\n" ~
    "    $p_fun = find_name $function\n";

    my @p_args;
    my @s1;
    my @s2;
    for @args -> $val {
        my $p_arg = $id++;
        push @p_args, $p_arg;
        $ret ~=
        "    .local pmc $p_arg\n" ~
        "    $p_arg = new .PerlUndef\n" ~
        "    $p_arg = assign $val\n";
        push @s1, '16';
        push @s2, $p_arg;
    };
    my $p_ret = $id++;
    $ret ~= 
    "    .local pmc $p_ret\n" ~ 
    "    $p_ret = new .PerlUndef\n" ~
    "    set_args '(" ~ @s1.join(', ') ~ ")', " ~ @s2.join(', ') ~ "\n" ~
    "    get_results \"(0)\", $p_ret\n" ~
    "    invokecc $p_fun\n";
}
sub emit_Pad ( $scope, @symbols, $statements ) { 
    "    # TODO - _start_pad \n" ~ 
    @symbols.map:{ 
        emit_Variable($_) ~ " my    # ???\n" 
    } ~ 
    "$statements\n" ~ 
    "    # TODO - _end_pad\n";
}
sub emit_Variable ( $s is copy ) {
    return $s;
  
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
sub emit_Rat ( $a, $b ) { "$a $b / " } 
sub emit_parameter( 
    $name, $is_invocant, $is_lvalue, $is_lazy, $is_named,
    $is_optional, $is_writable, $context, $default )
{
    #say "(param=$name, $is_invocant, $is_lvalue, $is_lazy, $is_named, ",
    #    "$is_optional, $is_writable, <", $context.perl, ">, <$default>)";
        
    # ??? - "paramDefault" 
    # ??? - what is the syntax for $is_lvalue 
    # ??? - what is the PIL for 'is copy' 

    # $context = [["CxtSlurpy", [[["MkType", ["Int"] ]]] ]]
    my $is_slurpy = $context[0][0] eq '"CxtSlurpy"';
    my $type = emit_Variable( $context[0][1][0][0][1] );
    
    my $s;
    $s ~= $type eq 'main' ?? '' !! ($type ~ ' ');
    $s ~= $is_slurpy ?? '*' !! '';

    $s ~= $is_named    eq 'true'        ?? ':' !! ''; 
    $s ~= emit_Variable( $name );
    $s ~= $is_optional eq 'true'        ?? '?' !! '';   # '!' is default
    $s ~= ' is rw '   if $is_writable eq 'true';
    $s ~= ' is lazy ' if $is_lazy     eq 'true';
    $s ~= $is_invocant eq 'true'        ?? ':' !! ',';   
    return $s; 
}  
sub emit_parameter_with_default( $param, $default ) {
    return $param if $default eq '';
    # rewrite '$name,' to '$name = default,'
    my ($name, $separator) = $param ~~ m:perl5{(.*)(.)};
    
    $default ~ 
    $name ~ 
    ' = ' ~ 
    $separator
}

# -- Main program
# this is the same for all languages

push @*INC, './';
require 'pil2_json_emit.pm';

# slurp stdin - xinming++ 
my $pil2 = ~$*IN.slurp;

my @b = tokenize( $pil2 );
# say "Tokens: ", @b.join('><');

my $ast = parse( << { >>, 'hash', << } >>, @b );
# say $ast.perl;

my $program = traverse_ast( $ast );
say $program;

