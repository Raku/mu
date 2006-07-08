=pod

PIL2-JSON Forth-like code emitter
by fglock

  ../../pugs -CPIL2-JSON -e ' say "hello "; say 1 + 1 ' | \
    ../../pugs pil2_json_emit_forth.p6

  #! /usr/bin/forth
  : &*END ;
  "hello " &say
  1 1 &infix:<+> &say

=cut

use v6;

# -- Language specific

sub emit_Main ( $global, $main ) {
    "#! /usr/bin/io\n" ~
    $global ~ 
    $main
}
sub emit_Stmt ( $s ) { $s ~ "\n" }
sub emit_Code ( $body, $is_multi, $lvalue, @params, $type ) {
    "    # ??? - block \{ \n" ~ 
    $body ~ 
    "    # end block"
}	
sub emit_Assign( $to, $from ) { $from ~ ' ' ~ $to ~ " =\n" }
sub emit_Bind( $to, $from )   { $from ~ ' ' ~ $to ~ " :=\n" } 
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
    
    "    # TODO - sub definition: $name\n" ~
    "    # TODO - param list (" ~ $param_list ~ ")\n" ~ 
    $body ~ 
    "\n"
}	
sub emit_App ( $function is rw, @args, $context, $invocant ) {   

    if $function eq '&say'     { return @args[].join(", "), " print " }
    if $function eq '&infix:+' { return @args[].join(" + "), " " }

    "$function (", @args[].join(", "), ")";
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
    $s ~~ s:perl5/^"(.*)"$/$0/;
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
my $pil2 = ** $*IN.slurp;

my @b = tokenize( $pil2 );
# say "Tokens: ", @b.join('><');

my $ast = parse( << { >>, 'hash', << } >>, @b );
# say $ast.perl;

my $program = traverse_ast( $ast );
say $program;

