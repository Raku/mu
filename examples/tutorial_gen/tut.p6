use v6;
use File::Spec;
use HTML::Entities;

# todo_
# all :perl5
# subs and each_line => 1

my $prog_name = $*PROGRAM_NAME;
$prog_name ~~ s:perl5:g{\\}{\/};
my ( $path ) = splitpath( $prog_name )[1];
$path ||= '.';

my $conf_fp;
my $dg = 1;

if ( @ARGS[0] ) {
    $conf_fp = @ARGS[0];
    $path = splitpath( $conf_fp )[1];
} else {    
    $conf_fp = "$path/conf.p6";
}
say ~$path if $dg;

# todo_ require problem
#my $conf_raw = require $conf_fp || die "Conf file '$conf_fp' not loaded;";
#my %conf = hash( $conf_raw );


my %conf = {
    index => [
#       { src => './tut_src/hello-world.p6',    dest_dir => 'base',  },
#       { src => './tut_src/hello-world-ad.p6', dest_dir => 'base',  },
#       { src => './tut_src/',                  dest_dir => 'tut',   },
      <hello-world.p6 hello-world-ad.p6>
    ],
    add_others=> 1,

    tut_src_dir => './tut-src',
    pugs => 'pugs',
    #pugs => './../../pugs',
    output_dir => './tut-output',
    
    each_line => 0,

    # temprorary files
    temp_fp => './tut-temp.p6',      
    temp_out_fp => './tut-temp.out',

    # html
    output_type => 'html',
    output_suffix => '.html',
    tut_src_rel => './../tut-src',
    
    # TODO
    # raw    
    # output_type   => 'raw',
    # output_suffix => '.txt',
    
};

say %conf.ref ~ ' ' ~ %conf if $dg;
#say %conf.ref; say %conf.<index>.ref;

my $os;
$os = 'win32' if lc($?OS) eq any<mswin32 mingw>;
say 'os: ' ~ $os if $dg;

for %conf.keys -> $key {
    die "Conf keys 'f_*' are reserved!\n" if $key ~~ rx:perl5{^f_};
    if ( $key ~~ rx:perl5{_fp$} ) {
        %conf{'f_'  ~ $key} = catfile( $path, %conf{$key} );
    } elsif  ( $key ~~ rx:perl5{_dir$} ) {
        %conf{'f_' ~ $key} = catdir( $path, %conf{$key} );
    } else {
        # todo_
        #next;
    }
    say "$key  : %conf{$key}" if $dg;
    # todo_ if next
    say "f_$key: %conf{'f_' ~ $key}" if $dg;
}
say '' if $dg;

my $pugs = %conf<pugs>;
$pugs = catfile( $path, $pugs ) if $pugs ~~ rx:perl5{^[\.\\\/]};
if ( $os eq 'win32' ) {
    $pugs ~~ s:perl5:g{\/}{\\};
    $pugs ~= '.exe' unless $pugs ~~ rx:perl5{\.exe$};
}
say 'pugs: ' ~ $pugs ~ "\n" if $dg;
my $stat = system( "$pugs -v" );
# correct for unix?
# die "Pugs '$pugs' run test failed (code $stat)!\n" unless $stat;

my $out_dir = %conf<f_output_dir>;
unless -d $out_dir {
    mkdir $out_dir or die "Output dir '$out_dir' create error:\n";
}

my $out_suffix;
if defined %conf<output_suffix> {
    $out_suffix = %conf<output_suffix>;
} elsif %conf<output_type> eq 'html' {
    $out_suffix = '.html';
} else {
    $out_suffix = '.txt';
}

if %conf<output_type> eq 'html' {
    unless defined %conf<tut_src_rel> {
        die "File::Spec bug";
        %conf<tut_src_rel> = abs2rel( %conf<f_tut_src_dir>, %conf<f_output_dir> );
    }
    say "\%conf<tut_src_rel>: %conf<tut_src_rel>" if $dg;
}

die "Source file directory '%conf<tut_src_dir> ('%conf<f_tut_src_dir>') not found!" 
    unless -d %conf<f_tut_src_dir>;


sub get_output ( Str $tut_fp, :$each_line = 0 ) {
    my $file_t = slurp $tut_fp or die "Slurp failed '$tut_fp'\n";
    my @parts = ( $each_line ) ?? split( "\n", $file_t ) !! split( "\n\n", $file_t );

    my $new_pl = '';
    for @parts.kv -> $part_num, $part {
        $new_pl ~= $part ~ "\n";
        # todo_
        #unless $part ~~ rx:perl5{^\s*$} {
            $new_pl ~= 'print "#~# ' ~ $part_num ~ ' #~#\n";';
            # todo_ waiting for io_redirect_to_scalar
            # $new_pl ~= '$*ERR.print("#!# ' ~ $part_num ~ ' #!#\n");';
            $new_pl ~= "\n";
        #}
    }
    say "new_pl: $new_pl";
    
    # todo_
    # my ( $out, $err );
    #open $*OUT,">", \$out;
    #open $*OUT,">", \$err;
    #my $status = eval $new_pl;   
    #say $status;
    
    my $fh_p6_temp = open %conf<f_temp_fp> :w;
    $fh_p6_temp.print( $new_pl );
    close $fh_p6_temp;
    
    my $cmd = "$pugs %conf<f_temp_fp> > %conf<f_temp_out_fp>";
    say "running: '$cmd'\n";
    my $status = system $cmd;
    my $out = slurp %conf<f_temp_out_fp>;
    unlink %conf<f_temp_out_fp>;
    
    say ~ '-' x 60 ~ " out b --\n" ~ $out ~ "\n" ~ '-' x 60 ~ ' out e --' if $dg;

    my @out_parts = split( rx:perl5{#~# \d+ #~#\n}, $out);

    say @out_parts.perl;
    
    for @out_parts -> $out_part is rw {
        if $out_part ~~ rx:perl5{^\s*$} {
            $out_part = undef;
        } else {
            $out_part ~~ s:perl5{^\n}{};
            $out_part ~~ s:perl5{\n$}{} if $each_line;
            
        }
        say $out_part.perl if $dg;
    }

    say @parts.perl if $dg;
    say @out_parts.perl if $dg;
    return ( \@parts, \@out_parts );
}
    

sub gen_html ( 
    @parts, @out_parts, 
    Str $prev_tut_fn, Str $tut_fn, Str $next_tut_fn, 
    Str $out_dir, Str :$suffix 
) {
    my ( $part, $out_part );
    say ~@out_parts;
 
    my $html_fp = catfile( $out_dir, $tut_fn ~ $suffix );
    my $fh_html = open $html_fp :w;

    # ===== html =====>>
    $fh_html.say(
qq|<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML//EN">
<html>
  <head>
    <title>Perl6-Pugs Tutorial</title>
    <meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1" />
    <link rel="stylesheet" title="Default Display" media="screen" type="text/css" href="tut.css">
  </head>
  <body>
    <br>
    <table id="nav" cellspacing=0 cellpadding=0 border=0>
      <tr>
        <td class="link">
        { if $prev_tut_fn { qq|<a href="{$prev_tut_fn}{$suffix}">\&lt;\&lt;prev</a>| } }
        </td>
        <td class="name">{$tut_fn}</td>
        <td class="link">
        { if $next_tut_fn { qq|<a href="{$next_tut_fn}{$suffix}">next\&gt;\&gt</a>| } }
        </td>
      </tr>
    </table>
    <br><br>
    <table id="test_out" cellspacing=0 cellpadding=0 border=0>| ); # <<=====
 
    for zip( @parts.keys, @parts.values, @out_parts ) -> $part_num, $part, $out_part {
        if $dg {
            my $sep = '-' x 20;
            say "$sep $part_num in_part -----";
            say $part; 
            say "$sep $part_num out_part ----";
            say $out_part;
            say "$sep $part_num out_part e --";
        }

        my $rw_part_hack = $part;
        # ===== html =====>>
        $fh_html.say( qq|
        <tr>
          <td class="src"><pre>{encode_entities($rw_part_hack)}</pre></td>
        { 
# todo_
#            ( $out_part ) 
#                ?? qq|<td><div class="out"><pre>{ encode_entities $todo_c }</pre></div></td>|
#                :: qq|<td class="empty"></td>|
#            ; 
            if $out_part {
                qq|<td><div class="out"><pre>{ my $todo_c = $out_part; encode_entities($todo_c); }</pre></div></td>|;
            } else {
                qq|<td class="empty"></td>|;
            }
        }
        </tr>| ); # <<=====        
    }

    # TODO s{./../tut}{abs2rel}
    # ===== html =====>>
    $fh_html.say( qq|
    </table>
    <table id="help" cellspacing=0 cellpadding=0 border=0>
      <tr>
        <td><a href="{%conf<tut_src_rel>}/{$tut_fn}">{$tut_fn} src</a> \&nbsp; \&nbsp; <a href="{%conf<tut_src_rel>}/">src dir</a><td>     
      </tr>
    </table></body></html>
| ); # <<=====
    close $fh_html;
}


my @prep_index = *%conf<index>;
if %conf<add_others> { 
    # todo_ maybe
    # my %index{ @prep_index } >>= 1;
    my %index; for @prep_index -> $key { %index{$key} = 1; }
    # say %index.perl if $dg;
    
    my @ls = sort readdir %conf<f_tut_src_dir>;
    for @ls -> $each {
        # todo_
        #next unless -f $each;
        #next if exists %index{$fn};
        
        if ( ( $each ~~ rx:perl5{\.p6$} ) && ( -f catfile(%conf<f_tut_src_dir>, $each) ) && ( not %index{$each} ) ) {
            push @prep_index, $each;
        }
    }
}
say ~@prep_index if $dg;

my ( @parts, @out_parts );
my ( $tut_fp, $tt_vars );
my ( $prev_tut_fn, $next_tut_fn );
# TODO autrijus "zip() is in." 
for @prep_index.kv -> $idx, $tut_fn {
    if ( $idx > 0 ) { $prev_tut_fn = @prep_index[$idx-1] } else { $prev_tut_fn = undef };
    if ( $idx + 1 < @prep_index.elems ) { $next_tut_fn = @prep_index[$idx+1] } else { $next_tut_fn = undef };

    say "p:'$prev_tut_fn'  a:'$tut_fn'  n:'$next_tut_fn'";
    $tut_fp = catfile %conf<f_tut_src_dir>, $tut_fn;


    # todo_
    # my ( @parts, @out_parts ) = get_output( $tut_fp, each_line => %conf<each_line> );
    # my ( @parts, @out_parts );  # todo_ - uncomment and you will see for $idx >= 1
    { my ( @r ) = get_output( $tut_fp, each_line => %conf<each_line> ); @parts = *@r[0]; @out_parts = *@r[1] }

    say "parts: {+@parts}, out_parts: {+@out_parts}" if $dg;
    say 'parts: ' ~ @parts.perl if $dg;
    say 'out_parts: ' ~ @out_parts.perl if $dg;
    
    gen_html( 
        @parts, @out_parts, 
        $prev_tut_fn, $tut_fn, $next_tut_fn, 
        $out_dir, suffix => $out_suffix 
    );
}
