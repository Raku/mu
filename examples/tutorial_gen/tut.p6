use v6;
use File::Spec;

# todo_
# all :perl5

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
#my $conf = hash( $conf_raw );

#       { src => './tut_src/hello-world.p6',    dest_dir => 'base',  },
#       { src => './tut_src/hello-world-ad.p6', dest_dir => 'base',  },
#       { src => './tut_src/',                  dest_dir => 'tut',   },


my $conf = {
    index => [
      # dir tut-src /B
      <hello-world.p6 hello-world-ad.p6>
    ],
    tut_src_dir => './tut-src',
    pugs => 'pugs',
    #pugs => './../../pugs',
    output_dir => './tut-output',

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

say $conf.ref ~ ' ' ~ $conf if $dg;
#say $conf.ref; say $conf.<index>.ref;

my $os;
$os = 'win32' if lc($?OS) eq any<mswin32 mingw>;
say 'os: ' ~ $os if $dg;
my $os;

say ~$path;
for $conf.keys -> $key {
    die "Conf keys 'f_*' are reserved!\n" if $key ~~ rx:perl5{^f_};
    if ( $key ~~ rx:perl5{_fp$} ) {
        $conf{'f_'  ~ $key} = catfile( $path, $conf{$key} );
    } elsif  ( $key ~~ rx:perl5{_dir$} ) {
        $conf{'f_' ~ $key} = catdir( $path, $conf{$key} );
    } else {
        # todo_
        #next;
    }
    say "$key  : $conf{$key}" if $dg;
    # todo_ if next
    say "f_$key: $conf{'f_' ~ $key}" if $dg;
}
say '';

my $pugs = $conf<pugs>;
$pugs = catfile( $path, $pugs ) if $pugs ~~ rx:perl5{^[\.\\\/]};
if ( $os eq 'win32' ) {
    $pugs ~~ s:perl5:g{\/}{\\};
    $pugs ~= '.exe' unless $pugs ~~ rx:perl5{\.exe$};
}
say '--- pugs: ' ~ $pugs ~ "\n" if $dg;
my $stat = system( "$pugs -v" );
# TODO
#die "Pugs '$pugs' run test failed (code $stat)!\n" if $stat != 0;

my $out_dir = $conf<f_output_dir>;
unless -d $out_dir {
    mkdir $out_dir or die "Output dir '$out_dir' create error:\n";
}

my $out_suffix;
if defined $conf<output_suffix> {
    $out_suffix = $conf<output_suffix>;
} elsif $conf<output_type> eq 'html' {
    $out_suffix = '.html';
} else {
    $out_suffix = '.txt';
}

if $conf<output_type> eq 'html' {
    unless defined $conf<tut_src_rel> {
        die "File::Spec bug";
        $conf<tut_src_rel> = abs2rel( $conf<f_tut_src_dir>, $conf<f_output_dir> );
    }
    $conf<tut_src_rel> ~~ s:perl5:g{\\}{\/};
}

die "Source file directory '$conf<tut_src_dir> ('$conf<f_tut_src_dir>') not found!" 
    unless -d $conf<f_tut_src_dir>;


sub get_output ( Str $tut_fp, +$each_semicolon = 0 ) {
    my $file_t = slurp $tut_fp || die "Slurp failed '$tut_fp'\n";
    my @parts = split "\n\n", $file_t;
    #TODO one-line
    #my @parts = split "\n", $file_t;
#    say $file_t;
#    say ~@parts;

    my $new_pl = '';
    for @parts.kv -> $part_num, $part {
        $new_pl ~= $part ~ "\n";
        # TODO
        #unless $part ~~ rx:perl5{^\s*$} {
            $new_pl ~= 'print "#~# ' ~ $part_num ~ ' #~#\n";';
            # todo_ waiting for io_redirect_to_scalar
            # $new_pl ~= '$*ERR.print("#!# ' ~ $part_num ~ ' #!#\n");';
            $new_pl ~= "\n";
        #}
    }
    say $new_pl;
    
    #my $out, $err;
    #open $*OUT,">", \$out;
    #open $*OUT,">", \$err;
    #my $status = eval $new_pl;   
    #say $status;
    
    my $fh_p6_temp = open '>' ~ $conf<f_temp_fp>;
    print $fh_p6_temp, $new_pl;
    close $fh_p6_temp;
    
    my $cmd = "$pugs $conf<f_temp_fp> > $conf<f_temp_out_fp>";
    say "running: '$cmd'\n";
    my $status = system $cmd;
    my $out = slurp $conf<f_temp_out_fp>;
    unlink $conf<f_temp_out_fp>;
    
    say ~ '-' x 60 ~ " out b --\n" ~ $out ~ "\n" ~ '-' x 60 ~ ' out e --' if $dg;

  # my @out_parts = split( rx:perl5{#~# \d+ #~#}, $out);
    my @out_parts = split( rx:perl5{#~# \d+ #~}, $out);
    
    # todo_
    #for @out_parts -> $out_part is rw {
    #  $out_part = undef; ...
    for @out_parts.keys -> $idx {
        my $out_part = @out_parts[$idx];
        if $out_part ~~ rx:perl5{^#\s*$} {
            @out_parts[$idx] = undef;
        } else {
            @out_parts[$idx] ~~ s:perl5{^#\n}{};
            #TODO one-line
            #$out_part ~~ s:perl5{\n$}{};
            
        }
    }
    return \@parts, \@out_parts;
}
    

sub gen_html ( 
    @parts, @out_parts, 
    Str $prev_tut_fn, Str $tut_fn, Str $next_tut_fn, 
    Str $out_dir, Str +$suffix 
) {
    use HTML::Entities;

    my ( $part, $out_part );
    say ~@out_parts;
 
    my $html_fp = catfile( $out_dir, $tut_fn ~ $suffix );
    my $fh_html = open '>' ~ $html_fp;

    # ===== html =====>>
    say $fh_html,
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
    <table id="test_out" cellspacing=0 cellpadding=0 border=0>|; # <<=====
 
    my $out_part;
    for @parts.kv -> $part_num, $part {
        $out_part = @out_parts[$part_num];
        say ~ '-' x 20 ~ " $part_num in_part -----\n" ~ $part ~ "\n" ~ '-' x 20 ~ " $part_num out_part ----\n"  ~ '-' x 20 ~ "\n" ~ $out_part ~ "\n" ~ '-' x 20 ~ ' $part_num out_part e --' if $dg;

        # ===== html =====>>
        say $fh_html, qq|
        <tr>
          <td class="src"><pre>{encode_entities $part}</pre></td>
        { 
            if $out_part { 
                qq|<td><div class="out"><pre>{encode_entities $out_part}</pre></div></td>| 
            } else { 
                qq|<td class="empty"></td>|; 
            }
        }
        </tr>|; # <<=====        
    }

    # TODO s{./../tut}{abs2rel}
    # ===== html =====>>
    say $fh_html, qq|
    </table>
    <table id="help" cellspacing=0 cellpadding=0 border=0>
      <tr>
        <td><a href="{$conf<tut_src_rel>}/{$tut_fn}">{$tut_fn} src</a> \&nbsp; \&nbsp; <a href="{$conf<tut_src_rel>}/">src dir</a><td>     
      </tr>
    </table></body></html>
|; # <<=====
    close $fh_html;
}


say $conf<index>.ref;
my @prep_index = *$conf<index>;
# say @prep_index.ref; say join ' - ', @prep_index[0], @prep_index[1];

#for $conf<index>.kv -> 

#if $conf<find_others> { 
#    # todo_ maybe
#    # my $index_h = hash( zip( $index, [1..Inf] ) );
#    my $index_h;
#    for @index -> $key {
#        $index_h{$key} = 1;
#    }

# TODO
#    require File::Find::Rule;
#    my @files = File::Find::Rule::find( name => [ '*.p6' ], in => $conf<f_tut_src_dir> ); 
#    foreach my $fn ( @files ) {
#        $fn = abs2rel( $fn, $conf<f_tut_src_dir> );
#        next if $index_h{$fn};
#        push @index, $fn;
#    }
#}

my ( $tut_fp, $tt_vars );
my ( $tut_fn, $prev_tut_fn, $next_tut_fn );
# todo_ autrijus "zip() is in." 
for @prep_index.kv -> $idx, $tut_fn {
    if ( $idx > 0 ) { $prev_tut_fn = @prep_index[$idx-1] } else { $prev_tut_fn = undef };
    if ( $idx + 1 < @prep_index.elems ) { $next_tut_fn = @prep_index[$idx+1] } else { $next_tut_fn = undef };

    say "p:'$prev_tut_fn'  a:'$tut_fn'  n:'$next_tut_fn'";
    
    $tut_fp = catfile( $conf<f_tut_src_dir>, $tut_fn );
    slurp $tut_fp || die "$tut_fp";
    say $tut_fp;

    my ( @parts, @out_parts );
    # todo_
    { my $r = get_output( $tut_fp ); @parts = $r[0], @out_parts = $r[1] }
    say "parts: {+@parts}, out_parts: {+@out_parts}" if $dg;
    
    gen_html( 
        @parts, @out_parts, 
        $prev_tut_fn, $tut_fn, $next_tut_fn, 
        $out_dir, suffix => $out_suffix 
    );
}
