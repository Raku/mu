module CGI::Util-0.0.1;

use v6;

sub make_attribute(%attrs, Bool ?$escape = 0) is export {
    my @return;
    
    for %attrs.keys -> $key {
        my $copy = lc $key;
        
        $copy ~~ s:P5:g/_/-/;
        
        my $value = ($escape) ?? simple_escape(%attrs{$key}) !! %attrs{$key};
        
        push @return, (%attrs{$key}.defined) ?? qq/$copy="$value"/ !! qq/$copy/;
    }
    
    return @return;
}

sub simple_escape (Str $string is copy) returns Str {
    $string ~~ s:P5:g/&/&amp;/;
    $string ~~ s:P5:g/</&lt;/;
    $string ~~ s:P5:g/>/&gt;/;
    $string ~~ s:P5:g/"/&quot;/;
    
    return $string;
}

1;
