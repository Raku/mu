use v6;

module HTTP::Headers::Util-0.0.1;

sub split_header_words (*@values) returns Str is export {
    my @return;
    
    for @values -> $value {
        my @current;
        
        while ($value.chars) {
            if ($value ~~ s:P5/^\s*(=*[^\s=;,]+)//) {
                push @current, $0;  # 'token' or parameter 'attribute'
                
                # a quoted value
                if ($value ~~ s:P5/^\s*=\s*\"([^\"\\]*(?:\\.[^\"\\]*)*)\"//) {
                    my $val = $0;
                    $val ~~ s:P5:g/\\(.)/$0/;
                    push @current, $val
                # some unquoted value
                } elsif ($value ~~ s:P5/^\s*=\s*([^;,\s]*)//) {
                    my $val = $0;
                    $val ~~ s:P5/\s+$//;
                    push @current, $val;
                # no value, a lone token
                } else {
                    push @current, undef;
                }
            } elsif ($value ~~ s:P5/^\s*,//) {
                push @return, @current if @current;
                @current = ();
            } elsif ($value ~~ s:P5/^\s*;// || $value ~~ s:P5/^\s+//) {
                # continue
            } else {
                die "This should not happen: '$_'";
            }
        }
        
        push @return, @current if @current;
    }
    
    return @return;
}

multi sub join_header_words(*%words is copy) {
    my @return;
    
    for %words.kv -> $key, $value {
        if ($value.defined) {
            if ($value ~~ rx:P5/[\x00-\x20()<>@,;:\\\"\/\[\]?={}\x7F-\xFF]/) || (!$v.chars) {
                $value ~~ s:P5/([\"\\])/\\$1/; # escape " and \
                $key ~= qq(="$value");
            } else {
                # token
                $key ~= qq(=$value);
            }
        }
        
        push @return, $key;
    }
    
    return @return.join("; ");
}
