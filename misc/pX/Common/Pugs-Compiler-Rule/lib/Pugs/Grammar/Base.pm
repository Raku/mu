package Pugs::Grammar::Base;

# defines <ws>, unicode character classes, etc

sub ws {
    my $class = shift;
    return unless $_[0];
    return { 
        bool  => 1,
        match => $1,
        tail  => $2,
    }
        if $_[0] =~ /^(\s+)(.*)$/s;
    return;
};

1;

