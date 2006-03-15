package Pugs::Grammar::Base;

# defines <ws>, etc

sub any { 
    my $class = shift;
    return unless $_[0];
    return { 
        bool  => 1,
        match => { '.'=> substr($_[0],0,1) },
        tail  => substr($_[0],1),
        ( $_[2]->{capture} ? ( capture => [ substr($_[0],0,1) ] ) : () 
),
    };
}

sub ws {
    my $class = shift;
    return unless $_[0];
    return { 
        bool  => 1,
        match => { 'ws'=> $1 },
        tail  => $2,
        ( $_[2]->{capture} ? ( capture => [ $1 ] ) : () ),
    }
        if $_[0] =~ /^(\s+)(.*)$/s;
    return;
};

1;

