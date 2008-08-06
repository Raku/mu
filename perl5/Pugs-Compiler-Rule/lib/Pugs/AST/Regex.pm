package Pugs::AST::Regex;

use strict;
use warnings;
use Data::Dumper;

# Optimizations:
# a[b] => ab       # simplify unary alternation or concat
# ac|ab => a[c|b]  # "tries"; there are different rules for "|" and "||"

# TODO:
# quantifiers
# & 
# character classes

sub optimize {
    my $n = $_[0];
    my $changed;
    for ( 0..3 ) {
        $changed = 0;
        if ( exists $n->{concat} ) {
            my $opt = $n->{concat};
            for (my $i = 0; $i <= $#$opt; $i++) {
                if ( exists $opt->[$i]{concat} ) 
                {
                    splice( @$opt, $i, 1, @{$opt->[$i]{concat}} );
                    $changed = 1;
                    redo;
                }
            }
            $changed ||= optimize($_) for @$opt;
            if ( @$opt == 1 ) {
                delete $n->{concat};
                %$n = %{$opt->[0]};
                $changed = 1;
            }
        }
        if ( exists $n->{alt1} ) {
            my $opt = $n->{alt1};
            for (my $i = 0; $i <= $#$opt; $i++) {
                if ( exists $opt->[$i]{alt1} ) 
                {
                    splice( @$opt, $i, 1, @{$opt->[$i]{alt1}} );
                    $changed = 1;
                    redo;
                }
            }
            for (my $i = 0; $i < $#$opt; $i++) {
                for (my $j = $i + 1; $j <= $#$opt; $j++) {
                    last if $j > $#$opt;
                    if ( fold_constant_in_list( 'alt1', $opt, $i, $j ) ) {
                        $changed = 1;
                        redo;
                    }
                }
            }
            $changed ||= optimize($_) for @$opt;
            if ( @$opt == 1 ) {
                delete $n->{alt1};
                %$n = %{$opt->[0]};
                $changed = 1;
            }
        }
        if ( exists $n->{alt} ) {
            my $opt = $n->{alt};
            for (my $i = 0; $i <= $#$opt; $i++) {
                if ( exists $opt->[$i]{alt} ) 
                {
                    splice( @$opt, $i, 1, @{$opt->[$i]{alt}} );
                    $changed = 1;
                    redo;
                }
            }
            for (my $i = 0; $i < $#$opt; $i++) {
                last if $i >= $#$opt;
                if ( fold_constant_in_list( 'alt', $opt, $i, $i + 1 ) ) {
                        $changed = 1;
                        redo;
                }
            }
            $changed ||= optimize($_) for @$opt;
            if ( @$opt == 1 ) {
                delete $n->{alt};
                %$n = %{$opt->[0]};
                $changed = 1;
            }
        }
        # last unless $changed;
    }
    return $changed;
}

sub fold_constant_in_list {
    my ($node_name, $list, $i1, $i2) = @_;
    # constant1, constant2 => constant1
    if (   exists $list->[$i1]{constant} 
        && exists $list->[$i2]{constant}
        && $list->[$i1]{constant} eq $list->[$i2]{constant} 
       ) 
    {
        splice( @$list, $i2, 1 );
        return 1;
    }
    # constant1, concat(constant1,...) => concat(constant1,alt(null,...))
    if (   exists $list->[$i1]{constant} 
        && exists $list->[$i2]{concat}
        && exists $list->[$i2]{concat}[0]{constant}
        && $list->[$i1]{constant} eq $list->[$i2]{concat}[0]{constant} 
       ) 
    {
        my @tail = splice( @{$list->[$i2]{concat}}, 1 );
        my $tail = @tail == 1 ? $tail[0] : { concat => \@tail };
        $list->[$i2]{concat}[1] = 
            {
                $node_name => [
                    { 'metasyntax' => {
                            'metasyntax' => 'null',
                            'modifier' => ''
                        }
                    },
                    $tail,
                ],
            };
        splice( @$list, $i1, 1 );
        return 1;
    }
    # concat(constant1,...), constant1 => concat(constant1,alt(...,null))
    if (   exists $list->[$i2]{constant} 
        && exists $list->[$i1]{concat}
        && exists $list->[$i1]{concat}[0]{constant}
        && $list->[$i2]{constant} eq $list->[$i1]{concat}[0]{constant} 
       ) 
    {
        my @tail = splice( @{$list->[$i1]{concat}}, 1 );
        my $tail = @tail == 1 ? $tail[0] : { concat => \@tail };
        $list->[$i1]{concat}[1] = 
            {
                $node_name => [
                    $tail,
                    { 'metasyntax' => {
                            'metasyntax' => 'null',
                            'modifier' => ''
                        }
                    },
                ],
            };
        splice( @$list, $i2, 1 );
        return 1;
    }
    # concat(constant1,...), concat(constant1,...) => concat(constant1,alt(...,...))
    if (   exists $list->[$i1]{concat}
        && exists $list->[$i1]{concat}[0]{constant}
        && exists $list->[$i2]{concat}
        && exists $list->[$i2]{concat}[0]{constant}
        && $list->[$i1]{concat}[0]{constant} eq $list->[$i2]{concat}[0]{constant} 
       ) 
    {
        my @tail1 = splice( @{$list->[$i1]{concat}}, 1 );
        my $tail1 = @tail1 == 1 ? $tail1[0] : { concat => \@tail1 };
        my @tail2 = splice( @{$list->[$i2]{concat}}, 1 );
        my $tail2 = @tail2 == 1 ? $tail2[0] : { concat => \@tail2 };
        $list->[$i1]{concat}[1] = 
            {
                $node_name => [
                    $tail1,
                    $tail2
                ],
            };
        splice( @$list, $i2, 1 );
        return 1;
    }
    return;
}

1;
