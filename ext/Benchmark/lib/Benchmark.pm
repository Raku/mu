use v6;
module Benchmark:ver<0.1>;

sub timeit ($count, $code is copy) is export {
    $code = eval "sub \{ $code \}" unless $code.isa("Code");
    # TODO: handle count too low case.
    return _time_func( _loop_func($count, $code) )
           »-« # overhead
           _time_func( _loop_func($count, -> {}) );
}

sub _loop_func ($count, $code) {
    return sub {
        for (1..$count) {
            $code.();
        }
    }
}

sub _time_func ($loop) {
    my @s = (time, times);
    $loop.();
    return (time, times) »-« @s;
}

sub timethese ($count, %hash) is export {
    for %hash.kv -> $name, $code {
        my @time = timeit($count, $code);
        say "$name: { @time[0] != 0 ?? int($count / @time[0]) !! "Inf" }/s";
    }
}

=begin pod

=begin NAME
Benchmark - Benchmark running time of Perl 6 code
=end NAME

=begin SYNOPSIS
    use v6;
    use Benchmark;

    @t = timeit($count, 'code');
    @t = timeit($count, { 1 + 1 });

    timethese($count, { hyper => { my @r = @a >>+<< @b },
                        normal => {
                          my @r = gather {
                            take(@a[$_] + @b[$_]) for 0..@a.end;
                          }
                         }
                       });
=end SYNOPSIS

=head1 TODO

many

=begin AUTHOR
Chia-liang Kao, L<clkao@clkao.org|mailto:clkao@clkao.org>
=end AUTHOR

=begin COPYRIGHT
Copyright (c) 2005. Chia-liang Kao. All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>
=end COPYRIGHT

=end pod
