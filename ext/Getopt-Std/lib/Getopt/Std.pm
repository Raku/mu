use v6;

class Getopt::Std-0.01;

sub getopts (Str $spec, @args? is rw = @*ARGS) is export {
    my %spec = hashify($spec);
    my %opts;
    my $cur;
    while $cur = @args.shift {
        given $cur {
            when '--' { last }
            when rx:P5/-(.)(.*)/ {
                if ! defined %spec{$0} {
                    warn "unrecognized option: $0";
                    if $1.chars { unshift @args, "-$1" }
                }
                elsif ! %spec{$0} {
                    %opts{$0} = 1;
                    if $1.chars { unshift @args, "-$1" }
                } else {
                    if $1.chars {
                        %opts{$0} = ~$1;
                    } else {
                        %opts{$0} = @args.shift err warn "missing option for -$0";
                    }
                }
            }
        }
    }
    return %opts;
}


sub hashify (Str $spec is copy) returns Hash {
    my @with_args = $spec ~~ rx:P5:g/([^:]):/;
    my @without_args = $spec ~~ rx:P5:g/([^:])(?!:)/;
    #@with_args.perl.say;
    (map { ;~$_ => 0 }, @without_args), map { ;~$_ => 1 }, @with_args;
}
