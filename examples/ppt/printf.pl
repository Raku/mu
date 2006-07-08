use v6-alpha;

if !@*ARGS {
    die "usage: $*PROGRAM_NAME format [argument ...]\n";
}

print sprintf @*ARGS.shift, @*ARGS;
$*OUT.flush;
