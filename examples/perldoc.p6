use v6;

# naive version of perldoc implemented in and for perl6

my $VERSION = '0.01';

if (not defined %*ENV<HOME>) {
   die "Cannot work without a HOME directory"; 
}
#my $dir =  %*ENV<HOME> ~ ($*OS ~~ m:i/win/ ?? "/p6pod" !! "/.p6pod");
my $dir =  %*ENV<HOME> ~ "/.p6pod";

# is there an ARGS parser already Getopt::* ?
@*ARGS or usage();
if (@*ARGS[0] eq "--index") {
    index_pods();
} elsif (@*ARGS[0] eq "--keyword" and defined @*ARGS[1]) {
    lookup(@*ARGS[1]);
} elsif (@*ARGS[0] eq "--list") {
    list_pod_files();
} else {
    display_pod(@*ARGS[0]);
    #usage();
}

sub list_pod_files {
    # for now assume we run in the same directory where pugs is and the docs are in
    # ./docs/Perl6
    my $ROOT = dirname($PROGRAM_NAME) ~ "/..";
    my $dir = $ROOT ~ "/docs/Perl6";
    say "processing $dir tree";
    my $prefix_length = $dir.chars;
    my @podfiles;
    for list_files($dir, 1) -> $podfile {
        @podfiles.push(substr $podfile, $prefix_length+1, -4);
    }
    for 0..@podfiles -> $i {
        say "$i) @podfiles[$i]";
    }
    print "$ ";
    my $selection = =$*IN;
    say "selected '$selection'";
    display_pod("$dir/@podfiles[$selection].pod");
}

sub display_pod {
    my ($podfile) = @_;
    my $fh = open $podfile err die "Could not open '$podfile'\n";
    for =$fh -> $line {
        say $line;
    }
}

sub index_pods {
    say "Should index the files now";
    mkdir $dir if not -e $dir;
    # TODO: go over all the files in the standard directory, whatever the standard will be
    
    #my @files = list_files(dirname($PROGRAM_NAME));
    #say @files.perl;
    my %data;
    for list_files(dirname($PROGRAM_NAME)) -> $podfile {
        say "Processing '$podfile'";
        my $fh = open $podfile err die "Could not open '$podfile'\n";
        my $row = 0;
        my $section;
        for =$fh -> $line {
            $row++;
            if ($line ~~ /^=head\d\ (.*)/) {
                $section = $0;
            }
            if ($line ~~ /X\<(.*?)\>/) {
                #say "Found $row $0";
                #my %h = ("file" =>  $podfile,  "row" =>  $row);
                push @{%data{$0}}, "$row.$podfile"; # only one dimension work in pugs so we have this workaround
            }
            # always remember in which entry are we in (row number or =head? name or both)
            # if there is one or more X<> tags in a row, remember the values and in the end save to
            # an index file
        }
    }
    return %data;
}

# I think File::Find does not work currently...
sub list_files ($dir, $full) {
    #say "opening $dir";
    my $dh = opendir $dir err die "Could not open $dir";
    my @entries;
    for $dh.readdir -> $entry {
        next if $entry eq "." or $entry eq "..";
        #say "Entry $entry";
        if (substr($entry, -4) eq ".pod") {
            @entries.push($full ?? "$dir/$entry" !! $entry);
        }
        if (-d "$dir/$entry") {
            @entries.push(list_files("$dir/$entry", $full));
        }
    }
    return @entries;
}

sub lookup($keyword) {
    say "Now look up $keyword";
    my %data = index_pods();
    #say %data.keys;
    #say "-----";

    if (%data{$keyword}) {
        for @{%data{$keyword}} -> $entry {
            my ($row, $file) = split /\./, $entry, 2;
            say "here: $file  $row";
            my $fh = open $file err die "Could not open $file";
            for (0..$row) {
                =$fh;
            }
            # TODO: I guess the display should show a few lines before and a few lines after
            # max to the next section start, I am not sure.
            for (1..10) {
                my $line = =$fh;
                say $line;
            }
        }
    }
}

# TODO: especailly now that pugs is a bit slow starting up, we might want an interactive
# help system, that onces loads itself will not go down till exiting


# should be improved and moved to File::Basename, or better yet we should have one module with all
# frequently needed filesystem related functions....
sub dirname($path is copy) {
    $path ~~ s{/<-[/]>*$}{};
    return $path;
}

sub usage {
    say "Usage:";
#    say "    $PROGRAM_NAME --index";
#    say "    $PROGRAM_NAME --keyword KEYWORD";
    say "    $PROGRAM_NAME PODFILE     - display the given podfile";
    say "    $PROGRAM_NAME --list      - list the available podfiles";
    exit;
}



