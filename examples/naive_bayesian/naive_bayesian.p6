#!/usr/bin/pugs

use v6;

my %words;

sub load_db returns Void {
    return() unless -e "words.db.p6";
    my $db = open("words.db.p6") err die "Cannot open the words.db.p6 file: $!";
    for (=$db) -> $_line {
        my $line = $_line;
        my ($key, $value) = split("\t", $line);
        %words{"$key"} = $value;
    }
    $db.close();
}

sub save_db returns Void {
    my $db = open("words.db.p6", :w) err die "Cannot open the words.db.p6 file: $!";
    for (%words.kv) -> $key, $value {
        $db.say($key ~ "\t" ~ $value);
    }
    $db.close();
}

sub parse_file (Str $file) returns Hash {
    my %words_in_file;    
    my $fh = open("$file") err die "Cannot open the '$file' file: $!";
    for (=$fh) -> $_line {
        my $line = $_line;       
        while ($line ~~ s:perl5/(\w+)[ \t\n\r]//) {
            %words_in_file{lc($0)}++;
        }
    }
    $fh.close;
    return %words_in_file;
}

sub add_words (Str $category, %words_in_file) returns Void {
    for (%words_in_file.kv) -> $key, $value {
        %words{"$category-$key"} += $value;
    }    
}

sub classify (%words_in_file) returns Void {

    my %count;
    my $total = 0;
    
    for (%words.kv) -> $key, $value {
        $key ~~ rx:perl5/^(.+)-(.+)$/;
        %count{$0} += $value;
        $total     += $value;
    }

    my %score;
    for (%words_in_file.keys) -> $word  {
        for (%count.kv) -> $category, $count {
            if (defined(%words{"$category-$word"})) {
                %score{$category} += log(%words{"$category-$word"} / $count);
            }
            else {
                %score{$category} += log(0.01 / $count);
            }
        }
    }
   
    for (%count.kv) -> $category, $count {
        %score{$category} += log($count / $total)
    }
    
    # do this weird sort block because: 
    #    %score{$^a} <=> %score{$^b}
    # does not currently work
    for (%count.keys.sort:{ %score{$^a} == %score{$^b} ?? 0 !! %score{$^a} > %score{$^b} ?? -1 !! 1 }) -> $category {
        say("$category %score{$category}");
    }
}

load_db();

if (@*ARGS[0] eq 'add' && +@*ARGS == 3) {
    add_words(@*ARGS[1], parse_file(@*ARGS[2]));
}
elsif (@*ARGS[0] eq 'classify' && +@*ARGS == 2) {
    classify(parse_file(@*ARGS[1]));
}
else {
    say("USAGE:
    add <category> <file>
    classify <file>");
}

save_db();
