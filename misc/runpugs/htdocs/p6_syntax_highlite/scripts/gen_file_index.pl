# check out the documentation at the end...

use strict;
use File::Find;

# unbuffer output...
$|++;

#configure this by hand...
my $EXAMPLES_SVN_DIR = "c:/perl6/pugs/examples";
my $filename = "../js/pugs_examples.js";
my $MAX_LINES = 100;

print "EXAMPLES_SVN_DIR = $EXAMPLES_SVN_DIR\n";
print "filename = $filename\n";

# find perl6 examples recursively
print "Finding good Perl6 example(s)\n";
my @DIRLIST = ($EXAMPLES_SVN_DIR);
my %examples = ();
find(\&process_p6_example, @DIRLIST);


#convert hash to object of hashes
my $numExamples = scalar(keys %examples);
print "found $numExamples good perl6 example(s)\n";
if($numExamples > 0) {
    # if the json returned is good, then write it...
    print "Writing $filename\n";
    my $text = "var examples = {\n";
    my $count = 0;
    for my $example (sort keys %examples) {
        my $filepath = $examples{$example};
        $text .= "    '$example' : '$filepath'";
        $text .= ($count < $numExamples-1) ? ",\n" : "\n";
        $count++;
    }
    $text .= "};\n";
    open(FILE,">$filename") or 
        die "Error: could not open $filename for writing.\n";
    print FILE $text;    
    close(FILE);
} else {
    # otherwise, croak about it
    die "No examples are available!\n";
}

print "Thanks for your time... happy hacking!\n";

exit;

#-----------------------------------
# File::Find process a perl file
#-----------------------------------
sub process_p6_example {
    # find .pl files
    my $filename = $_;
    my $filepath = $File::Find::name;
    if($filename =~ /\.pl$/) {
        my $found = $examples{$filename};
        if($found) {
            print "duplicate $filename\n";
        } else {
            open(FILE,$filepath) or 
                die "Could not open $filepath for reading\n";
            my $count = 0;
            while(my $line = <FILE>) {
                $count++;
            }
            close(FILE);
            # insert into %examples
            if($count <= $MAX_LINES) {
                # skip file beyond $MAX_LINES
                $filepath =~ s/^.+examples\/(.+)$/$1/;
                $examples{$filename} = $filepath;    
            } else {
                print "File $filename has more than $count (max: $MAX_LINES)\n";
            }
        }
    }
}

