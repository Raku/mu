# PIL2 simple tokenizer & pretty-printer
# ../../pugs -CPIL2 -e ' say "hello" ' | ../../pugs pil2_tokenizer.p6

use v6;

my @pil2 = =<>;
my $pil2 = @pil2.join('');

my $tokens =
    m:g {
        (
          \" [ \\\\ | \\" | . ]*? \" |   # quoted string
          \, | \= | \{ | \( | \[ | \} | \) | \] | \w+
        )
    };

my @b = $pil2 ~~ $tokens;

my $tabs = 1;
my $tab = '  ';
for @b {
    if $_ eq ',' { 
        print $_, "\n", $tab x $tabs;
    }
    elsif $_ eq '['|'('|'{' { 
        print $_, "\n"; $tabs++; print $tab x $tabs;
    }
    elsif $_ eq ']'|')'|'}' { 
        $tabs--; print "\n", $tab x $tabs, $_, " ";
    }
    else { print $_, " " }
}
print "\n";
