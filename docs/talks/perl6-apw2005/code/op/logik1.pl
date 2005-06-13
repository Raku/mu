#=err-Operator
# In Perl 5 Programmen verbreitet (aber fehleranfällig!):
my $option = $para{'x'} || "wert";

#!\pause
# Etwas besser, aber jedoch unübersichtlich:
my $option = $para{'x'};
$option = "wert" unless defined $option;

#!\pause
# zur Vereinfachung gibt es in Perl 6 den // Operator:
my $option = %para{'x'} // "wert";

#!\pause
# er kann auch sehr einfach verkettet werden:
my $option = %para{'x'} // %conf{'x'} // "wert";
