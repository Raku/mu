use strict;
use warnings;

our ($Class, $Object);

{
    tests => 2,
    code => sub {
        my $MI = $Class->new(
            '$:name'        => 'Class::MI',
            '$:superclass'    => $Class,
            '%:methods'        => {
                find_method => sub {
                    my $class = shift;
                    my $label = shift;

                    my $method = $class->get_method($label);
                    return $method if $method;

                    foreach my $superclass (@{ $class->superclass }){
                        my $method = $superclass->find_method($label);
                        return $method if $method;
                    }

                    return undef;
                },
            }
        );

        my $Sup1 = $Class->new(
            '$:name'        => 'super1',
            '$:superclass'    => $Object,
            '%:methods'        => {
                from_first => sub { "from_first" },
            },
        );

        my $Sup2 = $Class->new(
            '$:name'        => 'super2',
            '$:superclass'    => $Object,
            '%:methods'        => {
                from_second => sub { "from_second" },
            },
        );

        my $Sub = $MI->new(
            '$:name'        => 'sub',
            '$:superclass'    => [ $Sup1, $Sup2 ],
        );

        my $iSub = $Sub->new;
        is($iSub->from_first, "from_first", "MI parent 1");
        is($iSub->from_second, "from_second", "MI parent 2");
    },
}
