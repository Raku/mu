use ExtUtils::MakeMaker;

WriteMakefile(
    NAME            => 'Data::Transform::Trivial',
    VERSION_FROM    => 'lib/Data/Transform/Trivial.pm',
    ABSTRACT        => 'A trivial tree transformation engine',
    AUTHOR          => 'dakkar <dakkar@thenautilus.net>',
    PREREQ_PM       => { 'List::MoreUtils' => 0,
                         'List::Util' => 0,
                         'Carp::Clan' => 0,
                     },
);
