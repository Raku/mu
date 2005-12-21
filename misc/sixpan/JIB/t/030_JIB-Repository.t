use Test::More 'no_plan';
use Test::NoWarnings;
use strict;
use YAML;
use Path::Class;

BEGIN {
    chdir 't' if -d 't';
    use lib qw[../lib inc];
    require 'conf.pl';
}

my $class = 'JIB::Repository';
my $root = 'repo';
my @acc = sort qw(root config pool pool_rel index index_file);
use_ok($class);

# create object
my $repo;
{
    $repo = $class->new(root => $root);
    isa_ok($repo, $class, 'Object created');

    my @can = sort $repo->ls_accessors;
    ok(scalar @can, '  Object has accessors');
    is_deeply(\@can, \@acc, '  Object can do what it should');

    for my $method (@can) {
        ok(defined $repo->$method, "  '$method' returns value");
    }
}

# create repo
{
    ok($repo->create(), 'Creating repository');

    for my $file (qw(jibs dists)) {
        ok(-e dir($root)->subdir($file), "  '$file' exists");
    }
}

# adding packages
{
    my $file1 = 'p5-Foo-Bar-1.2-cpan+KANE.jib';
    my $jib1 = dir('src')->file($file1);
    my $pkg1 = JIB::Package->new(file => $jib1);

    ok($repo->add_package(package => $pkg1), 'Adding a package');

    ok(-e $repo->pool->file($file1), '  package exists in the pool afterwards');

    {
        for my $index ($repo->index_file, $repo->index_files(package => $pkg1)) {
            ok(my $index_data = YAML::Load(scalar $index->slurp), "  index '$index' looks ok");
            is($index_data->[0]->{package}, $pkg1->package, '    and contains the right package as the first item');
        }
    }

    my $log = file('error_log');
    {
        local $Log::Message::Simple::ERROR_FH = $log->openw;

        ok(!$repo->add_package(package => $pkg1), '  Adding the same package again does\'t work');
        like(scalar $log->slurp, qr/already exists in this repository/, '    and yields an error');

        $log->remove;
    }

    {
        local $Log::Message::Simple::ERROR_FH = $log->openw;

        ok($repo->add_package(package => $pkg1, force => 1), '  But it does if we force it');
        ok(-z $log, '    and doesn\'t give an error message');

        $log->remove;
    }
}

END {
    dir($root)->rmtree;
}

# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
