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

my $Class = 'JIB::Repository';
my $Root = 'repo';
my @Acc = sort qw(root config pool pool_rel index index_file);
use_ok($Class);

### create object
my $Repo;
{
    $Repo = $Class->new(root => $Root);
    isa_ok($Repo, $Class,       'Object created');

    my @can = sort $Repo->ls_accessors;
    ok(scalar @can,             '  Object has accessors');
    is_deeply(\@can, \@Acc,     '  Object can do what it should');

    for my $method (@can) {
        ok( defined $Repo->$method, 
                                "  '$method' returns value");
    }
}

### create repo
{
    ok($Repo->create(),         'Creating repository');

    for my $file (qw(jibs dists)) {
        ok(-e dir($Root)->subdir($file), 
                                "  '$file' exists");
    }
}

### adding packages
{   ### XXX config
    my $file1 = 'p5-Foo-Bar-1.2-cpan+KANE.jib';
    my $jib1 = dir('src')->file($file1);
    my $pkg1 = JIB::Package->new(file => $jib1);

    ok($Repo->add_package(package => $pkg1), 
                                'Adding a package');

    ok(-e $Repo->pool->file($file1), 
                                '  package exists in the pool afterwards');

    
    for my $index ( $Repo->index_file, $Repo->index_files(package => $pkg1) ) {
        ok(my $index_data = YAML::Load(scalar $index->slurp), 
                                "  index '$index' looks ok");
        is($index_data->[0]->{package}, $pkg1->package, 
                                '    and contains right package as 1st item');
    }
    

    my $log = file('error_log');
    {   my $fh = $log->openw;
        local $Log::Message::Simple::ERROR_FH = $fh;

        ok(!$Repo->add_package(package => $pkg1), 
                                "  Adding the same package again doesn't work");

        ### must close file before being able to slurp it, at least on OSX
        close $fh;
        like(scalar $log->slurp, qr/already exists in this repository/, 
                                '    and yields an error');

        $log->remove;
    }

    {
        local $Log::Message::Simple::ERROR_FH = $log->openw;

        ok($Repo->add_package(package => $pkg1, force => 1), 
                                '  But it does if we force it');
        ok(-z $log,             "    and doesn't give an error message");

        $log->remove;
    }
}

END {
    dir($Root)->rmtree;
}

# Local variables:
# c-indentation-style: bsd
# c-basic-offset: 4
# indent-tabs-mode: nil
# End:
# vim: expandtab shiftwidth=4:
