#! /opt/local/bin/perl

print "cleaning the precompilation cache for v6 and PCR\n";

eval {
    require Cache::FileCache;
    my $cache = new Cache::FileCache( { 'namespace' => 'v6-rules' } );
    $cache->Clear;
};

print "$@ \n" if $@;

