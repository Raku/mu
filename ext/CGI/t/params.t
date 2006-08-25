use v6-alpha;
use Test;

plan 30;

=pod

More advanced tests for params

=cut

use CGI; pass "(dummy instead of broken use_ok)";

my $q = CGI.new;

{
    my $query_string = 'multi=1;multi=2;multi=3';

    $q.unpack_params($query_string);
    is($q.pack_params, $query_string, 'packed the params correctly');

    my @param_keys = $q.param;
    is(+@param_keys, 1, 'we have 1 parameter');
    is(@param_keys[0], 'multi', 'the first one is "multi"');

    my $list = $q.param('multi');
    is(+$list, 3, 'we have three elements in the multi list');
    is($list[0], 1, 'the first element is 1');
    is($list[1], 2, 'the second element is 2');
    is($list[2], 3, 'the third element is 3');        
}

$q.clear_params;

{
    my $query_string = 'first_name=Stevan;last_name=Little';

    $q.unpack_params($query_string);
    is($q.pack_params, $query_string, 'packed the params correctly');

    my @param_keys = $q.param;
    is(+@param_keys, 2, 'we have 1 parameter');
    is(@param_keys[0], 'first_name', 'the first one is "first_name"');
    is(@param_keys[1], 'last_name', 'the first one is "last_name"');
    
    is($q.param('first_name'), 'Stevan', 'got the right value for first_name');       
    is($q.param('last_name'), 'Little', 'got the right value for last_name');
}

try { $q.set_delimiter('!') };
ok $!, 'Setting qs delimiter to an invalid value should fail';
ok $q.set_delimiter('&'), '... but setting it to a valid value should succeed';

$q.clear_params;

{
    my $query_string = 'multi=1&multi=2&multi=3';

    $q.unpack_params($query_string);
    is($q.pack_params, $query_string, 'packed the params correctly');

    my @param_keys = $q.param;
    is(+@param_keys, 1, 'we have 1 parameter');
    is(@param_keys[0], 'multi', 'the first one is "multi"');

    my $list = $q.param('multi');
    is(+$list, 3, 'we have three elements in the multi list');
    is($list[0], 1, 'the first element is 1');
    is($list[1], 2, 'the second element is 2');
    is($list[2], 3, 'the third element is 3');        

    $q.clear_params;

    $query_string = 'foo=1;foo=2;';
    $q.unpack_params($query_string);
    is($q.pack_params, 'foo=1&foo=2', 'Parms work even if the default delimiter is changed');
}

$q.clear_params;

{
    my $query_string = 'first_name=Stevan&last_name=Little';

    $q.unpack_params($query_string);
    is($q.pack_params, $query_string, 'packed the params correctly');

    my @param_keys = $q.param;
    is(+@param_keys, 2, 'we have 1 parameter');
    is(@param_keys[0], 'first_name', 'the first one is "first_name"');
    is(@param_keys[1], 'last_name', 'the first one is "last_name"');   
    
    is($q.param('first_name'), 'Stevan', 'got the right value for first_name');       
    is($q.param('last_name'), 'Little', 'got the right value for last_name');
}

