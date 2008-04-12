use v6-alpha;

use Muldis::DB::Validator;

Muldis::DB::Validator::main(
    :engine_name('Muldis::DB::Engine::Example'),
    :machine_config({}),
);
