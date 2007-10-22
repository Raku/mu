use strict;
use warnings;

use Test::More 'no_plan';
use Pugs::Runtime::Tracer;

my $buf;
open $::PCR_TRACE_FH, '>', \$buf;

trace("Howdy,");
is $buf, 'Howdy,', 'trace works';

trace("Audrey!");
is $buf, 'Howdy,Audrey!', 'trace appends okay';

$buf = '';
open $::PCR_TRACE_FH, '>', \$buf;

trace_begin('quant', 644, 652, 25);
is $buf, '>>BEGIN quant<< 644..652 at 25' . "\n", 'trace_begin works';

trace_end('end', 1, 32);
is $buf, <<'_EOC_', 'trace_end works';
>>BEGIN quant<< 644..652 at 25
>>END end<< success at 32
_EOC_

trace_end('end', undef, 51);
is $buf, <<'_EOC_', 'trace_end works';
>>BEGIN quant<< 644..652 at 25
>>END end<< success at 32
>>END end<< fail at 51
_EOC_

my $out = expand_tracing_code(<<'_EOC_');
    ## <constant>
    ## pos: 25 32
    ...
    ## </constant>
_EOC_
is $out, <<'_EOC_';
    ## <constant>
    ## pos: 25 32
     do {
       trace_begin('constant', 25, 32, $pos);
       my $retval =
    ...
    ## </constant>
     ;
       trace_end('constant', $retval, $pos);
       return $retval;
     }
_EOC_

