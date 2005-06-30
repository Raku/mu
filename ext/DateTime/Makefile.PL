use lib "../../", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('DateTime');
version    ('0.01');
abstract   ('Dates, DateTimes, and DateTimes with TimeZones');
author     ('Dave Rolsky');
license    ('perl');

WritePugs  (6);
