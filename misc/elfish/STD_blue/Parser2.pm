
class Parser2 is Parser {

  method parser_name {
    %*ENV{'ELF_STD_BLUE_RUN'} || "./STD_blue_run";
  };

};

if not($*parser0) { $*parser0 = Parser2.new('is_for_active_runtime',1) }
$*parser1 = Parser2.new;
