
my $filename = @*ARGS.shift;
my $code = slurp($filename);

package GLOBAL { #sigh
  sub quote ($s) {
    $s.re_gsub(/ /,'&nbsp;').re_gsub(/</,'&lt;').re_gsub(/>/,'&gt;');
  }
}
sub text_pane () {
  my $n = -1; #0 based
  my $line = 0;
  my $line_start = sub () {
    $line++;
    my $s = '<span id=line'~$line~' class=ln>';
    my $num = sprintf('%3d  ',$line);
    $num.re_gsub(/ /,'&nbsp;');
    '<span id=lnN'~$line~' class=lineNum>'~$num~'</span>'~$s;
  };
  my $txt = "";
  $txt = $txt~$line_start.();
  $txt = $txt ~ $code.split(//).map(sub($e){
    $n++;
    my $pre = "";
    my $post = "";
    my $c = $e;
    if $e eq "\n" {
      $pre="</span>";
      $c="\n";
      $post = $line_start.();
    }
    elsif $e eq " " {$c='&nbsp'}
    elsif $e eq "<" {$c='&lt;'}
    elsif $e eq ">" {$c='&gt;'}
    $pre~'<span id=c'~$n~' class=d onmouseover="cOver(event)" onclick="cClick(event)">'~$c~"</span>"~$post;
  }).join("");
  $txt = '<pre>'~$txt~'</pre>';
  $txt = $txt ~'
<script>
var text_length='~$n~'
</script>
';
  $txt = $txt ~'
<script>
function char(n) {
  return document.getElementById("c"+n)
}
function color_text(from,to,fg,bg) {
  if(!from){ from = 0 }
  if(!to){ to =  text_length }
  for(var c=from ; c<to ; c++) {
    e = char(c)
    if(e) {
      if(fg) { e.style.color = fg }
      if(bg) { e.style.backgroundColor = bg }
    }
  }
}
function cClick(ev) {
  var event = ev || window.event;
  var target = event.target || event.srcElement;
  //target.style.color = "#f00"
  var id = target.id
  var c = id.substring(1,id.length) * 1
  //color_text(c,c+10,"#f30",null)
  parent.ast.color_asts(c)
  parent.ir.color_asts(c)
  return true
}
function cOver(ev) {
  return true
}
//color_text(10,20,"#070","#fdd")
function scroll_to_id(id) {
  var d = document.getElementById(id)
  var pos = findPos(d)[1] - 50
  if(pos < 0) { pos = 0 }
  window.scroll(0,pos)
}
function findPos(obj) {
  var curleft = curtop = 0;
  do {
    curleft += obj.offsetLeft;
    curtop += obj.offsetTop;  
  } while (obj = obj.offsetParent);
  return [curleft,curtop];
}
</script>
';
  my $head = '
<head>
<style type="text/css">
SPAN.lineNum {color: sienna}
SPAN.d {background-color: white;}
SPAN.d:hover {background-color: red;}
</style>
</head>
';
  $head~$txt;
}

$Main::match_counter = 1;
$Main::ir_counter = 1;
$Main::matches = [];
$Main::irnodes = [];
my $parse = $*parser1.parse($code,$filename);
my $ir = $parse.make_ir_from_Match_tree();
$ir.do_all_analysis();

sub ast_pane {
  my $s = '<small><pre>'~$parse.match_foo~'</pre></small>';
  my $id2r = '
range_from_id = []
'~ $Main::matches.map(sub($e){
  my $id = $e[0];
  my $m = $e[1];
  'range_from_id["'~$id~'"] = ['~$m.from~','~$m.to~']'
}).join("\n");
  my $c2id = '
function Ast(id,from,to) {
    this.id = id
    this.from = from
    this.to = to
//    this.covers = function(n) { return (n >= this.from && n <= this.to) }
}
//Ast.prototype.covers = function(n) { return (this.from <= n && n <= this.to) }
var all_asts = [
'~ $Main::matches.map(sub($e){
  my $id = $e[0];
  my $m = $e[1];
  '(new Ast("'~$id~'",'~$m.from~','~$m.to~'))'
}).join(",") ~ '
];
function find_asts_at(n) {
  var res = []
  var i
  for(i in all_asts) {
    var ast = all_asts[i]
    if(ast.from <= n && n <= ast.to) {
      res.push(ast)
    }
  }
  if(res.length == 0) { diebug }
  return res
}
var old_color_ast_elems = []
function color_asts(n) {
  var i
  for(i in old_color_ast_elems) {
    var e = old_color_ast_elems[i]
    e.style.backgroundColor = "#fff"
  }
  old_color_ast_elems = []
  asts = find_asts_at(n).reverse()
  ast = asts[0]
  if(ast) {
    var e = document.getElementById(ast.id)
    old_color_ast_elems = [e]
    e.style.backgroundColor = "#fef"
    var pos = findPos(e)[1] - 300
    if(pos < 0) { pos = 0 }
    window.scroll(0,pos)
  }
}
function findPos(obj) {
  var curleft = curtop = 0;
  do {
    curleft += obj.offsetLeft;
    curtop += obj.offsetTop;  
  } while (obj = obj.offsetParent);
  return [curleft,curtop];
}
';

  $s = $s ~ '
<script>
'~$id2r~$c2id~'
var old_from
var old_to
function mClick(ev) {
  var event = ev || window.event;
  var target = event.target || event.srcElement;
  target.style.color = "#f00"
  var id = target.id
  var r = range_from_id[id]
  if(old_from) {
    parent.src.color_text(old_from,old_to,"#000","#fff")
    old_from = old_to = null
  }
  if(r) {
    var from = r[0]
    var to = r[1]
    old_from = from
    old_to = to
    parent.src.color_text(from,to,null,"#ff0")
    parent.src.scroll_to_id("c"+from)
    parent.ir.color_asts(from)
  }
  return true
}
</script>
';
  $s;
}

sub ir_pane () {
  my $s = '
<head>
<style type="text/css">
SPAN.ir {z-index:1000}
SPAN.irnn {font-weight:bold; color:#ee0000; z-index:0}
SPAN.irfn {font-style:italic; color:#333333; z-index:0}
</style>
</head>
<small><pre>'~$ir.irx1_foo~'</pre></small>';
  my $id2r = '
range_from_id = []
'~ $Main::irnodes.map(sub($e){
  my $id = $e[0];
  my $ir = $e[1];
  my $m = $ir.match;
  'range_from_id["'~$id~'"] = ['~$m.from~','~$m.to~']'
}).join("\n");
  my $c2id = '
function Ir(id,from,to) {
    this.id = id
    this.from = from
    this.to = to
//    this.covers = function(n) { return (n >= this.from && n <= this.to) }
}
//Ir.prototype.covers = function(n) { return (this.from <= n && n <= this.to) }
var all_asts = [
'~ $Main::irnodes.map(sub($e){
  my $id = $e[0];
  my $ir = $e[1];
  my $m = $ir.match;
  '(new Ir("'~$id~'",'~$m.from~','~$m.to~'))'
}).join(",") ~ '
];
function find_asts_at(n) {
  var res = []
  var i
  for(i in all_asts) {
    var ast = all_asts[i]
    if(ast.from <= n && n <= ast.to) {
      res.push(ast)
    }
  }
  if(res.length == 0) { diebug }
  return res
}
var old_color_ast_elems = []
function color_asts(n) {
  var i
  for(i in old_color_ast_elems) {
    var e = old_color_ast_elems[i]
    e.style.backgroundColor = "#fff"
  }
  old_color_ast_elems = []
  asts = find_asts_at(n).reverse()
  ast = asts[0]
  if(ast) {
    var e = document.getElementById(ast.id)
    old_color_ast_elems = [e]
    e.style.backgroundColor = "#fef"
    var pos = findPos(e)[1] - 300
    if(pos < 0) { pos = 0 }
    window.scroll(0,pos)
  }
}
function findPos(obj) {
  var curleft = curtop = 0;
  do {
    curleft += obj.offsetLeft;
    curtop += obj.offsetTop;  
  } while (obj = obj.offsetParent);
  return [curleft,curtop];
}
';

  $s = $s ~ '
<script>
'~$id2r~$c2id~'
var old_from
var old_to
function iClick(ev,targ) {
  var event = ev || window.event;
  var target = targ || event.target || event.srcElement;
  var id = target.id
  var r = range_from_id[id]
  if(old_from) {
    parent.src.color_text(old_from,old_to,"#000","#fff")
    old_from = old_to = null
  }
  if(r) {
    var from = r[0]
    var to = r[1]
    old_from = from
    old_to = to
    parent.src.color_text(from,to,null,"#ff0")
    parent.src.scroll_to_id("c"+from)
    parent.ir.color_asts(from)
    parent.ast.color_asts(from)
  }
  return true
}
</script>
';
  $s;
}

my $drawable = '
<body onload="foo()">
<script type="text/javascript" src="wz_jsgraphics.js"></script>
<script>
function foo() {
  var jg = new jsGraphics("everything");
  jg.setColor("#00ff00")
  jg.drawLine(10, 113, 820, 455)
  jg.paint()
}
</script>
<div id="everything" style="position: absolute;width:"100%;"></div>
<IFRAME src="deleteme0.html" style="width:100%;height:100%">
</body>
';
my $frameset = '
<FRAMESET cols="30%, 30%, 40%">
<FRAME NAME="ast" src=deleteme2.html>
<FRAME NAME="ir" src=deleteme3.html>
<FRAME NAME="src" src=deleteme1.html>
</FRAMESET>
';
unslurp($frameset,'deleteme0.html');
unslurp(text_pane(),"deleteme1.html");
unslurp(ast_pane(),"deleteme2.html");
unslurp(ir_pane(),"deleteme3.html");
say $frameset;
#say $drawable;

class Match {
  method match_foo() {
    #my $s = $.rule~"<"~$.from~","~$.to~",'"~$.str~"',{";
    my $id = 'm'~$Main::match_counter++;
    $Main::matches.push([$id,self]);
    my $s = "<span id="~$id~' class=m onclick="mClick(event)">'~$.rule~"&lt;"~$.from~","~$.to~",'...',{";
    for $.hash.keys {
      my $k = $_;
      my $v = $.hash{$k};
      my $vs = 'undef';
      if defined($v) {
        $vs = $v.match_foo;
      }
      $s = $s ~ "\n  "~$k~" =&gt; "~self.indent_except_top($vs)~",";
    }
    if $.hash.keys.elems {$s = $s ~ "\n"}
    $s = $s ~ "}&gt;</span>";
  }
}
class ARRAY {
  method match_foo() {
    ("[\n" ~
     Match.indent(self.map(sub($e){$e.match_foo}).join(",\n")) ~
     "\n]")
  }
};
class HASH {
  method match_foo() {
    my $s = "{";
    for self.keys {
      my $k = $_;
      my $v = self.{$k};
      my $vs = 'undef';
      if defined($v) {
        $vs = $v.match_foo;
      }
      $s = $s ~ "\n  "~$k~" =&gt; "~Match.indent_except_top($vs)~",";
    }
    if self.keys.elems {$s = $s ~ "\n"}
    $s ~ "}"
  };
};
class STRING {
  method match_foo() {
    my $s = self;
    if $s.length > 30 { $s = $s.substr(0,20)~"..."; }
    "'"~quote($s)~"'"
  }
}
class INTEGER {
  method match_foo() {
    "'"~self~"'"
  }
}
class FLOAT {
  method match_foo() {
    "'"~self~"'"
  }
}
class UNDEF {
  method match_foo() {
    'undef'
  }
}


package IRx1 {
  class Base {
    method irx1_foo() {
      my $id = 'i'~$Main::ir_counter++;
      $Main::irnodes.push([$id,self]);
      my $s = '<span id='~$id~' class=ir onclick="iClick(event,null)">';
      # multiple onclick shouldn't be needed, but z-index doesn't seem to doing it.
      $s = $s ~ '<span class=irnn onclick="iClick(event,'~$id~')">'~$.node_name()~'</span>';
      $s = $s ~ "\n";
      my $vs = "";
      my $names = $.field_names();
      my $values = $.field_values().map(sub($e){$e.irx1_foo()});
      loop (my $i=0; $i < $names.elems; $i++) {
        $vs = $vs ~ '<span class=irfn onclick="iClick(event,'~$id~')">'~$names[$i]~'</span>  '~Match.indent_except_top($values[$i])~"\n";
      }
      # Need to deal with recursion
      # $vs = $vs ~ 'notes  ' ~Match.indent_except_top($.notes.irx1_foo)~"\n";
      $s = $s ~ Match.indent($vs);
      $s = $s ~ '</span>';
      $s;
    }
  }
}
class HASH {
  method irx1_foo() {
    my $s = "{";
    for self.keys {
      my $k = $_;
      my $v = self.{$k};
      my $vs = 'undef';
      if defined($v) {
        $vs = $v.irx1_foo;
      }
      $s = $s ~ "\n  "~$k~" =&gt; "~Match.indent_except_top($vs)~",";
    }
    if self.keys.elems {$s = $s ~ "\n"}
    $s ~ "}"
  };
};
package ARRAY {
  method irx1_foo() {
    ("[\n" ~
     Match.indent(self.map(sub($e){$e.irx1_foo}).join(",\n")) ~
     "\n]")
  };
};
package STRING {
  method irx1_foo() {
    my $s = self;
    if $s.length > 30 { $s = $s.substr(0,20)~"..."; }
    "'"~quote($s)~"'"
  };
};
package INTEGER {
  method irx1_foo() {
    self ~ ""
  };
};
package FLOAT {
  method irx1_foo() {
    self ~ ""
  };
};
package UNDEF {
  method irx1_foo() {
    'undef'
  };
};
