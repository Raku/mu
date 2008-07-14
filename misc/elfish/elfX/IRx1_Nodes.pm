package IRx1 {
  class Base {
  };
  class CompUnit_and_Block {};
  class CompUnit is CompUnit_and_Block {};
  class Block    is CompUnit_and_Block {};

  class CompUnit is Base {
    has $.match;
    has $.statements;
    has $.filename;
    has $.notes;
    
    method newp($match,$statements,$filename) { self.new('match', $match, 'statements', $statements, 'filename', $filename) };
    method callback($emitter,$arg) { $emitter.cb__CompUnit(self,$arg) };
    method node_name() { 'CompUnit' };
    method field_names() { ['statements','filename'] };
    method field_values() { [$.statements,$.filename] };
  };
  class Block is Base {
    has $.match;
    has $.statements;
    has $.notes;
    
    method newp($match,$statements) { self.new('match', $match, 'statements', $statements) };
    method callback($emitter,$arg) { $emitter.cb__Block(self,$arg) };
    method node_name() { 'Block' };
    method field_names() { ['statements'] };
    method field_values() { [$.statements] };
  };
  class Use is Base {
    has $.match;
    has $.kind;
    has $.module_name;
    has $.expr;
    has $.notes;
    
    method newp($match,$kind,$module_name,$expr) { self.new('match', $match, 'kind', $kind, 'module_name', $module_name, 'expr', $expr) };
    method callback($emitter,$arg) { $emitter.cb__Use(self,$arg) };
    method node_name() { 'Use' };
    method field_names() { ['kind','module_name','expr'] };
    method field_values() { [$.kind,$.module_name,$.expr] };
  };
  class PackageDecl is Base {
    has $.match;
    has $.scope;
    has $.plurality;
    has $.kind;
    has $.name;
    has $.traits;
    has $.block;
    has $.notes;
    
    method newp($match,$scope,$plurality,$kind,$name,$traits,$block) { self.new('match', $match, 'scope', $scope, 'plurality', $plurality, 'kind', $kind, 'name', $name, 'traits', $traits, 'block', $block) };
    method callback($emitter,$arg) { $emitter.cb__PackageDecl(self,$arg) };
    method node_name() { 'PackageDecl' };
    method field_names() { ['scope','plurality','kind','name','traits','block'] };
    method field_values() { [$.scope,$.plurality,$.kind,$.name,$.traits,$.block] };
  };
  class MethodDecl is Base {
    has $.match;
    has $.scope;
    has $.typenames;
    has $.plurality;
    has $.name;
    has $.multisig;
    has $.traits;
    has $.block;
    has $.sigil;
    has $.postcircumfix;
    has $.notes;
    
    method newp($match,$scope,$typenames,$plurality,$name,$multisig,$traits,$block,$sigil,$postcircumfix) { self.new('match', $match, 'scope', $scope, 'typenames', $typenames, 'plurality', $plurality, 'name', $name, 'multisig', $multisig, 'traits', $traits, 'block', $block, 'sigil', $sigil, 'postcircumfix', $postcircumfix) };
    method callback($emitter,$arg) { $emitter.cb__MethodDecl(self,$arg) };
    method node_name() { 'MethodDecl' };
    method field_names() { ['scope','typenames','plurality','name','multisig','traits','block','sigil','postcircumfix'] };
    method field_values() { [$.scope,$.typenames,$.plurality,$.name,$.multisig,$.traits,$.block,$.sigil,$.postcircumfix] };
  };
  class SubDecl is Base {
    has $.match;
    has $.scope;
    has $.typenames;
    has $.plurality;
    has $.name;
    has $.multisig;
    has $.traits;
    has $.block;
    has $.notes;
    
    method newp($match,$scope,$typenames,$plurality,$name,$multisig,$traits,$block) { self.new('match', $match, 'scope', $scope, 'typenames', $typenames, 'plurality', $plurality, 'name', $name, 'multisig', $multisig, 'traits', $traits, 'block', $block) };
    method callback($emitter,$arg) { $emitter.cb__SubDecl(self,$arg) };
    method node_name() { 'SubDecl' };
    method field_names() { ['scope','typenames','plurality','name','multisig','traits','block'] };
    method field_values() { [$.scope,$.typenames,$.plurality,$.name,$.multisig,$.traits,$.block] };
  };
  class MacroDecl is Base {
    has $.match;
    has $.scope;
    has $.typenames;
    has $.plurality;
    has $.name;
    has $.multisig;
    has $.traits;
    has $.block;
    has $.notes;
    
    method newp($match,$scope,$typenames,$plurality,$name,$multisig,$traits,$block) { self.new('match', $match, 'scope', $scope, 'typenames', $typenames, 'plurality', $plurality, 'name', $name, 'multisig', $multisig, 'traits', $traits, 'block', $block) };
    method callback($emitter,$arg) { $emitter.cb__MacroDecl(self,$arg) };
    method node_name() { 'MacroDecl' };
    method field_names() { ['scope','typenames','plurality','name','multisig','traits','block'] };
    method field_values() { [$.scope,$.typenames,$.plurality,$.name,$.multisig,$.traits,$.block] };
  };
  class VarDecl is Base {
    has $.match;
    has $.scope;
    has $.typenames;
    has $.plurality;
    has $.var;
    has $.postcircumfix;
    has $.traits;
    has $.default_op;
    has $.default_expr;
    has $.notes;
    
    method newp($match,$scope,$typenames,$plurality,$var,$postcircumfix,$traits,$default_op,$default_expr) { self.new('match', $match, 'scope', $scope, 'typenames', $typenames, 'plurality', $plurality, 'var', $var, 'postcircumfix', $postcircumfix, 'traits', $traits, 'default_op', $default_op, 'default_expr', $default_expr) };
    method callback($emitter,$arg) { $emitter.cb__VarDecl(self,$arg) };
    method node_name() { 'VarDecl' };
    method field_names() { ['scope','typenames','plurality','var','postcircumfix','traits','default_op','default_expr'] };
    method field_values() { [$.scope,$.typenames,$.plurality,$.var,$.postcircumfix,$.traits,$.default_op,$.default_expr] };
  };
  class Var is Base {
    has $.match;
    has $.sigil;
    has $.twigil;
    has $.name;
    has $.notes;
    
    method newp($match,$sigil,$twigil,$name) { self.new('match', $match, 'sigil', $sigil, 'twigil', $twigil, 'name', $name) };
    method callback($emitter,$arg) { $emitter.cb__Var(self,$arg) };
    method node_name() { 'Var' };
    method field_names() { ['sigil','twigil','name'] };
    method field_values() { [$.sigil,$.twigil,$.name] };
  };
  class Trait is Base {
    has $.match;
    has $.verb;
    has $.expr;
    has $.notes;
    
    method newp($match,$verb,$expr) { self.new('match', $match, 'verb', $verb, 'expr', $expr) };
    method callback($emitter,$arg) { $emitter.cb__Trait(self,$arg) };
    method node_name() { 'Trait' };
    method field_names() { ['verb','expr'] };
    method field_values() { [$.verb,$.expr] };
  };
  class ClosureTrait is Base {
    has $.match;
    has $.kind;
    has $.block;
    has $.notes;
    
    method newp($match,$kind,$block) { self.new('match', $match, 'kind', $kind, 'block', $block) };
    method callback($emitter,$arg) { $emitter.cb__ClosureTrait(self,$arg) };
    method node_name() { 'ClosureTrait' };
    method field_names() { ['kind','block'] };
    method field_values() { [$.kind,$.block] };
  };
  class ModuleName is Base {
    has $.match;
    has $.name;
    has $.pairs;
    has $.notes;
    
    method newp($match,$name,$pairs) { self.new('match', $match, 'name', $name, 'pairs', $pairs) };
    method callback($emitter,$arg) { $emitter.cb__ModuleName(self,$arg) };
    method node_name() { 'ModuleName' };
    method field_names() { ['name','pairs'] };
    method field_values() { [$.name,$.pairs] };
  };
  class PathName is Base {
    has $.match;
    has $.path;
    has $.notes;
    
    method newp($match,$path) { self.new('match', $match, 'path', $path) };
    method callback($emitter,$arg) { $emitter.cb__PathName(self,$arg) };
    method node_name() { 'PathName' };
    method field_names() { ['path'] };
    method field_values() { [$.path] };
  };
  class SubName is Base {
    has $.match;
    has $.category;
    has $.pairs;
    has $.desigilname;
    has $.signature;
    has $.notes;
    
    method newp($match,$category,$pairs,$desigilname,$signature) { self.new('match', $match, 'category', $category, 'pairs', $pairs, 'desigilname', $desigilname, 'signature', $signature) };
    method callback($emitter,$arg) { $emitter.cb__SubName(self,$arg) };
    method node_name() { 'SubName' };
    method field_names() { ['category','pairs','desigilname','signature'] };
    method field_values() { [$.category,$.pairs,$.desigilname,$.signature] };
  };
  class ShapedParamName is Base {
    has $.match;
    has $.ident;
    has $.postcircumfix;
    has $.notes;
    
    method newp($match,$ident,$postcircumfix) { self.new('match', $match, 'ident', $ident, 'postcircumfix', $postcircumfix) };
    method callback($emitter,$arg) { $emitter.cb__ShapedParamName(self,$arg) };
    method node_name() { 'ShapedParamName' };
    method field_names() { ['ident','postcircumfix'] };
    method field_values() { [$.ident,$.postcircumfix] };
  };
  class Call is Base {
    has $.match;
    has $.invocant;
    has $.method;
    has $.capture;
    has $.notes;
    
    method newp($match,$invocant,$method,$capture) { self.new('match', $match, 'invocant', $invocant, 'method', $method, 'capture', $capture) };
    method callback($emitter,$arg) { $emitter.cb__Call(self,$arg) };
    method node_name() { 'Call' };
    method field_names() { ['invocant','method','capture'] };
    method field_values() { [$.invocant,$.method,$.capture] };
  };
  class Apply is Base {
    has $.match;
    has $.function;
    has $.capture;
    has $.notes;
    
    method newp($match,$function,$capture) { self.new('match', $match, 'function', $function, 'capture', $capture) };
    method callback($emitter,$arg) { $emitter.cb__Apply(self,$arg) };
    method node_name() { 'Apply' };
    method field_names() { ['function','capture'] };
    method field_values() { [$.function,$.capture] };
  };
  class Hyper is Base {
    has $.match;
    has $.operator;
    has $.capture;
    has $.notes;
    
    method newp($match,$operator,$capture) { self.new('match', $match, 'operator', $operator, 'capture', $capture) };
    method callback($emitter,$arg) { $emitter.cb__Hyper(self,$arg) };
    method node_name() { 'Hyper' };
    method field_names() { ['operator','capture'] };
    method field_values() { [$.operator,$.capture] };
  };
  class Capture is Base {
    has $.match;
    has $.arguments;
    has $.invocant;
    has $.notes;
    
    method newp($match,$arguments,$invocant) { self.new('match', $match, 'arguments', $arguments, 'invocant', $invocant) };
    method callback($emitter,$arg) { $emitter.cb__Capture(self,$arg) };
    method node_name() { 'Capture' };
    method field_names() { ['arguments','invocant'] };
    method field_values() { [$.arguments,$.invocant] };
  };
  class MultiSig is Base {
    has $.match;
    has $.signatures;
    has $.notes;
    
    method newp($match,$signatures) { self.new('match', $match, 'signatures', $signatures) };
    method callback($emitter,$arg) { $emitter.cb__MultiSig(self,$arg) };
    method node_name() { 'MultiSig' };
    method field_names() { ['signatures'] };
    method field_values() { [$.signatures] };
  };
  class Signature is Base {
    has $.match;
    has $.parameters;
    has $.return_type;
    has $.notes;
    
    method newp($match,$parameters,$return_type) { self.new('match', $match, 'parameters', $parameters, 'return_type', $return_type) };
    method callback($emitter,$arg) { $emitter.cb__Signature(self,$arg) };
    method node_name() { 'Signature' };
    method field_names() { ['parameters','return_type'] };
    method field_values() { [$.parameters,$.return_type] };
  };
  class Parameter is Base {
    has $.match;
    has $.type_constraints;
    has $.quant;
    has $.param_var;
    has $.ident;
    has $.traits;
    has $.post_constraints;
    has $.default_expr;
    has $.notes;
    
    method newp($match,$type_constraints,$quant,$param_var,$ident,$traits,$post_constraints,$default_expr) { self.new('match', $match, 'type_constraints', $type_constraints, 'quant', $quant, 'param_var', $param_var, 'ident', $ident, 'traits', $traits, 'post_constraints', $post_constraints, 'default_expr', $default_expr) };
    method callback($emitter,$arg) { $emitter.cb__Parameter(self,$arg) };
    method node_name() { 'Parameter' };
    method field_names() { ['type_constraints','quant','param_var','ident','traits','post_constraints','default_expr'] };
    method field_values() { [$.type_constraints,$.quant,$.param_var,$.ident,$.traits,$.post_constraints,$.default_expr] };
  };
  class TypeConstraint is Base {
    has $.match;
    has $.value;
    has $.where_expr;
    has $.notes;
    
    method newp($match,$value,$where_expr) { self.new('match', $match, 'value', $value, 'where_expr', $where_expr) };
    method callback($emitter,$arg) { $emitter.cb__TypeConstraint(self,$arg) };
    method node_name() { 'TypeConstraint' };
    method field_names() { ['value','where_expr'] };
    method field_values() { [$.value,$.where_expr] };
  };
  class PostConstraint is Base {
    has $.match;
    has $.multisig;
    has $.where_expr;
    has $.notes;
    
    method newp($match,$multisig,$where_expr) { self.new('match', $match, 'multisig', $multisig, 'where_expr', $where_expr) };
    method callback($emitter,$arg) { $emitter.cb__PostConstraint(self,$arg) };
    method node_name() { 'PostConstraint' };
    method field_names() { ['multisig','where_expr'] };
    method field_values() { [$.multisig,$.where_expr] };
  };
  class ParamVar is Base {
    has $.match;
    has $.sigil;
    has $.twigil;
    has $.name;
    has $.notes;
    
    method newp($match,$sigil,$twigil,$name) { self.new('match', $match, 'sigil', $sigil, 'twigil', $twigil, 'name', $name) };
    method callback($emitter,$arg) { $emitter.cb__ParamVar(self,$arg) };
    method node_name() { 'ParamVar' };
    method field_names() { ['sigil','twigil','name'] };
    method field_values() { [$.sigil,$.twigil,$.name] };
  };
  class Undef is Base {
    has $.match;
    has $.notes;
    
    method newp($match) { self.new('match', $match) };
    method callback($emitter,$arg) { $emitter.cb__Undef(self,$arg) };
    method node_name() { 'Undef' };
    method field_names() { [] };
    method field_values() { [] };
  };
  class NumInt is Base {
    has $.match;
    has $.text;
    has $.base;
    has $.notes;
    
    method newp($match,$text,$base) { self.new('match', $match, 'text', $text, 'base', $base) };
    method callback($emitter,$arg) { $emitter.cb__NumInt(self,$arg) };
    method node_name() { 'NumInt' };
    method field_names() { ['text','base'] };
    method field_values() { [$.text,$.base] };
  };
  class NumDec is Base {
    has $.match;
    has $.intpart;
    has $.fracpart;
    has $.exp;
    has $.notes;
    
    method newp($match,$intpart,$fracpart,$exp) { self.new('match', $match, 'intpart', $intpart, 'fracpart', $fracpart, 'exp', $exp) };
    method callback($emitter,$arg) { $emitter.cb__NumDec(self,$arg) };
    method node_name() { 'NumDec' };
    method field_names() { ['intpart','fracpart','exp'] };
    method field_values() { [$.intpart,$.fracpart,$.exp] };
  };
  class NumRad is Base {
    has $.match;
    has $.radix;
    has $.intpart;
    has $.fracpart;
    has $.base;
    has $.exp;
    has $.notes;
    
    method newp($match,$radix,$intpart,$fracpart,$base,$exp) { self.new('match', $match, 'radix', $radix, 'intpart', $intpart, 'fracpart', $fracpart, 'base', $base, 'exp', $exp) };
    method callback($emitter,$arg) { $emitter.cb__NumRad(self,$arg) };
    method node_name() { 'NumRad' };
    method field_names() { ['radix','intpart','fracpart','base','exp'] };
    method field_values() { [$.radix,$.intpart,$.fracpart,$.base,$.exp] };
  };
  class Array is Base {
    has $.match;
    has $.array;
    has $.notes;
    
    method newp($match,$array) { self.new('match', $match, 'array', $array) };
    method callback($emitter,$arg) { $emitter.cb__Array(self,$arg) };
    method node_name() { 'Array' };
    method field_names() { ['array'] };
    method field_values() { [$.array] };
  };
  class Hash is Base {
    has $.match;
    has $.hash;
    has $.notes;
    
    method newp($match,$hash) { self.new('match', $match, 'hash', $hash) };
    method callback($emitter,$arg) { $emitter.cb__Hash(self,$arg) };
    method node_name() { 'Hash' };
    method field_names() { ['hash'] };
    method field_values() { [$.hash] };
  };
  class Pair is Base {
    has $.match;
    has $.key;
    has $.value;
    has $.notes;
    
    method newp($match,$key,$value) { self.new('match', $match, 'key', $key, 'value', $value) };
    method callback($emitter,$arg) { $emitter.cb__Pair(self,$arg) };
    method node_name() { 'Pair' };
    method field_names() { ['key','value'] };
    method field_values() { [$.key,$.value] };
  };
  class Type is Base {
    has $.match;
    has $.typename;
    has $.notes;
    
    method newp($match,$typename) { self.new('match', $match, 'typename', $typename) };
    method callback($emitter,$arg) { $emitter.cb__Type(self,$arg) };
    method node_name() { 'Type' };
    method field_names() { ['typename'] };
    method field_values() { [$.typename] };
  };
  class Rx is Base {
    has $.match;
    has $.pat;
    has $.modifiers;
    has $.notes;
    
    method newp($match,$pat,$modifiers) { self.new('match', $match, 'pat', $pat, 'modifiers', $modifiers) };
    method callback($emitter,$arg) { $emitter.cb__Rx(self,$arg) };
    method node_name() { 'Rx' };
    method field_names() { ['pat','modifiers'] };
    method field_values() { [$.pat,$.modifiers] };
  };
  class Buf is Base {
    has $.match;
    has $.buf;
    has $.notes;
    
    method newp($match,$buf) { self.new('match', $match, 'buf', $buf) };
    method callback($emitter,$arg) { $emitter.cb__Buf(self,$arg) };
    method node_name() { 'Buf' };
    method field_names() { ['buf'] };
    method field_values() { [$.buf] };
  };
  class For is Base {
    has $.match;
    has $.expr;
    has $.block;
    has $.notes;
    
    method newp($match,$expr,$block) { self.new('match', $match, 'expr', $expr, 'block', $block) };
    method callback($emitter,$arg) { $emitter.cb__For(self,$arg) };
    method node_name() { 'For' };
    method field_names() { ['expr','block'] };
    method field_values() { [$.expr,$.block] };
  };
  class Cond is Base {
    has $.match;
    has $.clauses;
    has $.default;
    has $.invert_first_test;
    has $.notes;
    
    method newp($match,$clauses,$default,$invert_first_test) { self.new('match', $match, 'clauses', $clauses, 'default', $default, 'invert_first_test', $invert_first_test) };
    method callback($emitter,$arg) { $emitter.cb__Cond(self,$arg) };
    method node_name() { 'Cond' };
    method field_names() { ['clauses','default','invert_first_test'] };
    method field_values() { [$.clauses,$.default,$.invert_first_test] };
  };
  class Loop is Base {
    has $.match;
    has $.pretest;
    has $.block;
    has $.posttest;
    has $.label;
    has $.notes;
    
    method newp($match,$pretest,$block,$posttest,$label) { self.new('match', $match, 'pretest', $pretest, 'block', $block, 'posttest', $posttest, 'label', $label) };
    method callback($emitter,$arg) { $emitter.cb__Loop(self,$arg) };
    method node_name() { 'Loop' };
    method field_names() { ['pretest','block','posttest','label'] };
    method field_values() { [$.pretest,$.block,$.posttest,$.label] };
  };
  class Given is Base {
    has $.match;
    has $.expr;
    has $.block;
    has $.notes;
    
    method newp($match,$expr,$block) { self.new('match', $match, 'expr', $expr, 'block', $block) };
    method callback($emitter,$arg) { $emitter.cb__Given(self,$arg) };
    method node_name() { 'Given' };
    method field_names() { ['expr','block'] };
    method field_values() { [$.expr,$.block] };
  };
  class When is Base {
    has $.match;
    has $.expr;
    has $.block;
    has $.notes;
    
    method newp($match,$expr,$block) { self.new('match', $match, 'expr', $expr, 'block', $block) };
    method callback($emitter,$arg) { $emitter.cb__When(self,$arg) };
    method node_name() { 'When' };
    method field_names() { ['expr','block'] };
    method field_values() { [$.expr,$.block] };
  };
  class Label is Base {
    has $.match;
    has $.labels;
    has $.statement;
    has $.notes;
    
    method newp($match,$labels,$statement) { self.new('match', $match, 'labels', $labels, 'statement', $statement) };
    method callback($emitter,$arg) { $emitter.cb__Label(self,$arg) };
    method node_name() { 'Label' };
    method field_names() { ['labels','statement'] };
    method field_values() { [$.labels,$.statement] };
  };
  class RegexDef is Base {
    has $.match;
    has $.ident;
    has $.pattern;
    has $.notes;
    
    method newp($match,$ident,$pattern) { self.new('match', $match, 'ident', $ident, 'pattern', $pattern) };
    method callback($emitter,$arg) { $emitter.cb__RegexDef(self,$arg) };
    method node_name() { 'RegexDef' };
    method field_names() { ['ident','pattern'] };
    method field_values() { [$.ident,$.pattern] };
  };
  class Regex is Base {
    has $.match;
    has $.patterns;
    has $.notes;
    
    method newp($match,$patterns) { self.new('match', $match, 'patterns', $patterns) };
    method callback($emitter,$arg) { $emitter.cb__Regex(self,$arg) };
    method node_name() { 'Regex' };
    method field_names() { ['patterns'] };
    method field_values() { [$.patterns] };
  };
  class RxFirst is Base {
    has $.match;
    has $.patterns;
    has $.notes;
    
    method newp($match,$patterns) { self.new('match', $match, 'patterns', $patterns) };
    method callback($emitter,$arg) { $emitter.cb__RxFirst(self,$arg) };
    method node_name() { 'RxFirst' };
    method field_names() { ['patterns'] };
    method field_values() { [$.patterns] };
  };
  class RxEvery is Base {
    has $.match;
    has $.patterns;
    has $.notes;
    
    method newp($match,$patterns) { self.new('match', $match, 'patterns', $patterns) };
    method callback($emitter,$arg) { $emitter.cb__RxEvery(self,$arg) };
    method node_name() { 'RxEvery' };
    method field_names() { ['patterns'] };
    method field_values() { [$.patterns] };
  };
  class RxSubmatch is Base {
    has $.match;
    has $.patterns;
    has $.notes;
    
    method newp($match,$patterns) { self.new('match', $match, 'patterns', $patterns) };
    method callback($emitter,$arg) { $emitter.cb__RxSubmatch(self,$arg) };
    method node_name() { 'RxSubmatch' };
    method field_names() { ['patterns'] };
    method field_values() { [$.patterns] };
  };
  class RxAny is Base {
    has $.match;
    has $.patterns;
    has $.notes;
    
    method newp($match,$patterns) { self.new('match', $match, 'patterns', $patterns) };
    method callback($emitter,$arg) { $emitter.cb__RxAny(self,$arg) };
    method node_name() { 'RxAny' };
    method field_names() { ['patterns'] };
    method field_values() { [$.patterns] };
  };
  class RxAll is Base {
    has $.match;
    has $.patterns;
    has $.notes;
    
    method newp($match,$patterns) { self.new('match', $match, 'patterns', $patterns) };
    method callback($emitter,$arg) { $emitter.cb__RxAll(self,$arg) };
    method node_name() { 'RxAll' };
    method field_names() { ['patterns'] };
    method field_values() { [$.patterns] };
  };
  class RxSequence is Base {
    has $.match;
    has $.patterns;
    has $.notes;
    
    method newp($match,$patterns) { self.new('match', $match, 'patterns', $patterns) };
    method callback($emitter,$arg) { $emitter.cb__RxSequence(self,$arg) };
    method node_name() { 'RxSequence' };
    method field_names() { ['patterns'] };
    method field_values() { [$.patterns] };
  };
  class RxQuantifiedAtom is Base {
    has $.match;
    has $.atom;
    has $.quantifier;
    has $.notes;
    
    method newp($match,$atom,$quantifier) { self.new('match', $match, 'atom', $atom, 'quantifier', $quantifier) };
    method callback($emitter,$arg) { $emitter.cb__RxQuantifiedAtom(self,$arg) };
    method node_name() { 'RxQuantifiedAtom' };
    method field_names() { ['atom','quantifier'] };
    method field_values() { [$.atom,$.quantifier] };
  };
  class RxBackslash is Base {
    has $.match;
    has $.char;
    has $.notes;
    
    method newp($match,$char) { self.new('match', $match, 'char', $char) };
    method callback($emitter,$arg) { $emitter.cb__RxBackslash(self,$arg) };
    method node_name() { 'RxBackslash' };
    method field_names() { ['char'] };
    method field_values() { [$.char] };
  };
  class RxAssertion is Base {
    has $.match;
    has $.ident;
    has $.notes;
    
    method newp($match,$ident) { self.new('match', $match, 'ident', $ident) };
    method callback($emitter,$arg) { $emitter.cb__RxAssertion(self,$arg) };
    method node_name() { 'RxAssertion' };
    method field_names() { ['ident'] };
    method field_values() { [$.ident] };
  };
  class RxModInternal is Base {
    has $.match;
    has $.mod;
    has $.notes;
    
    method newp($match,$mod) { self.new('match', $match, 'mod', $mod) };
    method callback($emitter,$arg) { $emitter.cb__RxModInternal(self,$arg) };
    method node_name() { 'RxModInternal' };
    method field_names() { ['mod'] };
    method field_values() { [$.mod] };
  };
  class RxCapture is Base {
    has $.match;
    has $.pattern;
    has $.notes;
    
    method newp($match,$pattern) { self.new('match', $match, 'pattern', $pattern) };
    method callback($emitter,$arg) { $emitter.cb__RxCapture(self,$arg) };
    method node_name() { 'RxCapture' };
    method field_names() { ['pattern'] };
    method field_values() { [$.pattern] };
  };
  class RxGroup is Base {
    has $.match;
    has $.pattern;
    has $.notes;
    
    method newp($match,$pattern) { self.new('match', $match, 'pattern', $pattern) };
    method callback($emitter,$arg) { $emitter.cb__RxGroup(self,$arg) };
    method node_name() { 'RxGroup' };
    method field_names() { ['pattern'] };
    method field_values() { [$.pattern] };
  };
  class RxBlock is Base {
    has $.match;
    has $.block;
    has $.notes;
    
    method newp($match,$block) { self.new('match', $match, 'block', $block) };
    method callback($emitter,$arg) { $emitter.cb__RxBlock(self,$arg) };
    method node_name() { 'RxBlock' };
    method field_names() { ['block'] };
    method field_values() { [$.block] };
  };
  class RxBind is Base {
    has $.match;
    has $.var;
    has $.binding;
    has $.notes;
    
    method newp($match,$var,$binding) { self.new('match', $match, 'var', $var, 'binding', $binding) };
    method callback($emitter,$arg) { $emitter.cb__RxBind(self,$arg) };
    method node_name() { 'RxBind' };
    method field_names() { ['var','binding'] };
    method field_values() { [$.var,$.binding] };
  };
  class RxLiteral is Base {
    has $.match;
    has $.text;
    has $.quote;
    has $.notes;
    
    method newp($match,$text,$quote) { self.new('match', $match, 'text', $text, 'quote', $quote) };
    method callback($emitter,$arg) { $emitter.cb__RxLiteral(self,$arg) };
    method node_name() { 'RxLiteral' };
    method field_names() { ['text','quote'] };
    method field_values() { [$.text,$.quote] };
  };
  class RxSymbol is Base {
    has $.match;
    has $.symbol;
    has $.notes;
    
    method newp($match,$symbol) { self.new('match', $match, 'symbol', $symbol) };
    method callback($emitter,$arg) { $emitter.cb__RxSymbol(self,$arg) };
    method node_name() { 'RxSymbol' };
    method field_names() { ['symbol'] };
    method field_values() { [$.symbol] };
  };
}
