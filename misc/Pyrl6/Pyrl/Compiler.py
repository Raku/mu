import sys

class Regex(object):
	def compile(self, rule_source, param):
		if ('P5' in param) or ('Perl5' in param):
			return RegexPerl5.compile(rule_source, param)
		self = dict(source=rule_source,
			grammar='Pyrl.Grammar.Base',
			ratchet=0, sigspace=0, ignorecase=0)
		self.cont = 0
		if 'grammar' in param:
			self.grammar = param['grammar']
			del param['grammar']
		if 'ratchet' in param:
			self.ratchet = param['ratchet']
			del param['ratchet']
		if 'c' in param:
			self.cont = param['c']
			del param['c']
		if 'continue' in param:
			self.cont = param['continue']
			del param['continue']
		if 'i' in param:
			self.ignorecase = param['i']
			del param['i']
		if 'ignorecase' in param:
			self.ignorecase = param['ignorecase']
			del param['ignorecase']
		if 's' in param:
			del param['s']
		for key in param.keys():
			sys.stderr.write("Error in rule: unknown parameter %s\n" % key)
		# i left caching out this this time around.
		# todo: write the rest.
	def code(self):
		def foo(grammar, string, flags, state):
			self.match(string, grammar, flags, state)
		return foo
	def match(self, string, grammar, flags, state):
		if not string: return Pyrl.Runtime.Match(dict(bool=0))
		if type(grammar) == type(dict()):
			state = flags
			flags = grammar
			grammar = flags['grammar']
		if not grammar: grammar = self.grammar
		p = 'p' in flags and flags['p'] or
			'pos' in flags and flags['pos'] or
			self.p
		cont = 'c' in flags and flags['c'] or
			'cont' in flags and flags['cont'] or
			'continue' in flags and flags['continue'] or
			self.cont
		ignorecase = 'i' in flags and flags['i'] or
			'ignorecase' in flags and flags['ignorecase'] or
			self.ignorecase
		# self.code (rule->{code}) no longer handled.
		# TODO: continue this
class Token(Regex):
	def compile(self, rule_source, param={}):
		if not 'ratchet' in param:
			param['ratchet'] = 1
		return Regex.compile(self, rule_source, param)
class Rule(Regex):
	def compile(self, rule_source, param={}):
		if not 'ratchet' in param:
			param['ratchet'] = 1
		if not ('s' in param) or ('sigspace' in param):
			param['sigspace'] = 1
		return Regex.compile(self, rule_source, param)
