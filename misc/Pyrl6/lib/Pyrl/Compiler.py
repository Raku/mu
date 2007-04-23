class Regex(object):
	def compile(the_class, rule_source, param):
		if ('P5' in param) or ('Perl5' in param):
			return RegexPerl5.compile(rule_source, param)
		self = dict(source=rule_source,
			grammar='Pyrl.Grammar.Base',
			ratchet=0, sigspace=0, ignorecase=0)
		self['continue'] = 0
		if 'grammar' in param:
			self['grammar'] = param['grammar']
			del param['grammar']
		if 'ratchet' in param:
			self['ratchet'] = param['ratchet']
			del param['ratchet']
		# TODO: continue adding from regex.pm
class Token(Regex):
	def compile(the_class, rule_source, param={}):
		if not 'ratchet' in param:
			param['ratchet'] = 1
		return Regex.compile(the_class, rule_source, param)
class Rule(Regex):
	def compile(the_class, rule_source, param={}):
		if not 'ratchet' in param:
			param['ratchet'] = 1
		if not ('s' in param) or ('sigspace' in param):
			param['sigspace'] = 1
		return Regex.compile(the_class, rule_source, param)
