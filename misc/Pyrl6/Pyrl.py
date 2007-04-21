class Backend(object):
	def __init__(self, code, args):
		self.code = code
		self.args = args
	def run(self):
		tokens = Tokenizer(self.code).tokens()
		ast    = Parser(tokens).parse()
		Environment(ast).run()
class Tokenizer(object):
	def __init__(self, code):
		self.code = code
	def tokens(self):
		return []
class Parser(object):
	def __init__(self, tokens):
		self.tokens = tokens
	def parse(self):
		return []
class Environment(object):
	def __init__(self, ast):
		self.ast = ast
	def run(self):
		print "Running"
