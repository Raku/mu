import ply.lex as lex

tokens = (
	'SQSTR',
	'IDENT',
	'LBRACE',
	'RBRACE',
	'WHITESPACE',
)

t_SQSTR    = r"\'.*?[^\\]\'"
t_IDENT    = r'\w+'
t_LBRACE   = r'{'
t_RBRACE   = r'}'

def t_WHITESPACE(t):
	r'\s+'
	return None

def inputstr(string):
	lex.input(string)
def tokenslist():
	rv = list()
	while True:
		tok = lex.token()
		if not tok:
			break
		rv.append(tok)
	return rv
def t_error(t):
	print "Illegal character '%s'" % t.value[0]
	t.lexer.skip(1)

lex.lex()
