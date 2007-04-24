import ply.yacc as yacc
from MiniPerl.Lex import tokens

def p_class(p):
	"class: CLASS IDENT LBRACE statements RBRACE"
	p[0] = "class %s\n%s\nend" % (p[1], p[3])
def p_statements(p):
	"statements: statement statements"
	p[0] = p[1] + p[2]
def p_statement(p):
	"statement: IDENT IDENT SEMICOLON"
	p[0] = p[1] + p[2]
def p_error(p):
    print "Syntax error in input!"
yacc.yacc()
