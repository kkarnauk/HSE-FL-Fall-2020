import sys

import ply.lex as lexer

def lex(program_text):
	reserved = {
		'module' : 'MODULE',
		'sig'    : 'SIG',
		'type'   : "TYPE"
	}

	tokens = [
		'NUM',   #Number
		'OPER',  #Operator
		'DELIM', #Delimiter
		'LITER', #Literal
		'IDENT', #Identificator
	] + list(reserved.values())

	def t_IDENT(token):
		r'[a-zA-Z_][a-zA-Z_0-9]*' #Any digit is not allowed to be the first symbol

		token.type = reserved.get(token.value, 'IDENT')
		return token

	def t_NUM(token):
		r'[0-9]+'

		token.value = int(token.value)
		return token

	def t_OPER(token):
		r'(->)|(:-)'

		token.value = "'{0}'".format(token.value)
		return token

	def t_DELIM(token):
		r'[,\.\[\]|]'

		token.value = "'{0}'".format(token.value)
		return token

	t_LITER = r'"[^"]*"'

	t_ignore = ' \t'

	def t_newline(token):
		r'\n+'

		token.lexer.lineno += len(token.value)

	def t_error(token):
		print('Illegal character {0}'.format(token.value[0]))
		token.lexer.skip(1)


	program_lexer = lexer.lex()
	program_lexer.input(program_text)

	results = []

	while True:
		token = program_lexer.token()

		if not token:
			break

		results.append('{0}, {1}, {2}, {3}'.format(token.type, token.value, token.lineno, token.lexpos))

	return results