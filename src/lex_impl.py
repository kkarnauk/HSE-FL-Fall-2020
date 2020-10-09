import ply.lex as lex 
import sys

tokens = [
  'IDENTIFIER',
  'TURNSTILE',
  'COMMA',
  'SEMICOLON',
  'LEFTBRACKET',
  'RIGHTBRACKET',
  'DOT'
]

t_IDENTIFIER = r'[a-zA-Z_][a-zA-Z_0-9]*'
t_TURNSTILE = r':-'
t_COMMA = ','
t_SEMICOLON = ';'
t_LEFTBRACKET = '\('
t_RIGHTBRACKET = '\)'
t_DOT = '\.'

t_ignore = ' \t'

def t_newline(t): 
  r'\n+'
  t.lexer.lineno += len(t.value)

def t_error(t): 
  raise SyntaxError

lexer = lex.lex() 
