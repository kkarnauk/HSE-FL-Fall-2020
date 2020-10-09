import sys

import lexer_impl as lexer

try:
	file = open(sys.argv[1], 'r')
except IOError:
	print("File '{0}' doesn't exist.".format(sys.argv[1]))
	exit(1)

results = lexer.lex(file.read())

file = open(file.name + ".out", 'w')

for result in results:
	file.write(result + '\n')