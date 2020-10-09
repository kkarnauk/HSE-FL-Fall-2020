#!/usr/bin/env python3

import os, sys

# Sorry for relative paths, but I'm not really good with python...
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))) + '/test/')

from parser_impl import parse
from tests import run_tests

if len(sys.argv) < 2:
	print('Please, pass arguments.')

	exit()

if len(sys.argv) > 2:
	if sys.argv[1] == 'run' and sys.argv[2] == 'tests':
		run_tests()
	else:
		print('Unknown command.')

	exit()

input_file = open(sys.argv[1], 'r')

program_text = input_file.read()

try:
	result = parse(program_text)

	output_file = open(input_file.name + '.out', 'w')

	output_file.write(str(result))

except SyntaxError:
	print('SyntaxError')
