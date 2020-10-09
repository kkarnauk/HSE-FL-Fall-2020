import os, sys, glob

current_dir = os.path.dirname(os.path.abspath(__file__))

sys.path.append(os.path.dirname(current_dir) + '/src/')

from parser_impl import parse

def run_tests():
	if run_correct_program_tests() and run_incorrect_program_tests():
		print('All tests passed!')

def run_correct_program_tests():
	result = True

	for file_name in glob.glob(current_dir + '/data/correct_program*.in'):
		program_name, ext = os.path.splitext(file_name)
		
		if not os.path.exists(program_name + '.out'):
			print('Output for test \'{0}\' wasn\'t provided'.format(os.path.basename(file_name)))

			result = False

			continue	

		file = open(file_name, 'r')

		output_file = open(program_name + '.out', 'r')

		if output_file.read().strip('\n') != parse(file.read().strip('\n')):
			print('Test \'{0}\' failed'.format(os.path.basename(file_name)))

			result = False

			continue

	return result


def run_incorrect_program_tests():
	result = True

	for file_name in glob.glob(current_dir + '/data/incorrect_program*.in'):
		file = open(file_name, 'r')

		try:
			parse(file.read())

			print('Test \'{0}\' failed'.format(os.path.basename(file_name)))

			result = False

		except SyntaxError:
			pass

	return result