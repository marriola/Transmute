import sys

from project import Project

if len(sys.argv) < 2:
	exit(0)
	
project = Project(sys.argv[1])

project.compile()
project.print_table_of_contents()
