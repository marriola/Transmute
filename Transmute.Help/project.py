import json
import os.path
import re
import subprocess

from heading import Heading
from input_file import InputFile
from section import Section
from split_point import SplitPoint

class Project:
	current_headings = None
	current_lines = None
	last_heading = None
	index = []
	files = []
	splits = []

	def __init__(self, file_path):
		with open(file_path) as f:
			project = json.load(f)

		self.source = project['source']
		self.template = project['template']
		self.title_prefix = project['titlePrefix']
		self.author = project['author']

		for section in project['sections']:
			self.splits.append(SplitPoint(section['output'], section['title'], section['heading']))

		self.index.append(Section(self.splits[0].filename, self.splits[0].title))
		self.current_headings = self.index[0].headings

		self.files.append(InputFile(self.splits[0].filename))
		self.current_lines = self.files[0].lines

	def to_dict(self, heading, filename):
		return {
			'Heading': heading.title,
			'Path': filename + heading.url.replace('.md', '.html'),
			'Children': [self.to_dict(c, filename) for c in heading.children]
		}

	def handle_header(self, line):
		'''Record the heading for the section, and end the section if at a defined split point.'''

		space_index = line.index(' ')
		level, header = line[:space_index], line[space_index+1:].strip()
		split_point = next((sp for sp in self.splits if sp.heading == header), None)

		if split_point:
			self.index.append(Section(split_point.filename, split_point.title))
			self.current_headings = self.index[-1].headings
			
			self.files.append(InputFile(split_point.filename))
			self.current_lines = self.files[-1].lines
			self.last_heading = None

		header_url = '#' + re.sub('[^a-zA-Z0-9 _-]', '', header.lower().replace(' ', '-'))
		next_heading = Heading(len(level), header, header_url)
		# print(len(level), header)

		if self.last_heading:
			if next_heading.level > self.last_heading.level:
				self.stack.append(self.current_headings)
				self.current_headings = self.last_heading.children
			elif next_heading.level < self.last_heading.level:
				# TODO make sure we return to the right level when going up multiple levels
				self.current_headings = self.stack.pop()

		self.current_headings.append(next_heading)
		self.last_heading = next_heading
		# print([h.title for h in self.current_headings])

	def process_source(self):
		'''Processes the source file'''
		self.stack = []

		with open(self.source) as f:
			for line in f:
				if line.startswith('#'):
					self.handle_header(line)
				self.current_lines.append(line)

	def write_table_of_contents(self):
		'''Writes the table of contents JSON file'''
		headings_export = []

		for file in self.index:
			filename = file.filename.replace('.md', '.html')
			children = next((h.children for h in file.headings if len(h.children) > 0), [])

			headings_export.append({
				'Path': filename,
				'Heading': file.title,
				'Children': [self.to_dict(c, filename) for c in children]
			})

		toc_path = os.path.join('out', 'toc.json')

		with open(toc_path, 'w', encoding='utf-8') as f:
			json.dump(headings_export, f, ensure_ascii=False, indent=4)

	def write_parts(self):
		'''Writes the individual markdown files'''

		for file, section in zip(self.files, self.index):
			file.normalize_headers(section)
			file.resolve_links(section.headings, self.index)

			out_path = os.path.join('out', file.filename)

			with open(out_path, 'w') as out:
				for line in file.lines:
					out.write(line)

	def compile(self):
		'''Converts the split Markdown files to HTML using pandoc'''
		
		self.process_source()
		self.write_table_of_contents()
		self.write_parts()
		print()

		for file, section in zip(self.files, self.index):
			filename = os.path.join('out', file.filename)
			base_filename, _ = os.path.splitext(filename)
			out_filename = base_filename + '.html'

			args = [
				'pandoc',
				filename,
				'-o', out_filename,
				'-f', 'markdown',
				'-t', 'html',
				'--lua-filter', 'nowidths.lua'
			]

			if self.template:
				args.extend(['--template', self.template])

			if self.author:
				args.extend(['-M', f'author-meta="{self.author}"'])

			if self.title_prefix:
				args.extend(['-M', f'title-prefix="{self.title_prefix}"'])

			if section.title:
				args.extend(['-M', f'pagetitle="{section.title}"'])

			print(' '.join(args))
			result = subprocess.run(args)

			if result.returncode != 0:
				print('return code =', result.returncode, '\n')
				return

		print()

	def print_table_of_contents(self):
		def inner(section, level=0):
			for heading in section.children:
				print('  ' * level, f'{heading.title} ({heading.url})')
				inner(heading, level + 1)

		for file in self.index:
			print(file.filename)
			for heading in file.headings:
				print(f'  {heading.title} ({heading.url})')
				inner(heading, 2)
