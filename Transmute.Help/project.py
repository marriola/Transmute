import json
import os.path
import re
import subprocess

class InputFile:
	def __init__(self, filename):
		self.filename = filename
		self.lines = []

class Section:
	def __init__(self, filename, title):
		self.filename = filename
		self.title = title
		self.headings = []

class Heading:
	def __init__(self, level, title, url):
		self.level = level
		self.title = title
		self.url = url
		self.children = []

class Split:
	def __init__(self, filename, title, heading):
		self.filename = filename
		self.title = title
		self.heading = heading

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
			self.splits.append(Split(section['output'], section['title'], section['heading']))

		split_point = self.splits[0]

		self.index.append(Section(split_point.filename, split_point.title))
		self.current_headings = self.index[0].headings

		self.files.append(InputFile(split_point.filename))
		self.current_lines = self.files[0].lines

	def header_level(self, line):
		'''Returns the level of a heading'''
		return len(line.split(' ')[0])

	def to_dict(self, heading, filename):
		return {
			'Heading': heading.title,
			'Path': filename + heading.url.replace('.md', '.html'),
			'Children': [self.to_dict(c, filename) for c in heading.children]
		}

	def normalize_headers(self, file, index):
		'''Adjusts a file's headings down to bring the lowest level to level 1'''

		min_header_level = min(self.header_level(line) for line in file.lines if line.startswith('#'))

		for h in index.headings:
			h.level -= min_header_level - 1

		for i in range(0, len(file.lines)):
			line = file.lines[i]

			if line.startswith('#'):
				split_index = line.index(' ')
				header, rest = line[:split_index], line[split_index+1:]
				header_length = len(header) - min_header_level + 1
				file.lines[i] = '#' * header_length + ' ' + rest

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
		# print([h.title for h in current_headings])

	def handle_links(self, line, current_headings):
		'''Iterates over each link in the text and adds the filename to each when the section it points to is located in another file.'''

		link_matches = list(re.finditer('\\[(?P<text>.*?)\\]\\((?P<url>.*?)\\)', line))

		if len(link_matches) == 0:
			return line

		for m in reversed(link_matches):
			url = m.group('url')

			if url.startswith('#') and not any(h for h in current_headings if h.url == url):
				section = [s for s in self.index if any(h for h in s.headings if h.url == url)]

				if section:
					url = section[0].filename.replace('.md', '.html') + url
					url_start, url_end = m.span('url')
					line = line[:url_start] + url + line[url_end:]

		return line

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
			self.normalize_headers(file, section)

			for i in range(0, len(file.lines)):
				file.lines[i] = self.handle_links(file.lines[i], section.headings)

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
