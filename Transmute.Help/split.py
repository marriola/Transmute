import json
import os.path
import re
import sys

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

current_headings = None
current_lines = None
last_heading = None
input_file = None
index = []
files = []
splits = []

if len(sys.argv) < 2:
	exit(0)
	
with open(sys.argv[1]) as f:
	project = json.load(f)
	input_file = project['source']
	splits = []			

	for i, section in enumerate(project['sections']):
		splits.append(Split(section['output'], section['title'], section['heading']))

with open(input_file) as f:
	split_point = splits[0]

	index.append(Section(split_point.filename, split_point.title))
	current_headings = index[0].headings
	
	files.append(InputFile(split_point.filename))
	current_lines = files[0].lines
	
	def header_level(line):
		return len(line.split(' ')[0])
		
	def to_dict(heading, filename):
		return {
			'Heading': heading.title,
			'Path': filename + heading.url.replace('.md', '.html'),
			'Children': [to_dict(c, filename) for c in heading.children]
		}
		
	def write_headings():
		headings_export = []
	
		for file in index:
			filename = file.filename.replace('.md', '.html')
			children = next((h.children for h in file.headings if len(h.children) > 0), [])
		
			headings_export.append({
				'Path': filename,
				'Heading': file.title,
				'Children': [to_dict(c, filename) for c in children]
			})

		with open('out/toc.json', 'w', encoding='utf-8') as f:
			json.dump(headings_export, f, ensure_ascii=False, indent=4)
	
	def normalize_headers(file, index):
		'''Adjusts a file's headings down to bring the lowest level to level 1'''
	
		min_header_level = min(header_level(line) for line in file.lines if line.startswith('#'))
		
		for h in index.headings:
			h.level -= min_header_level - 1

		for i in range(0, len(file.lines)):
			line = file.lines[i]
			
			if line.startswith('#'):
				split_index = line.index(' ')
				header, rest = line[:split_index], line[split_index+1:]
				header_length = len(header) - min_header_level + 1
				file.lines[i] = '#' * header_length + ' ' + rest
				
	def write_all():
		'''Writes the individual markdown files and the headings file'''

		for i, file in enumerate(files):
			normalize_headers(file, index[i])

			for j in range(0, len(file.lines)):
				file.lines[j] = handle_links(file.lines[j], index[i].headings)

			with open(os.path.join('out', file.filename), 'w') as out:
				for line in file.lines:
					out.write(line)
					
		write_headings()
		
	stack = []
					
	def handle_header(line):
		'''Record the heading for the section, and end the section if at a defined split point.'''

		global current_headings, current_lines, last_heading
		
		space_index = line.index(' ')
		level, header = line[:space_index], line[space_index+1:].strip()
		split_point = next((sp for sp in splits if sp.heading == header), None)

		if split_point:
			index.append(Section(split_point.filename, split_point.title))
			current_headings = index[-1].headings
			
			files.append(InputFile(split_point.filename))
			current_lines = files[-1].lines
			last_heading = None

		header_url = '#' + re.sub('[^a-zA-Z0-9 _-]', '', header.lower().replace(' ', '-'))
		next_heading = Heading(len(level), header, header_url)
		# print(len(level), header)
		
		if last_heading:
			if next_heading.level > last_heading.level:
				stack.append(current_headings)
				current_headings = last_heading.children
			elif next_heading.level < last_heading.level:
				# TODO make sure we return to the right level when going up multiple levels
				current_headings = stack.pop()
		
		current_headings.append(next_heading)
		last_heading = next_heading
		# print([h.title for h in current_headings])
			
		
	def handle_links(line, current_headings):
		'''Iterates over each link in the text and adds the filename to each when the section it points to is located in another file.'''
	
		link_matches = list(re.finditer('\[(?P<text>.*?)\]\((?P<url>.*?)\)', line))

		if len(link_matches) == 0:
			return line
	
		for m in reversed(link_matches):
			url = m.group('url')
		
			if url.startswith('#') and not any(h for h in current_headings if h.url == url):
				section = [s for s in index if any(h for h in s.headings if h.url == url)]

				if section:
					url = section[0].filename.replace('.md', '.html') + url
					url_start, url_end = m.span('url')
					line = line[:url_start] + url + line[url_end:]
				
		return line

	for line in f:
		if line.startswith('#'):
			handle_header(line)
		current_lines.append(line)
		
	write_all()
	
def dump_headings():
	def inner(section, level=0):
		for heading in section.children:
			print('  ' * level, f'{heading.title} ({heading.url})')
			inner(heading, level + 1)

	for file in index:
		print(file.filename)
		for heading in file.headings:
			print(f'  {heading.title} ({heading.url})')
			inner(heading, 2)

dump_headings()