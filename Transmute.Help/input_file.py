import re

class InputFile:
	def __init__(self, filename):
		self.filename = filename
		self.lines = []

	def header_level(self, line):
		'''Returns the level of a heading'''
		return len(line.split(' ')[0])

	def normalize_headers(self, section):
		'''Adjusts a file's headings down to bring the lowest level to level 1'''

		min_header_level = min(self.header_level(line) for line in self.lines if line.startswith('#'))

		for h in section.headings:
			h.level -= min_header_level - 1

		for i in range(0, len(self.lines)):
			line = self.lines[i]

			if line.startswith('#'):
				split_index = line.index(' ')
				header, rest = line[:split_index], line[split_index+1:]
				header_length = len(header) - min_header_level + 1
				self.lines[i] = '#' * header_length + ' ' + rest

	def resolve_links(self, current_headings, index):
		'''Iterates over each link in the text and adds the filename to each when the section it points to is located in another file.'''

		for i in range(0, len(self.lines)):
			line = self.lines[i]
			link_matches = list(re.finditer('\\[(?P<text>.*?)\\]\\((?P<url>.*?)\\)', line))

			if len(link_matches) == 0:
				continue

			for m in reversed(link_matches):
				url = m.group('url')

				if url.startswith('#') and not any(h for h in current_headings if h.url == url):
					section = [s for s in index if any(h for h in s.headings if h.url == url)]

					if section:
						url = section[0].filename.replace('.md', '.html') + url
						url_start, url_end = m.span('url')
						line = line[:url_start] + url + line[url_end:]

			self.lines[i] = line
