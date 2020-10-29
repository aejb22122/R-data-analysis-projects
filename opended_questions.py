#!/usr/bin/env python
# Given a .docx file, extract a CSV list of all tagged (commented) text
# This is version 6.0 of the script
# Date: 12 February 2020

import zipfile
import csv
from bs4 import BeautifulSoup as Soup
import tkinter as tk
from tkinter import filedialog
import re

# Show file selection dialog box
root = tk.Tk()
root.withdraw()
paths = filedialog.askopenfilenames()
root.update()

with open('/'.join(paths[0].split('/')[0:-1])+'/output.csv', 'w', newline='', encoding='utf-8-sig') as f:
	csvw = csv.writer(f)
	# loop through each selected file
	for path in paths:
		# Write a header line with the filename
		csvw.writerow([path.split('/')[-1], ''])
		# .docx files are really ZIP files with a separate 'file' within them for the document
		# itself and the text of the comments. This unzips the file and parses the comments.xml
		# file within it, which contains the comment (label) text
		unzip = zipfile.ZipFile(path)
		comments = Soup(unzip.read('word/comments.xml'), 'lxml')
		# The structure of the document itself is more complex and we need to do some
		# preprocessing to handle multi-paragraph and nested comments, so we unzip
		# it into a string first
		doc = unzip.read('word/document.xml').decode()
		# Find all the comment start and end locations and store them in dictionaries
		# keyed on the unique ID for each comment
		start_loc = {x.group(1): x.start() for x in re.finditer(r'<w:commentRangeStart.*?w:id="(.*?)"', doc)}
		end_loc = {x.group(1): x.end() for x in re.finditer(r'<w:commentRangeEnd.*?w:id="(.*?)".*?>', doc)}
		# loop through all the comments in the comments.xml file
		for c in comments.find_all('w:comment'):
			c_id = c.attrs['w:id']
			# Use the locations we found earlier to extract the xml fragment from the document for
			# each comment ID, adding spaces to separate any paragraphs in multi-paragraph comments
			xml = re.sub(r'(<w:p .*?>)', r'\1 ', doc[start_loc[c_id]:end_loc[c_id] + 1])
			# Parse the XML fragment, extract any text and write to file along with the label text
			csvw.writerow([''.join(c.findAll(text=True)), ''.join(Soup(xml, 'lxml').findAll(text=True))])
		unzip.close()
