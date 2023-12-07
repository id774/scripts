#!/usr/bin/env python

"""
html2yaml.py: HTML to YAML Converter Script

Description:
This script parses an HTML document and converts it into a structured
YAML format. It uses Nokogiri for HTML parsing and provides options for
including or excluding specific elements based on CSS or XPath selectors.
This is useful for converting HTML structures into a format that can be
easily processed by scripts or programs.

Author: id774 (More info: http://id774.net)
Source Code: https://github.com/id774/scripts
License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
Contact: idnanashi@gmail.com

Dependencies:
- BeautifulSoup
- PyYAML
- requests

Usage:
Run the script with the URL or path of the HTML file, along with optional
arguments for inclusion or exclusion of elements:
  html2yaml.py [options] URL_OR_PATH

Options:
  -i, --include=XPATH_OR_CSS  Include elements matching the given selector
  -x, --exclude=XPATH_OR_CSS  Exclude elements matching the given selector

Example:
  html2yaml.py --exclude='//div[@class="footer"]' http://example.com
  This command will parse the HTML from example.com, excluding divs with class "footer".

Input/Output Example:
- Given an HTML document like:
  <!DOCTYPE html>
  <html>
  <head>
      <title>Example Page</title>
  </head>
  <body>
      <div id="main" class="container">
          <h1>Welcome to the Example Page</h1>
          <p>This is a sample paragraph.</p>
      </div>
  </body>
  </html>
- The output will be a YAML format representing the structure of the HTML:
  :name: html
  :children:
    - :name: head
      :children:
        - :name: title
          :children:
            - :name: text
              :value: Example Page
    - :name: body
      :children:
        - :name: div
          :attributes:
            :id: main
            :class: container
          :children:
            - :name: h1
              :children:
                - :name: text
                  :value: Welcome to the Example Page
            - :name: p
              :children:
                - :name: text
                  :value: This is a sample paragraph.

"""

import sys
import yaml
from bs4 import BeautifulSoup
import requests

def html_to_yaml(element):
    """ Convert HTML element to YAML-structured data. """
    data = {'name': element.name}

    if element.attrs:
        data['attributes'] = element.attrs

    children = [html_to_yaml(child)
                for child in element.children if child.name]
    if children:
        data['children'] = children

    return data

def main():
    if len(sys.argv) != 2:
        print("Usage: python html2yaml.py URL_OR_PATH")
        sys.exit(1)

    source = sys.argv[1]

    try:
        if source.startswith('http://') or source.startswith('https://'):
            response = requests.get(source)
            html = BeautifulSoup(response.text, 'html.parser')
        else:
            with open(source, 'r') as file:
                html = BeautifulSoup(file, 'html.parser')

        yaml_data = html_to_yaml(html)
        print(yaml.dump(yaml_data))
    except Exception as e:
        print(f"Error processing HTML: {e}")
        sys.exit(2)


if __name__ == "__main__":
    main()
