#!/usr/bin/env python

########################################################################
# html2yaml.py: HTML to YAML Converter Script
#
#  Description:
#  This script parses an HTML document and converts it into a structured
#  YAML format. It uses BeautifulSoup for HTML parsing and PyYAML for
#  YAML generation. The script provides options for including or excluding
#  specific elements based on CSS or XPath selectors. This is useful for
#  converting HTML structures into a format that can be easily processed
#  by scripts or programs. Now, it checks for the necessary libraries
#  and exits with an error message if they are not installed.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.2 2024-01-11
#       Added checks for BeautifulSoup and PyYAML libraries.
#  v1.1 2023-12-08
#       Removed f-strings for compatibility with Python versions below 3.6.
#  v1.0 2023-12-07
#       Initial release.
#
#  Dependencies:
#  - BeautifulSoup
#  - PyYAML
#  - requests
#
#  Notes:
#  This script requires the following Python libraries: BeautifulSoup,
#  PyYAML, and requests. These can be installed via pip using:
#    pip install bs4 pyyaml requests
#
#  Usage:
#  Run the script with the URL or path of the HTML file, along with optional
#  arguments for inclusion or exclusion of elements:
#    html2yaml.py [options] URL_OR_PATH
#
#  Options:
#    -i, --include=XPATH_OR_CSS  Include elements matching the given selector
#    -x, --exclude=XPATH_OR_CSS  Exclude elements matching the given selector
#
#  Example:
#    html2yaml.py --exclude='//div[@class="footer"]' http://example.com
#
########################################################################

import sys

# Importing necessary libraries
try:
    import requests
    import yaml
    from bs4 import BeautifulSoup
except ImportError as e:
    libraries_installed = False
else:
    libraries_installed = True

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
    if not libraries_installed:
        print("[ERROR] Required libraries not installed.", file=sys.stderr)
        sys.exit(1)

    if len(sys.argv) != 2:
        print("[INFO] Usage: python html2yaml.py URL_OR_PATH")
        sys.exit(0)

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
        print("[ERROR] Error processing HTML: {}".format(e), file=sys.stderr)
        sys.exit(2)


if __name__ == "__main__":
    main()
