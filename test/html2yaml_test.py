#!/usr/bin/env python

########################################################################
# html2yaml_test.py: Tests for html2yaml.py Script
#
#  Description:
#  This test suite verifies the functionality of the html2yaml.py script. It checks
#  whether the script correctly parses an HTML document and outputs the expected
#  YAML format. The tests depend on BeautifulSoup and PyYAML libraries. If these
#  libraries are not installed, the tests will be skipped.
#
#  Test Data:
#  The test uses an HTML file located at 'test/html2yaml_data.html', which should contain
#  a sample HTML structure. The expected YAML output is defined within the test cases.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2024-01-11
#       Initial test script for html2yaml.py
#
#  Dependencies:
#  - BeautifulSoup
#  - PyYAML
#  - requests
#
#  Usage:
#  Run the test from the command line:
#    python test/html2yaml_test.py
#
########################################################################

import os
import sys
import unittest

# Check if required libraries are installed
required_libraries_installed = True
try:
    import yaml
    from bs4 import BeautifulSoup
except ImportError:
    required_libraries_installed = False

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from html2yaml import html_to_yaml


class Html2YamlTest(unittest.TestCase):
    """ Unit tests for the html2yaml.py script. """

    @classmethod
    def setUpClass(cls):
        if not required_libraries_installed:
            raise unittest.SkipTest(
                "Required libraries not installed. Skipping tests.")

    def test_html_to_yaml_conversion(self):
        """ Test HTML to YAML conversion for accuracy. """
        test_html_path = os.path.join(
            os.path.dirname(__file__), 'html2yaml_data.html')

        with open(test_html_path, 'r') as file:
            html_content = file.read()

        soup = BeautifulSoup(html_content, 'html.parser')
        yaml_data = html_to_yaml(soup)
        yaml_string = yaml.dump(yaml_data)

        # Expected YAML structure
        expected_structure = """
        children:
        - children:
          - children:
            - name: title
            name: head
          - children:
            - attributes:
                class:
                - content
                id: main
              children:
              - name: h1
              - name: p
              - children:
                - name: li
                - name: li
                - name: li
                name: ul
              name: div
            name: body
          name: html
        name: '[document]'
        """

        # Test if the expected structure is in the generated YAML
        self.assertIn("html", yaml_string)
        self.assertIn("head", yaml_string)
        self.assertIn("title", yaml_string)
        self.assertIn("body", yaml_string)
        self.assertIn("div", yaml_string)
        self.assertIn("ul", yaml_string)
        self.assertIn("li", yaml_string)

        # Convert the expected structure to a YAML object
        expected_yaml = yaml.safe_load(expected_structure)

        # Test if the generated YAML matches the expected structure
        self.assertEqual(yaml_data, expected_yaml)


if __name__ == '__main__':
    unittest.main()
