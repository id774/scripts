#!/usr/bin/env python

########################################################################
# image_resize_test.py: Test script for image_resize.py
#
#  Description:
#  This test script contains unit tests for the image_resize.py script.
#  It verifies that the script correctly performs image resizing operations
#  for files in a specified directory. Mock objects are used to simulate
#  file system operations and interactions with the Pillow library.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2024-01-11
#       Initial test script for image_resize.py
#
########################################################################

import os
import subprocess
import sys
import unittest
from unittest.mock import MagicMock, patch

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

try:
    from PIL import Image
    pil_installed = True
except ImportError:
    pil_installed = False

import image_resize


class TestImageResize(unittest.TestCase):
    """ Unit tests for the image_resize.py script. """

    @classmethod
    def setUpClass(cls):
        if not pil_installed:
            raise unittest.SkipTest(
                "PIL library is not installed. Skipping tests.")

    def test_usage_shows_help(self):
        script_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        script_path = os.path.join(script_dir, 'image_resize.py')

        proc = subprocess.Popen(['python3', script_path, '-h'],
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE)
        out, err = proc.communicate()

        self.assertEqual(proc.returncode, 0)
        self.assertIn('Usage:', out.decode('utf-8'))

    @patch('image_resize.print')
    @patch('image_resize.Image.open')
    @patch('image_resize.os.walk')
    def test_read_dir(self, mock_os_walk, mock_image_open, mock_print):
        """ Test the functionality of the read_dir function. """
        mock_print.side_effect = lambda *args, **kwargs: None
        test_size = 300
        test_src = 'test_source_dir'
        test_out = 'test_output_dir'
        test_files = [('root', [], ['image1.jpg', 'image2.png'])]

        # Set up mock behavior
        mock_os_walk.return_value = test_files
        mock_image = MagicMock()
        mock_image.format.lower.return_value = 'jpeg'
        mock_image_open.return_value.__enter__.return_value = mock_image

        # Test read_dir function
        image_resize.read_dir(test_size, test_src, test_out)

        # Assertions to verify expected behavior
        mock_os_walk.assert_called_with(test_src)
        mock_image_open.assert_called()


if __name__ == '__main__':
    unittest.main()
