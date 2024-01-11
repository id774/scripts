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
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2024-01-11
#       Initial test script for image_resize.py
#
########################################################################

import unittest
from unittest.mock import patch, MagicMock
import sys
import os

try:
    from PIL import Image
    pil_installed = True
except ImportError:
    pil_installed = False

# Adjusting the path to import image_resize from the parent directory
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
import image_resize

@unittest.skipUnless(pil_installed, "PIL library is not installed. Skipping tests.")
class TestImageResize(unittest.TestCase):
    """Unit tests for the image_resize.py script."""

    @patch('image_resize.print')
    @patch('image_resize.Image.open')
    @patch('image_resize.os.walk')
    def test_read_dir(self, mock_os_walk, mock_image_open, mock_print):
        """Test the functionality of the read_dir function."""
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
