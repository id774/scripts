#!/usr/bin/env python

########################################################################
# test/wget_test.py: Test suite for wget.py
#
#  Description:
#  This test suite verifies the functionality of the wget.py script.
#  It uses mocking to simulate network requests and file operations
#  to avoid actual downloads during testing. The suite covers various
#  edge cases, such as normal downloads, HTTP errors, missing arguments,
#  file overwrites, and network timeouts, ensuring that the script behaves
#  as expected in different scenarios without accessing external resources
#  or modifying the real file system.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2025-01-10
#       Initial release. Test suite for wget.py script.
#
########################################################################

import os
import sys
import unittest
from unittest.mock import MagicMock, mock_open, patch

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import requests

from wget import download_file, usage


class TestWget(unittest.TestCase):
    """Test suite for wget.py"""

    @patch('wget.requests.get')
    @patch('builtins.open', new_callable=mock_open)
    def test_download_file(self, mock_open_file, mock_requests_get):
        """
        Test case: Normal file download.
        This test verifies that:
        - The file is correctly downloaded from the given URL.
        - The downloaded content is saved locally with the correct filename.
        """
        # Mock response content for the GET request
        mock_requests_get.return_value = MagicMock(status_code=200, content=b'Test content')

        # Call the download function
        download_file("http://example.com/testfile.txt")

        # Verify that the file is opened for writing in binary mode
        mock_open_file.assert_called_once_with("testfile.txt", 'wb')

        # Verify that the file's content is written correctly
        mock_open_file().write.assert_called_once_with(b'Test content')

    @patch('wget.requests.get')
    def test_http_error(self, mock_requests_get):
        """
        Test case: HTTP error during file download.
        This test verifies that:
        - HTTP errors such as 404 or 500 are properly raised as exceptions.
        """
        # Mock a RequestException to simulate an HTTP error
        mock_requests_get.side_effect = requests.exceptions.RequestException("HTTP Error")

        # Verify that the function raises the correct exception
        with self.assertRaises(requests.exceptions.RequestException):
            download_file("http://example.com/testfile.txt")

    @patch('wget.requests.get')
    def test_invalid_url(self, mock_requests_get):
        """
        Test case: Invalid URL provided.
        This test verifies that:
        - An invalid URL raises a MissingSchema exception.
        """
        # Mock a MissingSchema exception for invalid URLs
        mock_requests_get.side_effect = requests.exceptions.MissingSchema("Invalid URL")

        # Verify that the function raises the correct exception
        with self.assertRaises(requests.exceptions.MissingSchema):
            download_file("invalid-url")

    @patch('builtins.print')
    def test_usage(self, mock_print):
        """
        Test case: Missing command-line arguments.
        This test verifies that:
        - The usage message is displayed when no URL is provided.
        - The script exits with status code 1.
        """
        # Verify that the script exits and prints the usage message
        with self.assertRaises(SystemExit) as cm:
            usage()
        self.assertEqual(cm.exception.code, 1)

        # Verify that the correct usage message is printed
        mock_print.assert_called_once_with("Usage: {} <URL>".format(sys.argv[0]))

    @patch('wget.requests.get')
    @patch('builtins.open', new_callable=mock_open)
    def test_file_overwrite(self, mock_open_file, mock_requests_get):
        """
        Test case: File overwrite.
        Verifies that existing files are overwritten with new content.
        """
        mock_requests_get.return_value.content = b'New content'
        mock_requests_get.return_value.status_code = 200

        download_file("http://example.com/testfile.txt")

        mock_open_file.assert_called_once_with("testfile.txt", 'wb')
        mock_open_file().write.assert_called_once_with(b'New content')

    @patch('wget.requests.get')
    def test_network_timeout(self, mock_requests_get):
        """
        Test case: Network timeout during download.
        Ensures the script handles timeouts gracefully.
        """
        mock_requests_get.side_effect = requests.exceptions.Timeout

        with self.assertRaises(requests.exceptions.Timeout):
            download_file("http://example.com/timeoutfile.txt")


if __name__ == '__main__':
    unittest.main()
