#!/usr/bin/env python

########################################################################
# insta_downloader_test.py: Test suite for insta_downloader.py
#
#  Description:
#  This test suite verifies the functionality of the insta_downloader.py
#  script. It uses mocking to simulate network requests and file operations
#  to avoid actual downloads during testing. The suite tests various edge
#  cases, including empty profiles, successful downloads, and the handling
#  of multiple images in a post. This ensures the script behaves as expected
#  in different scenarios without affecting external systems or generating
#  unwanted network traffic.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2025-01-06
#       Initial release. Test suite for insta_downloader.py script.
#
########################################################################

import os
import sys
import unittest
from unittest.mock import MagicMock, patch

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from insta_downloader import InstagramPhotoDownloader


class TestInstagramPhotoDownloader(unittest.TestCase):
    """Test suite for InstagramPhotoDownloader."""

    @patch('insta_downloader.instaloader.Profile.from_username')
    @patch('insta_downloader.urllib.request.urlretrieve')
    @patch('insta_downloader.os.chmod')
    def test_download_images(self, mock_chmod, mock_urlretrieve, mock_from_username):
        """
        Test downloading a single image post.
        Simulates a profile with one post and verifies the download logic.
        """
        # Mock instaloader profile with a single post
        mock_profile = MagicMock()
        mock_profile.get_posts.return_value = [
            MagicMock(typename="GraphImage", url="http://example.com/image1.jpg",
                      date="2024-01-01", shortcode="ABC", get_sidecar_nodes=MagicMock(return_value=[]))
        ]
        mock_from_username.return_value = mock_profile

        # Initialize downloader with mocked data
        downloader = InstagramPhotoDownloader("test_user", permissions=0o644, sleep_time=0)

        # Mock _get_instagram_photo_urls to simulate post URLs
        downloader._get_instagram_photo_urls = MagicMock(return_value=[
            ("http://example.com/image1.jpg", "2024-01-01", "ABC", 1)
        ])

        # Suppress stdout to avoid progress messages during test
        with patch('sys.stdout', new_callable=MagicMock()):
            with patch('os.listdir', return_value=[]):
                downloader.download()

        # Check if the correct download and file permission methods were called
        mock_urlretrieve.assert_called_with("http://example.com/image1.jpg", "test_user_ABC_01.jpg")
        mock_chmod.assert_called_with("test_user_ABC_01.jpg", 0o644)

    @patch('insta_downloader.instaloader.Profile.from_username')
    def test_no_posts(self, mock_from_username):
        """
        Test behavior when no posts are available.
        Ensures the script exits gracefully without attempting downloads.
        """
        # Mock empty profile
        mock_profile = MagicMock()
        mock_profile.get_posts.return_value = []
        mock_from_username.return_value = mock_profile

        # Initialize downloader
        downloader = InstagramPhotoDownloader("test_user")

        # Mock _get_instagram_photo_urls to return empty list
        downloader._get_instagram_photo_urls = MagicMock(return_value=[])

        # Suppress stdout to avoid progress messages during test
        with patch('sys.stdout', new_callable=MagicMock()):
            with patch('os.listdir', return_value=[]):
                downloader.download()

    @patch('insta_downloader.instaloader.Profile.from_username')
    @patch('insta_downloader.urllib.request.urlretrieve')
    @patch('insta_downloader.InstagramPhotoDownloader._download_and_save_image')
    def test_multiple_images_in_post(self, mock_download_and_save_image, mock_urlretrieve, mock_from_username):
        """
        Test handling of a post with multiple images.
        Ensures each image in the post is processed and downloaded correctly.
        """
        # Mock instaloader profile with a multi-image post
        mock_profile = MagicMock()
        mock_profile.get_posts.return_value = [
            MagicMock(typename="GraphSidecar", get_sidecar_nodes=MagicMock(return_value=[
                MagicMock(display_url="http://example.com/image1.jpg"),
                MagicMock(display_url="http://example.com/image2.jpg")
            ]), date="2024-01-02", shortcode="DEF")
        ]
        mock_from_username.return_value = mock_profile

        # Initialize downloader
        downloader = InstagramPhotoDownloader("test_user", sleep_time=0)

        # Mock _get_instagram_photo_urls
        downloader._get_instagram_photo_urls = MagicMock(return_value=[
            ("http://example.com/image1.jpg", "2024-01-02", "DEF", 1),
            ("http://example.com/image2.jpg", "2024-01-02", "DEF", 2)
        ])

        # Suppress stdout
        with patch('sys.stdout', new_callable=MagicMock()):
            with patch('os.listdir', return_value=[]):
                downloader.download()

        # Verify _download_and_save_image was called for each image
        mock_download_and_save_image.assert_any_call("http://example.com/image1.jpg", "test_user_DEF_01.jpg")
        mock_download_and_save_image.assert_any_call("http://example.com/image2.jpg", "test_user_DEF_02.jpg")


if __name__ == '__main__':
    unittest.main()
