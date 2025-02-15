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
#  v1.0 2025-01-09
#       Initial release. Test suite for insta_downloader.py script.
#
########################################################################

import os
import sys
import unittest
from datetime import datetime
from unittest.mock import MagicMock, patch

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

# Attempt to import insta_downloader and set a flag if unavailable
try:
    from insta_downloader import InstagramPhotoDownloader
    HAS_INSTA_DOWNLOADER = True
except ImportError:
    HAS_INSTA_DOWNLOADER = False


@unittest.skipIf(not HAS_INSTA_DOWNLOADER, "insta_downloader module is not available")
class TestInstagramPhotoDownloader(unittest.TestCase):
    """Test suite for InstagramPhotoDownloader."""

    def setUp(self):
        """Set up mocks for all tests."""
        if not HAS_INSTA_DOWNLOADER:
            self.skipTest("insta_downloader module is not available")

        try:
            import instaloader
        except ModuleNotFoundError:
            self.skipTest("instaloader module is not installed")

        # Mock Profile.from_username globally for all tests
        patcher_profile = patch('instaloader.Profile.from_username', autospec=True)
        self.addCleanup(patcher_profile.stop)
        self.mock_from_username = patcher_profile.start()
        self.mock_profile = MagicMock()
        self.mock_profile.get_posts.return_value = []
        self.mock_from_username.return_value = self.mock_profile

        # Mock instaloader.Instaloader globally for all tests
        patcher_loader = patch('instaloader.Instaloader', autospec=True)
        self.addCleanup(patcher_loader.stop)
        self.mock_instaloader = patcher_loader.start()
        self.mock_loader = self.mock_instaloader.return_value

        # Set default behavior for the mock loader
        self.mock_loader.context = MagicMock()

    @patch('insta_downloader.time.sleep', return_value=None)
    @patch('insta_downloader.os.chmod')
    @patch('insta_downloader.urllib.request.urlretrieve')
    @patch('insta_downloader.instaloader.Profile.from_username')
    def test_download_images(self, mock_from_username, mock_urlretrieve, mock_chmod, mock_sleep):
        """
        Test downloading a single image post.
        Simulates a profile with one post and verifies the download logic.
        """
        # Mock instaloader profile with a single post
        mock_profile = MagicMock()
        mock_profile.get_posts.return_value = [
            MagicMock(typename="GraphImage", url="http://example.com/image1.jpg",
                      date=datetime(2024, 1, 1), shortcode="ABC", get_sidecar_nodes=MagicMock(return_value=[]))
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

    @patch('insta_downloader.time.sleep', return_value=None)
    @patch('insta_downloader.instaloader.Profile.from_username')
    def test_no_posts(self, mock_from_username, mock_sleep):
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

    @patch('insta_downloader.time.sleep', return_value=None)
    @patch('insta_downloader.InstagramPhotoDownloader._download_and_save_image')
    @patch('insta_downloader.urllib.request.urlretrieve')
    @patch('insta_downloader.instaloader.Profile.from_username')
    def test_multiple_images_in_post(self, mock_from_username, mock_urlretrieve, mock_download_and_save_image, mock_sleep):
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
        self.assertEqual(mock_download_and_save_image.call_count, 2)
        mock_download_and_save_image.assert_any_call("http://example.com/image1.jpg", "test_user_DEF_01.jpg")
        mock_download_and_save_image.assert_any_call("http://example.com/image2.jpg", "test_user_DEF_02.jpg")

    @patch('insta_downloader.time.sleep', return_value=None)
    @patch('insta_downloader.os.listdir')
    def test_skip_existing_files(self, mock_listdir, mock_sleep):
        """
        Test that existing files are skipped during the download process.
        """
        # Simulate existing files
        mock_listdir.return_value = ["test_user_ABC_01.jpg"]

        # Mock downloader methods
        downloader = InstagramPhotoDownloader("test_user")
        downloader._get_instagram_photo_urls = MagicMock(return_value=[
            ("http://example.com/image1.jpg", "2024-01-01", "ABC", 1)
        ])

        with patch('sys.stdout', new_callable=MagicMock()):
            with patch('insta_downloader.urllib.request.urlretrieve') as mock_urlretrieve:
                downloader.download()
                # Verify that urlretrieve was not called for the existing file
                mock_urlretrieve.assert_not_called()

    @patch('insta_downloader.time.sleep', return_value=None)
    @patch('insta_downloader.os.chmod')
    def test_apply_custom_permissions(self, mock_chmod, mock_sleep):
        """
        Test that custom file permissions are applied correctly.
        """
        downloader = InstagramPhotoDownloader("test_user", permissions=0o600)
        downloader._get_instagram_photo_urls = MagicMock(return_value=[
            ("http://example.com/image1.jpg", "2024-01-01", "ABC", 1)
        ])

        with patch('sys.stdout', new_callable=MagicMock()):
            with patch('insta_downloader.urllib.request.urlretrieve'):
                downloader.download()
                # Verify chmod is called with the correct permissions
                mock_chmod.assert_called_once_with("test_user_ABC_01.jpg", 0o600)

    @patch('insta_downloader.time.sleep', return_value=None)
    @patch('insta_downloader.InstagramPhotoDownloader._download_and_save_image')
    def test_sorting_posts(self, mock_download_and_save_image, mock_sleep):
        """
        Test that posts are sorted chronologically before downloading.
        """
        downloader = InstagramPhotoDownloader("test_user")
        downloader._get_instagram_photo_urls = MagicMock(return_value=[
            ("http://example.com/image2.jpg", "2024-01-02", "DEF", 1),
            ("http://example.com/image1.jpg", "2024-01-01", "ABC", 1)
        ])

        with patch('sys.stdout', new_callable=MagicMock()):
            downloader.download()
            # Verify that downloads occur in chronological order
            mock_download_and_save_image.assert_any_call("http://example.com/image1.jpg", "test_user_ABC_01.jpg")
            mock_download_and_save_image.assert_any_call("http://example.com/image2.jpg", "test_user_DEF_01.jpg")

    @patch('insta_downloader.time.sleep', return_value=None)
    @patch('insta_downloader.os.listdir')
    def test_empty_directory(self, mock_listdir, mock_sleep):
        """
        Test behavior when the source directory is empty.
        """
        mock_listdir.return_value = []

        downloader = InstagramPhotoDownloader("test_user")
        downloader._get_instagram_photo_urls = MagicMock(return_value=[])

        with patch('sys.stdout', new_callable=MagicMock()) as mock_stdout:
            downloader.download()
            # Verify that an appropriate message is printed
            self.assertIn("This account test_user has 0 image posts to download.", mock_stdout.mock_calls[0][1][0])


if __name__ == '__main__':
    unittest.main()
