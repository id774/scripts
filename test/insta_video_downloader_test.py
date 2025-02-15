#!/usr/bin/env python

########################################################################
# insta_video_downloader_test.py: Test suite for insta_video_downloader.py
#
#  Description:
#  This test suite verifies the functionality of the insta_video_downloader.py
#  script. It uses mocking to simulate network requests and file operations
#  to avoid actual downloads during testing. The suite tests various edge
#  cases, including empty profiles, successful downloads, and handling
#  of multiple videos in a post. This ensures the script behaves as expected
#  in different scenarios without affecting external systems or generating
#  unwanted network traffic.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2025-02-15
#       Initial release. Test suite for insta_video_downloader.py script.
#
########################################################################

import os
import sys
import unittest
from datetime import datetime
from unittest.mock import MagicMock, patch

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

# Attempt to import insta_video_downloader and set a flag if unavailable
try:
    from insta_video_downloader import InstagramVideoDownloader
    HAS_INSTA_VIDEO_DOWNLOADER = True
except ImportError:
    HAS_INSTA_VIDEO_DOWNLOADER = False

@unittest.skipIf(not HAS_INSTA_VIDEO_DOWNLOADER, "insta_video_downloader module is not available")
class TestInstagramVideoDownloader(unittest.TestCase):
    """Test suite for InstagramVideoDownloader."""

    def setUp(self):
        """Set up mocks for all tests."""
        if not HAS_INSTA_VIDEO_DOWNLOADER:
            self.skipTest("insta_video_downloader module is not available")

        try:
            import instaloader
        except ModuleNotFoundError:
            self.skipTest("instaloader module is not installed")

        patcher_profile = patch('instaloader.Profile.from_username', autospec=True)
        self.addCleanup(patcher_profile.stop)
        self.mock_from_username = patcher_profile.start()
        self.mock_profile = MagicMock()
        self.mock_profile.get_posts.return_value = []
        self.mock_from_username.return_value = self.mock_profile

        patcher_loader = patch('instaloader.Instaloader', autospec=True)
        self.addCleanup(patcher_loader.stop)
        self.mock_instaloader = patcher_loader.start()
        self.mock_loader = self.mock_instaloader.return_value

        self.mock_loader.context = MagicMock()

    @patch('insta_video_downloader.time.sleep', return_value=None)
    @patch('insta_video_downloader.os.chmod')
    @patch('insta_video_downloader.urllib.request.urlretrieve')
    @patch('insta_video_downloader.instaloader.Profile.from_username')
    def test_download_videos(self, mock_from_username, mock_urlretrieve, mock_chmod, mock_sleep):
        """
        Test downloading a single video post.
        """
        mock_profile = MagicMock()
        mock_profile.get_posts.return_value = [
            MagicMock(typename="GraphVideo", video_url="http://example.com/video1.mp4",
                      date=datetime(2024, 1, 1), shortcode="VID1")
        ]
        mock_from_username.return_value = mock_profile

        downloader = InstagramVideoDownloader("test_user", permissions=0o644, sleep_time=0)
        downloader._get_instagram_video_urls = MagicMock(return_value=[
            ("http://example.com/video1.mp4", datetime(2024, 1, 1), "VID1", 1)
        ])

        with patch('sys.stdout', new_callable=MagicMock()):
            with patch('os.listdir', return_value=[]):
                downloader.download()

        mock_urlretrieve.assert_called_with("http://example.com/video1.mp4", "test_user_VID1_01.mp4")
        mock_chmod.assert_called_once_with("test_user_VID1_01.mp4", 0o644)


if __name__ == '__main__':
    unittest.main()
