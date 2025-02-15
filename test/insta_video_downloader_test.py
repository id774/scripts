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

sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

try:
    from insta_video_downloader import InstagramVideoDownloader
    HAS_INSTA_VIDEO_DOWNLOADER = True
except ImportError:
    HAS_INSTA_VIDEO_DOWNLOADER = False

@unittest.skipIf(not HAS_INSTA_VIDEO_DOWNLOADER, "insta_video_downloader module is not available")
class TestInstagramVideoDownloader(unittest.TestCase):
    """
    Test suite for InstagramVideoDownloader.
    Ensures correct behavior for various scenarios including empty profiles,
    single video downloads, multiple video downloads, and file skipping.
    """

    def setUp(self):
        """
        Set up mock objects to replace actual network requests and file operations.
        """
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
        Test that a single video post (GraphVideo) is correctly downloaded.
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

    @patch('insta_video_downloader.time.sleep', return_value=None)
    @patch('insta_video_downloader.instaloader.Profile.from_username')
    def test_no_videos(self, mock_from_username, mock_sleep):
        """
        Test behavior when no videos are available.
        Ensures the script exits gracefully without attempting downloads.
        """
        mock_profile = MagicMock()
        mock_profile.get_posts.return_value = []
        mock_from_username.return_value = mock_profile

        downloader = InstagramVideoDownloader("test_user")
        with patch('sys.stdout', new_callable=MagicMock()):
            downloader.download()

    @patch('insta_video_downloader.time.sleep', return_value=None)
    @patch('insta_video_downloader.InstagramVideoDownloader._download_and_save_video')
    def test_multiple_videos_in_post(self, mock_download_and_save_video, mock_sleep):
        """
        Test handling of a post with multiple videos.
        Ensures each video in a post (GraphSidecar) is processed and downloaded correctly.
        """
        downloader = InstagramVideoDownloader("test_user")
        downloader._get_instagram_video_urls = MagicMock(return_value=[
            ("http://example.com/video1.mp4", datetime(2024, 1, 1), "VID1", 1),
            ("http://example.com/video2.mp4", datetime(2024, 1, 1), "VID1", 2)
        ])
        with patch('sys.stdout', new_callable=MagicMock()):
            downloader.download()

        self.assertEqual(mock_download_and_save_video.call_count, 2)
        mock_download_and_save_video.assert_any_call("http://example.com/video1.mp4", "test_user_VID1_01.mp4")
        mock_download_and_save_video.assert_any_call("http://example.com/video2.mp4", "test_user_VID1_02.mp4")

    @patch('insta_video_downloader.time.sleep', return_value=None)
    @patch('insta_video_downloader.os.listdir')
    def test_skip_existing_files(self, mock_listdir, mock_sleep):
        """
        Test that existing video files are skipped during the download process.
        """
        mock_listdir.return_value = ["test_user_VID1_01.mp4"]
        downloader = InstagramVideoDownloader("test_user")
        downloader._get_instagram_video_urls = MagicMock(return_value=[
            ("http://example.com/video1.mp4", datetime(2024, 1, 1), "VID1", 1)
        ])
        with patch('sys.stdout', new_callable=MagicMock()):
            with patch('insta_video_downloader.urllib.request.urlretrieve') as mock_urlretrieve:
                downloader.download()
                mock_urlretrieve.assert_not_called()

    @patch('insta_video_downloader.time.sleep', return_value=None)
    @patch('insta_video_downloader.os.chmod')
    def test_apply_custom_permissions(self, mock_chmod, mock_sleep):
        """
        Test that custom file permissions are applied correctly.
        """
        downloader = InstagramVideoDownloader("test_user", permissions=0o600)
        downloader._get_instagram_video_urls = MagicMock(return_value=[
            ("http://example.com/video1.mp4", datetime(2024, 1, 1), "VID1", 1)
        ])
        with patch('sys.stdout', new_callable=MagicMock()):
            with patch('insta_video_downloader.urllib.request.urlretrieve'):
                downloader.download()
                mock_chmod.assert_called_once_with("test_user_VID1_01.mp4", 0o600)

    @patch('insta_video_downloader.time.sleep', return_value=None)
    @patch('insta_video_downloader.InstagramVideoDownloader._download_and_save_video')
    def test_sorting_videos(self, mock_download_and_save_video, mock_sleep):
        """
        Test that videos are sorted chronologically before downloading.
        """
        downloader = InstagramVideoDownloader("test_user")
        downloader._get_instagram_video_urls = MagicMock(return_value=[
            ("http://example.com/video2.mp4", datetime(2024, 1, 2), "VID2", 1),
            ("http://example.com/video1.mp4", datetime(2024, 1, 1), "VID1", 1)
        ])
        with patch('sys.stdout', new_callable=MagicMock()):
            downloader.download()
            mock_download_and_save_video.assert_any_call("http://example.com/video1.mp4", "test_user_VID1_01.mp4")
            mock_download_and_save_video.assert_any_call("http://example.com/video2.mp4", "test_user_VID2_01.mp4")


if __name__ == '__main__':
    unittest.main()
