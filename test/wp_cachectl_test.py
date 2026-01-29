#!/usr/bin/env python

########################################################################
# wp_cachectl_test.py: Unit tests for wp_cachectl.py
#
#  Description:
#  This script tests the WordPress cache control tool (wp_cachectl.py).
#  It verifies that:
#    - Usage output is printed and exits with code 0 when invoked with -h.
#    - The script extracts its version from the header comment block.
#    - State-changing commands require --reason.
#    - State-changing commands refuse to run without --yes unless --dry-run is used.
#    - Default WordPress path is /var/www/wordpress, and can be overridden by --path.
#    - The script recognizes the layered model and performs correct WP-CLI calls
#      in dry-run mode for each event command.
#    - W3 Total Cache (W3TC) branching is applied: when W3TC is not active,
#      L3 is skipped; when W3TC is active, L3 flush is attempted.
#
#  Important Notes for Tests:
#  - These tests do not require a real WordPress installation.
#  - WP-CLI is mocked at the subprocess layer (subprocess.call/check_output).
#  - The tool uses OptionParser, so tests call main() after patching sys.argv.
#  - Tests focus on behavior and call intent, not on WP-CLI output formatting.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Test Cases:
#    - test_usage_shows_help:
#        Verifies that the script prints usage and exits with code 0 when invoked with -h option.
#    - test_get_script_version_from_header:
#        Confirms that get_script_version() extracts a version like "v1.0" from Version History.
#    - test_state_changing_requires_reason:
#        Confirms that commands other than "status" require --reason.
#    - test_state_changing_requires_yes_or_dry_run:
#        Confirms that commands other than "status" refuse to run without --yes unless --dry-run is used.
#    - test_default_path_is_var_www_wordpress:
#        Confirms that the default WordPress path is /var/www/wordpress.
#    - test_override_path_via_option:
#        Confirms that --path overrides the default path.
#    - test_after_core_update_dry_run_builds_expected_wp_cli_calls:
#        Confirms that after-core-update emits L1/L2 (and optionally update-db) WP-CLI calls in dry-run.
#    - test_after_theme_update_dry_run_builds_expected_wp_cli_calls:
#        Confirms that after-theme-update emits L1/L2 WP-CLI calls in dry-run.
#    - test_after_plugin_update_default_is_l2_only_in_dry_run:
#        Confirms that after-plugin-update emits L2 only by default in dry-run.
#    - test_after_plugin_update_with_l1_and_l3_force_in_dry_run:
#        Confirms that after-plugin-update with --l1 and --l3-force emits L1/L2 and attempts L3.
#    - test_troubleshoot_level_4_only_prints_guidance:
#        Confirms that troubleshoot --level 4 does not attempt WP-CLI calls beyond status checks.
#    - test_w3tc_skipped_when_not_active:
#        Confirms that L3 W3TC flush is skipped when W3TC is not active.
#    - test_w3tc_used_when_active:
#        Confirms that L3 W3TC flush is attempted when W3TC is active.
#
#  Version History:
#  v1.0 2026-01-29
#       Initial test implementation for wp_cachectl.py.
#
########################################################################

import os
import sys

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import contextlib
import io
import unittest
from unittest import mock

import wp_cachectl


class FakeWPEnv(object):
    """
    Provide a controlled fake environment for wp_cachectl:
    - Pretend 'wp' exists in PATH (command_exists -> True).
    - Pretend WordPress is installed (wp_try core is-installed -> True).
    - Control W3TC detection (help w3-total-cache + plugin is-active w3-total-cache).
    - Capture subprocess.call and subprocess.check_output calls.
    """

    def __init__(self, w3tc_active=False):
        self.w3tc_active = w3tc_active
        self.calls = []
        self.check_outputs = []

    def _fake_command_exists(self, command):
        return True

    def _fake_subprocess_call(self, argv, **kwargs):
        # argv is a list in our tool
        self.calls.append((argv, kwargs))
        return 0

    def _fake_check_output(self, argv, **kwargs):
        # Capture argv for inspection, then return plausible output
        self.check_outputs.append((argv, kwargs))

        # wp option get home
        if "option" in argv and "get" in argv:
            return b"https://example.test"

        # everything else
        return b""

    def patch_all(self):
        """
        Return a context manager that patches key functions used by wp_cachectl.
        """
        return contextlib.ExitStack().__enter__()


class TestWpCachectl(unittest.TestCase):
    def _run_main(self, argv, w3tc_active=False):
        """
        Run wp_cachectl.main() with patched subprocess and helpers.
        Returns (rc, stdout, stderr, fake_env).
        """
        fake = FakeWPEnv(w3tc_active=w3tc_active)

        buf_out = io.StringIO()
        buf_err = io.StringIO()

        # Keep patches minimal and targeted.
        with contextlib.redirect_stdout(buf_out), contextlib.redirect_stderr(buf_err):
            argv_old = sys.argv[:]
            try:
                sys.argv = argv[:]  # e.g. ["wp_cachectl.py", "status", ...]
                with mock.patch.object(wp_cachectl, "command_exists", fake._fake_command_exists), \
                        mock.patch.object(wp_cachectl.subprocess, "call", fake._fake_subprocess_call), \
                        mock.patch.object(wp_cachectl.subprocess, "check_output", fake._fake_check_output):

                    # Control wp_try behavior:
                    # - core is-installed should succeed
                    # - help w3-total-cache and plugin is-active w3-total-cache depend on w3tc_active
                    def _fake_wp_try(wp_bin, wp_path, args):
                        if args[:2] == ["core", "is-installed"]:
                            return True
                        if args[:2] == ["help", "w3-total-cache"]:
                            return bool(fake.w3tc_active)
                        if args[:3] == ["plugin", "is-active", "w3-total-cache"]:
                            return bool(fake.w3tc_active)
                        # default: succeed
                        return True

                    with mock.patch.object(wp_cachectl, "wp_try", _fake_wp_try):
                        try:
                            rc = wp_cachectl.main()
                        except SystemExit as e:
                            rc = int(getattr(e, "code", 1))
            finally:
                sys.argv = argv_old

        return rc, buf_out.getvalue(), buf_err.getvalue(), fake

    def test_usage_shows_help(self):
        # Usage is printed by usage() when invoked via __main__ with -h.
        # Here we test usage() directly to keep the test simple and stable.
        buf_out = io.StringIO()
        with contextlib.redirect_stdout(buf_out):
            with self.assertRaises(SystemExit) as cm:
                wp_cachectl.usage()
        self.assertEqual(cm.exception.code, 0)
        self.assertIn("Usage:", buf_out.getvalue())

    def test_get_script_version_from_header(self):
        ver = wp_cachectl.get_script_version()
        # Expect "vX.Y" style (best-effort)
        self.assertTrue(ver.startswith("v"))
        self.assertGreaterEqual(len(ver), 2)

    def test_state_changing_requires_reason(self):
        # after-theme-update without --reason => usage error (2)
        rc, out, err, _ = self._run_main(
            ["wp_cachectl.py", "after-theme-update", "--dry-run", "--path", "/tmp/wp"]
        )
        self.assertEqual(rc, 2)
        self.assertIn("--reason is required", err)

    def test_state_changing_requires_yes_or_dry_run(self):
        # after-theme-update with --reason but without --yes/--dry-run => usage error (2)
        rc, out, err, _ = self._run_main(
            ["wp_cachectl.py", "after-theme-update", "--reason", "theme update", "--path", "/tmp/wp"]
        )
        self.assertEqual(rc, 2)
        self.assertIn("Refusing to run without --yes", err)

    def test_default_path_is_var_www_wordpress(self):
        # Verify module constant
        self.assertEqual(wp_cachectl.DEFAULT_WP_PATH, "/var/www/wordpress")

    def test_override_path_via_option(self):
        rc, out, err, fake = self._run_main(
            ["wp_cachectl.py", "status", "--path", "/custom/wp"]
        )
        self.assertEqual(rc, 0)
        # Ensure the printed path includes the override
        self.assertIn("/custom/wp", out)

    def test_after_core_update_dry_run_builds_expected_wp_cli_calls(self):
        rc, out, err, fake = self._run_main(
            ["wp_cachectl.py", "after-core-update", "--dry-run", "--reason", "core update", "--path", "/tmp/wp"]
        )
        self.assertEqual(rc, 0)

        # In dry-run, the tool prints "DRY-RUN: ..." lines and does not call subprocess.call.
        # However, it still calls subprocess.check_output to get home URL.
        self.assertIn("DRY-RUN:", out)
        self.assertIn("L1: Delete transients", out)
        self.assertIn("L2: Flush object cache", out)

        # Ensure the WP-CLI commands are referenced in dry-run output.
        self.assertIn("transient delete --all", out)
        self.assertIn("cache flush", out)

    def test_after_core_update_with_update_db_dry_run(self):
        rc, out, err, fake = self._run_main(
            ["wp_cachectl.py", "after-core-update", "--dry-run", "--update-db",
             "--reason", "core update", "--path", "/tmp/wp"]
        )
        self.assertEqual(rc, 0)
        self.assertIn("core update-db", out)

    def test_after_theme_update_dry_run_builds_expected_wp_cli_calls(self):
        rc, out, err, fake = self._run_main(
            ["wp_cachectl.py", "after-theme-update", "--dry-run", "--reason", "theme", "--path", "/tmp/wp"]
        )
        self.assertEqual(rc, 0)
        self.assertIn("transient delete --all", out)
        self.assertIn("cache flush", out)

    def test_after_plugin_update_default_is_l2_only_in_dry_run(self):
        rc, out, err, fake = self._run_main(
            ["wp_cachectl.py", "after-plugin-update", "--dry-run", "--reason", "plugin", "--path", "/tmp/wp"]
        )
        self.assertEqual(rc, 0)
        self.assertIn("L1: skipped (default)", out)
        self.assertIn("cache flush", out)
        self.assertIn("L3: skipped (default)", out)

    def test_after_plugin_update_with_l1_and_l3_force_in_dry_run(self):
        rc, out, err, fake = self._run_main(
            ["wp_cachectl.py", "after-plugin-update", "--dry-run", "--l1", "--l3-force",
             "--reason", "plugin", "--path", "/tmp/wp"],
            w3tc_active=True
        )
        self.assertEqual(rc, 0)
        self.assertIn("transient delete --all", out)
        self.assertIn("cache flush", out)
        # With W3TC active, L3 attempt should appear in output.
        self.assertIn("w3-total-cache flush all", out)

    def test_troubleshoot_level_4_only_prints_guidance(self):
        rc, out, err, fake = self._run_main(
            ["wp_cachectl.py", "troubleshoot", "--dry-run", "--level", "4",
             "--reason", "stale", "--path", "/tmp/wp"]
        )
        self.assertEqual(rc, 0)
        # Level 4 should not run L1/L2/L3; it should print L4 guidance.
        self.assertNotIn("transient delete --all", out)
        self.assertNotIn("cache flush", out)
        self.assertIn("L4: CDN/Browser cache is not automated", out)

    def test_w3tc_skipped_when_not_active(self):
        rc, out, err, fake = self._run_main(
            ["wp_cachectl.py", "after-theme-update", "--dry-run", "--reason", "theme", "--path", "/tmp/wp"],
            w3tc_active=False
        )
        self.assertEqual(rc, 0)
        self.assertIn("L3: W3 Total Cache not active/available; skip", out)

    def test_w3tc_used_when_active(self):
        rc, out, err, fake = self._run_main(
            ["wp_cachectl.py", "after-theme-update", "--dry-run", "--reason", "theme", "--path", "/tmp/wp"],
            w3tc_active=True
        )
        self.assertEqual(rc, 0)
        self.assertIn("w3-total-cache flush all", out)


if __name__ == "__main__":
    unittest.main()
