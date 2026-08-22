#!/usr/bin/env python

########################################################################
# show_version_test.py: Test for show_version.py
#
#  Description:
#  This script tests show_version.py's behavior when checking package
#  distribution metadata, versions, and actual importability, and when
#  printing the Python version.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Test Cases:
#    - Prints Python version with -p
#    - Shows version for a known standard-library module
#    - Reports a package as not found when import fails
#    - Resolves metadata version and successful import together
#    - Separates distribution name from import name (e.g. scikit-learn / sklearn)
#    - Distinguishes installed-but-import-failed from not-found
#    - Falls back to actual import when metadata is missing or unavailable
#    - Resolves version via __version__, VERSION, or "unknown version"
#    - Calls help(module) only after a successful import
#    - Classifies non-ImportError exceptions as import failures
#    - Verifies the import-failure summary content and message normalization
#    - Verifies the distribution/import name mapping catalog
#
#  Version History:
#  v1.1 2026-08-22
#       Added test coverage for the v3.0 metadata/import classification
#       logic, the distribution-name to import-name mapping catalog, and
#       the Python code-quality tools required by pyck.py.
#  v1.0 2025-07-07
#       Initial release.
#
########################################################################

import io
import os
import sys
import types
import unittest
import warnings
from contextlib import redirect_stderr, redirect_stdout
from unittest.mock import MagicMock, patch

warnings.filterwarnings("ignore")

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import show_version


class _FakePackageNotFoundError(Exception):
    """ Stand-in for importlib.metadata.PackageNotFoundError. """


def make_fake_metadata(versions=None):
    """ Build a mock object that mimics importlib.metadata for tests. """
    versions = versions or {}
    fake = MagicMock()
    fake.PackageNotFoundError = _FakePackageNotFoundError

    def version(distribution_name):
        if distribution_name in versions:
            return versions[distribution_name]
        raise _FakePackageNotFoundError(distribution_name)

    fake.version = MagicMock(side_effect=version)
    return fake


class TestShowVersion(unittest.TestCase):
    def test_python_version_output(self):
        options = MagicMock(info=False, python=True)
        m = show_version.PythonModuleInfo(options)
        f = io.StringIO()
        with redirect_stdout(f):
            m.get_python_version()
        output = f.getvalue()
        self.assertIn("Python", output)

    def test_known_module_version(self):
        options = MagicMock(info=False, python=False)
        m = show_version.PythonModuleInfo(options)
        f = io.StringIO()
        with redirect_stdout(f), redirect_stderr(io.StringIO()):
            m.check_package('math', 'math')  # standard library module
        output = f.getvalue()
        self.assertIn("math", output)

    def test_missing_module_handling(self):
        options = MagicMock(info=False, python=False)
        m = show_version.PythonModuleInfo(options)

        with patch('importlib.import_module', side_effect=ImportError):
            f = io.StringIO()
            with redirect_stdout(f), redirect_stderr(io.StringIO()):
                m.check_package('nonexistent_package', 'nonexistent_package')
                m.show_not_found()
            output = f.getvalue()
        self.assertIn("nonexistent_package", output)
        self.assertIn("not found", output.lower())
        self.assertEqual(m.import_failed, [])

    def test_metadata_version_and_successful_import(self):
        options = MagicMock(info=False, python=False)
        m = show_version.PythonModuleInfo(options)
        fake_metadata = make_fake_metadata({'numpy': '1.26.4'})
        fake_module = types.ModuleType('numpy')

        with patch.object(show_version, 'importlib_metadata', fake_metadata), \
                patch('importlib.import_module', return_value=fake_module):
            f = io.StringIO()
            with redirect_stdout(f):
                m.check_package('numpy', 'numpy')
        output = f.getvalue()
        self.assertIn("numpy", output)
        self.assertIn("1.26.4", output)
        self.assertEqual(m.not_found, [])
        self.assertEqual(m.import_failed, [])

    def test_distribution_import_name_mapping(self):
        options = MagicMock(info=False, python=False)
        m = show_version.PythonModuleInfo(options)
        fake_metadata = make_fake_metadata({'scikit-learn': '1.5.0'})
        fake_module = types.ModuleType('sklearn')

        with patch.object(show_version, 'importlib_metadata', fake_metadata), \
                patch('importlib.import_module', return_value=fake_module) as mock_import:
            f = io.StringIO()
            with redirect_stdout(f):
                m.check_package('scikit-learn', 'sklearn')
        fake_metadata.version.assert_called_once_with('scikit-learn')
        mock_import.assert_called_once_with('sklearn')
        self.assertIn("scikit-learn", f.getvalue())

    def test_installed_but_import_failed(self):
        options = MagicMock(info=False, python=False)
        m = show_version.PythonModuleInfo(options)
        fake_metadata = make_fake_metadata({'torch': '2.3.0'})
        error = OSError('libcudnn.so: cannot open shared object file')

        with patch.object(show_version, 'importlib_metadata', fake_metadata), \
                patch('importlib.import_module', side_effect=error):
            f = io.StringIO()
            with redirect_stdout(f):
                m.check_package('torch', 'torch')

        self.assertEqual(m.not_found, [])
        self.assertEqual(len(m.import_failed), 1)
        distribution_name, import_name, exc_class, message = m.import_failed[0]
        self.assertEqual(distribution_name, 'torch')
        self.assertEqual(import_name, 'torch')
        self.assertEqual(exc_class, 'OSError')
        self.assertIn('libcudnn.so', message)

    def test_not_installed(self):
        options = MagicMock(info=False, python=False)
        m = show_version.PythonModuleInfo(options)
        fake_metadata = make_fake_metadata({})
        error = ImportError("No module named 'zzzpkg'")
        error.name = 'zzzpkg'

        with patch.object(show_version, 'importlib_metadata', fake_metadata), \
                patch('importlib.import_module', side_effect=error):
            f = io.StringIO()
            with redirect_stdout(f):
                m.check_package('zzzpkg', 'zzzpkg')

        self.assertEqual(m.import_failed, [])
        self.assertEqual(m.not_found, ['zzzpkg'])

    def test_metadata_missing_but_import_succeeds(self):
        options = MagicMock(info=False, python=False)
        m = show_version.PythonModuleInfo(options)
        fake_metadata = make_fake_metadata({})
        fake_module = types.ModuleType('mymodule')
        fake_module.__version__ = '0.9.1'

        with patch.object(show_version, 'importlib_metadata', fake_metadata), \
                patch('importlib.import_module', return_value=fake_module):
            f = io.StringIO()
            with redirect_stdout(f):
                m.check_package('mydist', 'mymodule')

        output = f.getvalue()
        self.assertIn("mydist", output)
        self.assertIn("0.9.1", output)
        self.assertEqual(m.not_found, [])
        self.assertEqual(m.import_failed, [])

    def test_metadata_unavailable_fallback(self):
        options = MagicMock(info=False, python=False)
        m = show_version.PythonModuleInfo(options)
        fake_module = types.ModuleType('mymodule')
        fake_module.__version__ = '3.2.1'

        with patch.object(show_version, 'importlib_metadata', None), \
                patch('importlib.import_module', return_value=fake_module):
            f = io.StringIO()
            with redirect_stdout(f):
                m.check_package('mydist', 'mymodule')

        output = f.getvalue()
        self.assertIn("mydist", output)
        self.assertIn("3.2.1", output)

    def test_version_attribute_fallback(self):
        options = MagicMock(info=False, python=False)
        m = show_version.PythonModuleInfo(options)
        fake_metadata = make_fake_metadata({})
        fake_module = types.ModuleType('mymodule')
        fake_module.VERSION = '7.7.7'

        with patch.object(show_version, 'importlib_metadata', fake_metadata), \
                patch('importlib.import_module', return_value=fake_module):
            f = io.StringIO()
            with redirect_stdout(f):
                m.check_package('mydist', 'mymodule')

        self.assertIn("7.7.7", f.getvalue())

    def test_unknown_version(self):
        options = MagicMock(info=False, python=False)
        m = show_version.PythonModuleInfo(options)
        fake_metadata = make_fake_metadata({})
        fake_module = types.ModuleType('mymodule')

        with patch.object(show_version, 'importlib_metadata', fake_metadata), \
                patch('importlib.import_module', return_value=fake_module):
            f = io.StringIO()
            with redirect_stdout(f):
                m.check_package('mydist', 'mymodule')

        self.assertIn("unknown version", f.getvalue())

    def test_info_calls_help_only_on_success(self):
        options = MagicMock(info=True, python=False)
        m = show_version.PythonModuleInfo(options)
        fake_metadata = make_fake_metadata({'okdist': '1.0'})
        fake_module = types.ModuleType('okmod')

        with patch('builtins.help') as mock_help, \
                patch.object(show_version, 'importlib_metadata', fake_metadata), \
                patch('importlib.import_module', return_value=fake_module):
            f = io.StringIO()
            with redirect_stdout(f):
                m.check_package('okdist', 'okmod')
        mock_help.assert_called_once_with(fake_module)

    def test_info_not_called_on_failure(self):
        options = MagicMock(info=True, python=False)
        m = show_version.PythonModuleInfo(options)
        fake_metadata = make_fake_metadata({})
        error = ImportError("No module named 'zzzpkg'")
        error.name = 'zzzpkg'

        with patch('builtins.help') as mock_help, \
                patch.object(show_version, 'importlib_metadata', fake_metadata), \
                patch('importlib.import_module', side_effect=error):
            f = io.StringIO()
            with redirect_stdout(f):
                m.check_package('zzzpkg', 'zzzpkg')
        mock_help.assert_not_called()

    def test_non_import_error_exception_is_import_failed(self):
        options = MagicMock(info=False, python=False)
        m = show_version.PythonModuleInfo(options)
        fake_metadata = make_fake_metadata({})

        with patch.object(show_version, 'importlib_metadata', fake_metadata), \
                patch('importlib.import_module', side_effect=RuntimeError('boom')):
            f = io.StringIO()
            with redirect_stdout(f):
                m.check_package('somedist', 'somemod')

        self.assertEqual(m.not_found, [])
        self.assertEqual(len(m.import_failed), 1)
        self.assertEqual(m.import_failed[0][2], 'RuntimeError')

    def test_import_failed_message_newline_normalized(self):
        options = MagicMock(info=False, python=False)
        m = show_version.PythonModuleInfo(options)
        fake_metadata = make_fake_metadata({'somedist': '1.0'})

        with patch.object(show_version, 'importlib_metadata', fake_metadata), \
                patch('importlib.import_module',
                      side_effect=RuntimeError('line one\nline two')):
            f = io.StringIO()
            with redirect_stdout(f):
                m.check_package('somedist', 'somemod')

        message = m.import_failed[0][3]
        self.assertNotIn('\n', message)
        self.assertIn('line one', message)
        self.assertIn('line two', message)

    def test_import_failed_summary_output(self):
        options = MagicMock(info=False, python=False)
        m = show_version.PythonModuleInfo(options)
        m.import_failed.append(
            ('torch', 'torch', 'OSError',
             'libcudnn.so: cannot open shared object file'))
        f = io.StringIO()
        with redirect_stdout(f):
            m.show_import_failed()
        output = f.getvalue()
        self.assertIn("These packages could not be imported:", output)
        self.assertIn("torch", output)
        self.assertIn("OSError", output)
        self.assertIn("libcudnn.so", output)

    def test_package_mapping_catalog(self):
        mapping = dict(show_version.PACKAGES)
        self.assertEqual(mapping['scikit-learn'], 'sklearn')
        self.assertEqual(mapping['beautifulsoup4'], 'bs4')
        self.assertEqual(mapping['Pillow'], 'PIL')
        self.assertEqual(mapping['mecab-python3'], 'MeCab')
        self.assertEqual(mapping['pystan'], 'stan')
        self.assertEqual(mapping['PyYAML'], 'yaml')
        self.assertEqual(mapping['python-dateutil'], 'dateutil')
        self.assertEqual(mapping['TA-Lib'], 'talib')

    def test_catalog_includes_expected_packages(self):
        names = [distribution_name for distribution_name, _ in show_version.PACKAGES]
        for expected in ('torch', 'tensorflow', 'keras', 'jax', 'transformers',
                         'datasets', 'huggingface_hub', 'scikit-learn', 'xgboost',
                         'lightgbm', 'catboost', 'numpy', 'scipy', 'pandas', 'polars',
                         'pyarrow', 'Flask', 'Django', 'fastapi', 'requests', 'httpx',
                         'spacy', 'mecab-python3', 'autopep8', 'flake8', 'autoflake',
                         'isort'):
            self.assertIn(expected, names)

    def test_catalog_excludes_removed_packages(self):
        names = [distribution_name for distribution_name, _ in show_version.PACKAGES]
        for removed in ('chainer', 'docopt', 'simplejson', 'migrate', 'genshi',
                        'pyper', 'awscli', 'zipline', 'CaboCha'):
            self.assertNotIn(removed, names)


if __name__ == '__main__':
    unittest.main()
