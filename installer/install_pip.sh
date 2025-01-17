#!/bin/sh

########################################################################
# install_pip.sh: Bulk Python Library Install Script
#
#  Description:
#  This script automates the installation of a wide range of Python
#  libraries that are essential for data analysis, machine learning,
#  scientific computing, and web development. It ensures that these
#  libraries are updated to their latest versions for optimal
#  performance and compatibility. Additionally, the script supports
#  environments with or without proxy settings, making it suitable for
#  various network configurations. Easy Install support is included
#  where available for certain libraries.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2025-01-16
#       Official release. Removed sudo dependency, improved documentation,
#       and refined error handling.
#  v0.4 2014-08-12
#       Added pip and pyflakes. Set upgrade as default.
#  v0.3 2014-06-27
#       Auto proxy detection.
#  v0.2 2014-02-10
#       Enabled proxy support.
#  v0.1 2014-02-09
#       Initial version.
#
#  Usage:
#  Run this script in a terminal to set up your Python environment.
#  Examples:
#     ./install_pip.sh /path/to/python
#     ./install_pip.sh
#
#  Requirements:
#  - pip must be installed prior to executing this script.
#
#  Exit Codes:
#  0: Success - All libraries were installed successfully.
#  1: Error - A critical issue occurred (e.g., missing dependencies).
#
#  Notes:
#  - If no path is provided, the script assumes default tools in PATH.
#  - Proxy support can be configured using the HTTP_PROXY environment variable.
#
########################################################################

# Function to check if a command exists in the PATH
check_command() {
    if ! command -v "$1" >/dev/null 2>&1; then
        echo "Error: $1 is not installed or not in PATH." >&2
        exit 1
    fi
}

# Function to set up the environment variables for pip and Easy Install
setup_environment() {
    if [ -n "$1" ]; then
        export EASY_INSTALL=$1/bin/easy_install
        export PIP=$1/bin/pip
    else
        export EASY_INSTALL=easy_install
        export PIP=pip
    fi

    # Verify that pip is available
    check_command "$PIP"

    # Warn if Easy Install is not available
    if ! command -v "$EASY_INSTALL" >/dev/null 2>&1; then
        echo "Warning: Easy Install (easy_install) is not found in PATH. Skipping Easy Install steps." >&2
        EASY_INSTALL=""
    fi

    # Set proxy if HTTP_PROXY is defined
    if [ -n "$HTTP_PROXY" ]; then
        PROXY="--proxy $HTTP_PROXY"
    else
        PROXY=""
    fi
}

# Function to install a single Python library
install_lib() {
    local lib=$1
    echo "Installing $lib..."
    $PIP install $PROXY -U "$lib"
}

# Function to install the necessary Python libraries using pip
install_libs() {
    echo "Updating pip to the latest version..."
    $PIP install $PROXY -U pip

    echo "Installing essential Python libraries..."
    libs=(
        "IPython"
        "jupyter"
        "notebook"
        "pyflakes"
        "flake8"
        "pytest"
        "pytest-pep8"
        "autopep8"
        "autoflake"
        "isort"
        "Cython"
        "docutils"
        "nose"
        "docopt"
        "simplejson"
        "msgpack-python"
        "numpy"
        "scipy"
        "scikit-learn"
        "japandas"
        "pandas-datareader"
        "chainer"
        "joblib"
        "dask"
        "patsy"
        "statsmodels"
        "sympy"
        "seaborn"
        "bokeh"
        "twisted"
        "Flask"
        "Flask-Assets"
        "Flask-Bootstrap"
        "Hamlish-Jinja"
        "gunicorn"
        "django"
        "SQLAlchemy"
        "lmdb"
        "migrate"
        "readline"
        "Pygments"
        "Babel"
        "Genshi"
        "bottle"
        "cherrypy"
        "beautifulsoup4"
        "lxml"
        "requests"
        "pysolr"
        "watson-developer-cloud"
        "html5lib"
        "husl"
        "pillow"
        "ggplot"
        "pyper"
        "jinja2"
        "tornado"
        "pyzmq"
        "awscli"
        "cchardet"
        "openpyxl"
        "xlrd"
        "simpy"
        "networkx"
        "pdfminer3k"
        "pybrain"
        "uwsgi"
        "pypandoc"
        "zipline"
        "DocumentFeatureSelection"
        "python-tr"
        "mod_wsgi"
        "beaker"
        "python-memcached"
        "psycopg2-binary"
        "mpi4py"
        "keras"
        "tensorflow"
        "matplotlib"
        "pandas"
        "pep8"
        "instaloader"
    )

    for lib in "${libs[@]}"; do
        install_lib "$lib"
    done

    # Install additional libraries using Easy Install if available
    if [ -n "$EASY_INSTALL" ]; then
        echo "Installing additional libraries using Easy Install..."
        $EASY_INSTALL -U TA-Lib
        $EASY_INSTALL -U nltk
    else
        echo "Skipping Easy Install steps as Easy Install is not available."
    fi
}

# Main function to coordinate environment setup and library installation
main() {
    echo "Starting Python library installation..."
    setup_environment "$1"
    install_libs
    echo "All tasks completed successfully."
}

# Start the script with the provided argument (if any)
main "$1"
