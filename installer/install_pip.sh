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
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run this script in a terminal to set up your Python environment.
#
#  Examples:
#      ./install_pip.sh /path/to/python
#      ./install_pip.sh
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
#  Version History:
#  v1.3 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.2 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
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
########################################################################

# Display full script header information extracted from the top comment block
usage() {
    awk '
        BEGIN { in_header = 0 }
        /^#{10,}$/ { if (!in_header) { in_header = 1; next } else exit }
        in_header && /^# ?/ { print substr($0, 3) }
    ' "$0"
    exit 0
}

# Check required commands
check_commands() {
    for cmd in "$@"; do
        cmd_path=$(command -v "$cmd" 2>/dev/null)
        if [ -z "$cmd_path" ]; then
            echo "[ERROR] Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif [ ! -x "$cmd_path" ]; then
            echo "[ERROR] Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Set up the environment variables for pip and Easy Install
setup_environment() {
    if [ -n "$1" ]; then
        export EASY_INSTALL=$1/bin/easy_install
        export PIP=$1/bin/pip
    else
        export EASY_INSTALL=easy_install
        export PIP=pip
    fi

    # Verify that pip is available
    check_commands "$PIP" sed

    # Warn if Easy Install is not available
    if ! command -v "$EASY_INSTALL" >/dev/null 2>&1; then
        echo "[WARN] Easy Install (easy_install) is not found in PATH. Skipping Easy Install steps." >&2
        EASY_INSTALL=""
    fi

    # Set proxy if HTTP_PROXY is defined
    if [ -n "$HTTP_PROXY" ]; then
        PROXY="--proxy $HTTP_PROXY"
    else
        PROXY=""
    fi
}

# Install a single Python library
install_lib() {
    echo "[INFO] Installing $1..."
    $PIP install $PROXY -U "$1"
}

# Install the necessary Python libraries using pip
install_libs() {
    echo "[INFO] Updating pip to the latest version..."
    $PIP install $PROXY -U pip

    echo "[INFO] Installing essential Python libraries..."
    # Define the list of libraries as a multi-line string
    libs="
    IPython
    jupyter
    notebook
    pyflakes
    flake8
    pytest
    pytest-flake8
    autopep8
    black
    autoflake
    isort
    Cython
    docutils
    nose
    docopt
    simplejson
    numpy
    scipy
    scikit-learn
    pandas-datareader
    chainer
    joblib
    dask
    patsy
    statsmodels
    sympy
    seaborn
    bokeh
    twisted
    Flask
    Flask-Assets
    Flask-Bootstrap
    Hamlish-Jinja
    gunicorn
    django
    SQLAlchemy
    lmdb
    migrate
    Pygments
    Babel
    Genshi
    bottle
    cherrypy
    beautifulsoup4
    lxml
    requests
    pysolr
    html5lib
    colorspacious
    pillow
    plotnine
    pyper
    jinja2
    tornado
    pyzmq
    awscli
    cchardet
    openpyxl
    xlrd
    simpy
    networkx
    uwsgi
    zipline
    DocumentFeatureSelection
    beaker
    python-memcached
    psycopg2-binary
    mpi4py
    keras
    tensorflow
    matplotlib
    pandas
    pycodestyle
    instaloader
    "

    # Loop through each library and install it
    for lib in $libs; do
        # Remove leading and trailing spaces/tabs
        lib=$(echo "$lib" | sed 's/^[ \t]*//;s/[ \t]*$//')
        install_lib "$lib"
    done

    # Install additional libraries using Easy Install if available
    if [ -n "$EASY_INSTALL" ]; then
        echo "[INFO] Installing additional libraries using Easy Install..."
        $EASY_INSTALL -U TA-Lib
        $EASY_INSTALL -U nltk
    else
        echo "[WARN] Skipping Easy Install steps as Easy Install is not available." >&2
    fi
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    echo "[INFO] Starting Python library installation..."
    setup_environment "$1"
    install_libs

    echo "[INFO] All specified python packages have been installed."
    return 0
}

# Execute main function
main "$@"
exit $?
