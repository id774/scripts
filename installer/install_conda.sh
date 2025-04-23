#!/bin/sh

########################################################################
# install_conda.sh: Bulk Python Library Install Script
#
#  Description:
#  This script sets up a Python development environment by installing
#  essential libraries and tools using Conda. It ensures that common
#  dependencies for data analysis, machine learning, and web development
#  are installed and ready for use.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.2 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.0 2025-01-16
#       Official release. Improved documentation, added command existence
#       checks, refined error handling.
#  v0.3 2018-05-10
#       Removed conda-forge channel.
#  v0.2 2018-05-02
#       Added conda-forge channel.
#  v0.1 2018-02-28
#       Initial version.
#
#  Usage:
#  Run this script in a terminal to set up your Python environment.
#
#  Examples:
#     ./install_conda.sh /path/to/python
#     ./install_conda.sh
#
#  Requirements:
#  - Conda must be installed prior to executing this script.
#
#  Exit Codes:
#  0: Success - All libraries were installed successfully.
#  1: Error - A critical issue occurred (e.g., missing dependencies).
#
#  Notes:
#  - If no path is provided, the script assumes the default installation
#    path for Anaconda (`$HOME/local/anaconda3`).
#  - Includes a wide range of tools for data science, machine learning,
#    and web development.
#
########################################################################

# Display script usage information
usage() {
    awk '
        BEGIN { in_usage = 0 }
        /^#  Usage:/ { in_usage = 1; print substr($0, 4); next }
        /^#{10}/ { if (in_usage) exit }
        in_usage && /^#/ { print substr($0, 4) }
    ' "$0"
    exit 0
}

# Function to check required commands
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

# Function to set up the environment variables for Conda and Easy Install
setup_environment() {
    if [ -n "$1" ]; then
        export EASY_INSTALL=$1/bin/easy_install
        export CONDA=$1/bin/conda
    else
        export EASY_INSTALL=$HOME/local/anaconda3/bin/easy_install
        export CONDA=$HOME/local/anaconda3/bin/conda
    fi

    # Verify that Conda is available
    check_commands "$CONDA" sed

    # Warn if Easy Install is not available
    if [ ! -x "$EASY_INSTALL" ]; then
        echo "[WARN] Easy Install (easy_install) is not found or not executable at $EASY_INSTALL. Skipping Easy Install steps." >&2
        EASY_INSTALL=""
    fi
}

# Function to install a single Python library using Conda
install_lib() {
    echo "[INFO] Installing $1..."
    $CONDA install -y "$1"
}

# Function to install the necessary libraries using Conda
install_libs() {
    echo "[INFO] Updating Conda base environment..."
    $CONDA update -n base -y conda

    echo "[INFO] Installing essential libraries using Conda..."
    # Define the list of libraries as a multi-line string
    libs="
    pip
    IPython
    jupyter
    notebook
    pyflakes
    flake8
    pytest
    pytest-pep8
    autopep8
    autoflake
    isort
    Cython
    docutils
    nose
    docopt
    simplejson
    msgpack-python
    numpy
    scipy
    scikit-learn
    japandas
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
    readline
    Pygments
    Babel
    Genshi
    bottle
    cherrypy
    beautifulsoup4
    lxml
    requests
    pysolr
    watson-developer-cloud
    html5lib
    husl
    pillow
    ggplot
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
    pdfminer3k
    pybrain
    uwsgi
    pypandoc
    zipline
    DocumentFeatureSelection
    python-tr
    mod_wsgi
    beaker
    python-memcached
    psycopg2-binary
    mpi4py
    keras
    tensorflow
    matplotlib
    pandas
    pep8
    instaloader
    "

    # Loop through each library and install it
    for lib in $libs; do
        # Remove leading and trailing spaces/tabs
        lib=$(echo "$lib" | sed 's/^[ \t]*//;s/[ \t]*$//')
        $PIP install $PROXY -U "$lib"
    done

    # Install additional libraries using Easy Install if available
    if [ -n "$EASY_INSTALL" ]; then
        echo "[INFO] Installing additional libraries using easy_install..."
        $EASY_INSTALL TA-Lib
        $EASY_INSTALL nltk
    else
        echo "[INFO] Skipping Easy Install steps as Easy Install is not available."
    fi
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    echo "[INFO] Starting Conda environment setup and library installation..."
    setup_environment "$1"
    install_libs

    echo "[INFO] All specified packages have been successfully installed."
}

# Execute main function
main "$@"
