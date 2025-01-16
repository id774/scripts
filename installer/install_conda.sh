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
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
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

# Function to check if a command exists in the PATH
check_command() {
    if ! command -v "$1" >/dev/null 2>&1; then
        echo "Error: $1 is not installed or not in PATH." >&2
        exit 1
    fi
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
    check_command "$CONDA"

    # Warn if Easy Install is not available
    if [ ! -x "$EASY_INSTALL" ]; then
        echo "Warning: Easy Install (easy_install) is not found or not executable at $EASY_INSTALL. Skipping Easy Install steps." >&2
        EASY_INSTALL=""
    fi
}

# Function to install the necessary libraries using Conda and Easy Install
install_libs() {
    echo "Updating Conda base environment..."
    $CONDA update -n base -y conda

    echo "Installing essential libraries using Conda..."
    $CONDA install -y \
        pip IPython jupyter notebook pyflakes flake8 pytest pytest-pep8 autopep8 \
        autoflake isort Cython docutils nose docopt simplejson msgpack-python \
        numpy scipy scikit-learn japandas pandas-datareader chainer joblib \
        dask patsy statsmodels sympy seaborn bokeh twisted Flask Flask-Assets \
        Flask-Bootstrap Hamlish-Jinja gunicorn django SQLAlchemy lmdb migrate \
        readline Pygments Babel Genshi bottle cherrypy beautifulsoup4 lxml \
        requests pysolr watson-developer-cloud html5lib husl pillow ggplot \
        pyper jinja2 tornado pyzmq awscli cchardet openpyxl xlrd simpy \
        networkx pdfminer3k pybrain uwsgi pypandoc zipline DocumentFeatureSelection \
        python-tr mod_wsgi beaker python-memcached psycopg2-binary mpi4py keras \
        tensorflow matplotlib pandas pep8 instaloader

    # Install additional libraries using Easy Install if available
    if [ -n "$EASY_INSTALL" ]; then
        echo "Installing additional libraries using easy_install..."
        $EASY_INSTALL TA-Lib
        $EASY_INSTALL nltk
    else
        echo "Skipping Easy Install steps as Easy Install is not available."
    fi
}

# Main function to coordinate environment setup and library installation
main() {
    echo "Starting Conda environment setup and library installation..."
    setup_environment "$1"
    install_libs
    echo "All tasks completed successfully."
}

# Start the script with the provided argument (if any)
main "$1"
