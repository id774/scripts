#!/bin/sh

setup_environment() {
    PYTHON_PATH=/opt/python/current
    IGNORE_ERRORS=E302
}

main() {
    setup_environment
    which $PYTHON_PATH/bin/pyflakes > /dev/null || exit 254
    which $PYTHON_PATH/bin/pep8 > /dev/null || exit 254
    $PYTHON_PATH/bin/pyflakes $*
    $PYTHON_PATH/bin/pep8 --ignore=$IGNORE_ERRORS --repeat $*
    exit 0
}

main $*
