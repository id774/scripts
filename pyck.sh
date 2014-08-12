#!/bin/sh

setup_environment() {
    PYTHON_PATH=/opt/python/current
}

main() {
    setup_environment
    which $PYTHON_PATH/bin/pyflakes > /dev/null || exit 1
    which $PYTHON_PATH/bin/pep8 > /dev/null || exit 1
    $PYTHON_PATH/bin/pyflakes $*
    $PYTHON_PATH/bin/pep8 --ignore=E221,E701,E202 --repeat $*
    exit 0
}

main $*
