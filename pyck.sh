#!/bin/sh

setup_environment() {
    PYTHON_PATH=/opt/python/current
    IGNORE_ERRORS=E302
}

main() {
    setup_environment
    which $PYTHON_PATH/bin/flake8 > /dev/null || exit 254
    $PYTHON_PATH/bin/flake8 --ignore=$IGNORE_ERRORS $*
    exit 0
}

main $*
