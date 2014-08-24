#!/bin/sh

setup_environment() {
    PYTHON_PATH=/opt/python/current
    IGNORE_ERRORS=E302
    test -n "$1" || DIR="."
    test -n "$1" && DIR=$1
}

main() {
    setup_environment $*
    which $PYTHON_PATH/bin/autopep8 > /dev/null || exit 254
    which $PYTHON_PATH/bin/flake8 > /dev/null || exit 254
    $PYTHON_PATH/bin/flake8 --ignore=$IGNORE_ERRORS $DIR | cut -d: -f 1 | sort | uniq | xargs $PYTHON_PATH/bin/autopep8 --ignore=$IGNORE_ERRORS -v -i
    exit 0
}

main $*
