#!/bin/sh

setup_environment() {
    PYTHON_PATH=/opt/python/current
    IGNORE_ERRORS=E302
}

run_check() {
    $PYTHON_PATH/bin/flake8 --ignore=$IGNORE_ERRORS $*
}

autofix() {
    shift && $PYTHON_PATH/bin/autopep8 --ignore=$IGNORE_ERRORS -v -i $*
}

main() {
    setup_environment
    which $PYTHON_PATH/bin/flake8 > /dev/null || exit 254
    test "$1" = "-i" || run_check $*
    test "$1" = "-i" && autofix $*
    exit 0
}

main $*
