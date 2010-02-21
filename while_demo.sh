#!/bin/sh

while_demo() {
  while [ $# -gt 0 ]
  do
    echo $1
    shift
  done
}

while_demo $*
