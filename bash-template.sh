#!/bin/bash

# Copyright (c) <YEAR>, <OWNER>
# All rights reserved.

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met: 

# 1. Redistributions of source code must retain the above copyright notice,
#    this list of conditions and the following disclaimer. 
# 2. Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution. 

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
# ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
# ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

# The views and conclusions contained in the software and documentation are those
# of the authors and should not be interpreted as representing official policies, 
# either expressed or implied, of the FreeBSD Project.


usage() {
  cat << EOS
Usage :
  SCRIPT_NAME [options] command [args..]

COMMANDS:
   a            A command.
   b            B command.
   c            C command.
   help         Shows a list of commands or help.

OPTIONS:
   --opt1, -1   Option 1.
   --opt2, -2   Option 2.
EOS
}

command_a() {
    echo "A command"
}

command_b() {
    echo "B command"
}

command_c() {
    echo "C command"
}

setup() {
    if [ ${opt1} -eq 1 ] ; then
        echo opt1: true
    fi
    if [ ${opt2} -eq 1 ] ; then
        echo opt2: true
    fi
    for arg in ${args[@]}; do
        echo args: ${arg}
    done
}

main() {
    args=()
    opt1=0
    opt2=0

    for arg in $*
    do
      case "${arg}" in
        -1|--opt1)  opt1=1 ;;
        -2|--opt2)  opt2=1 ;;
        help|a|b|c) cmd="${arg}" ;;
        * )         args+=("${arg}")
      esac
    done

    if [ -z "${cmd}" ] ; then
        usage
    else
      setup
      case "$cmd" in
        help) usage  ;;
        a)    command_a ;;
        b)    command_b ;;
        c)    command_c ;;
        * )   usage  ;;
      esac
    fi
}

main $*
