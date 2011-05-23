# screen.zsh
# Last Change: 23-May-2011.
# Maintainer:  id774 <idnanashi@gmail.com>

call_exec_screen() {
    if [ "$TERM" != "linux" ]; then
        if [ `ps ax | grep screen | grep -v grep | wc -l` = 0 ]; then
            exec screen -U -D -RR
        fi

        case "${TERM}" in
          *xterm*|rxvt|(dt|k|E)term)
            exec screen -U -D -RR
            ;;
        esac
    fi
}

call_exec_screen
