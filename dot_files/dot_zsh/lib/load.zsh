# load.zsh
# Last Change: 24-May-2011.
# Maintainer:  id774 <idnanashi@gmail.com>

call_screen() {
    if [ -f $HOME/.zsh/lib/screen.zsh ]; then
        source $HOME/.zsh/lib/screen.zsh
    elif [ -f /etc/zsh/lib/screen.zsh ]; then
        source /etc/zsh/lib/screen.zsh
    fi
}

load_plugins() {
    if [ -d /etc/zsh/plugins ]; then
        source /etc/zsh/plugins/*
    fi
    if [ -d $HOME/.zsh/plugins ]; then
        source $HOME/.zsh/plugins/*
    fi
}

load_local_settings() {
    if [ -f $HOME/.zshrc_local ]; then
        source $HOME/.zshrc_local
    fi
    if [ -f $HOME/.zshrc.local ]; then
        source $HOME/.zshrc.local
    fi
}

load_base() {
    if [ -f $HOME/.zsh/lib/base.zsh ]; then
        source $HOME/.zsh/lib/base.zsh
    elif [ -f /etc/zsh/lib/base.zsh ]; then
        source /etc/zsh/lib/base.zsh
    fi
}

load_main() {
    load_base
    load_local_settings
    load_plugins
    call_screen
}

load_main
