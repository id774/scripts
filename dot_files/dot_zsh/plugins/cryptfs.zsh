# cryptfs.zsh
# Last Change: 24-May-2011.
# Maintainer:  id774 <idnanashi@gmail.com>

mount_cryptfs() {
    test -d $HOME/crypt/bin && test -d $HOME/bin || cryptmount home-$USER-crypt
}

test -r $HOME/local/crypt.fs && mount_cryptfs
