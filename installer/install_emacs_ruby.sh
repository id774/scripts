#!/bin/sh
#
########################################################################
# Install Emacs Ruby
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.1 5/18,2008
#       Emacs byte compile.
#  v1.0 4/29,2009
#       Stable.
########################################################################

install_emacs_ruby() {
    test -L $1/ruby-mode.el && sudo rm $1/ruby-mode.el
    sudo ln -s $2/ruby-mode.el $1/ruby-mode.el
    test -L $1/ruby-style.el && sudo rm $1/ruby-style.el
    sudo ln -s $2/ruby-style.el $1/ruby-style.el
    test -L $1/ruby-electric.el && sudo rm $1/ruby-electric.el
    sudo ln -s $2/ruby-electric.el $1/ruby-electric.el
    test -L $1/inf-ruby.el && sudo rm $1/inf-ruby.el
    sudo ln -s $2/inf-ruby.el $1/inf-ruby.el
    test -L $1/rubydb2x.el && sudo rm $1/rubydb2x.el
    sudo ln -s $2/rubydb2x.el $1/rubydb2x.el
    test -L $1/rubydb3x.el && sudo rm $1/rubydb3x.el
    sudo ln -s $2/rubydb3x.el $1/rubydb3x.el
    cd $1
    test -f ruby-mode.elc && sudo rm ruby-mode.elc
    sudo emacs --batch --eval '(byte-compile-file "ruby-mode.el")'
    test -f ruby-style.elc && sudo rm ruby-style.elc
    sudo emacs --batch --eval '(byte-compile-file "ruby-style.el")'
    test -f ruby-electric.elc && sudo rm ruby-electric.elc
    sudo emacs --batch --eval '(byte-compile-file "ruby-electric.el")'
    test -f inf-ruby.elc && sudo rm inf-ruby.elc
    sudo emacs --batch --eval '(byte-compile-file "inf-ruby.el")'
    test -f rubydb2x.elc && sudo rm rubydb2x.elc
    sudo emacs --batch --eval '(byte-compile-file "rubydb2x.el")'
    test -f rubydb3x.elc && sudo rm rubydb3x.elc
    sudo emacs --batch --eval '(byte-compile-file "rubydb3x.el")'
}

install_emacs_ruby /usr/local/share/emacs/site-lisp $1
