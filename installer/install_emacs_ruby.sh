#!/bin/sh
#
########################################################################
# Install Emacs Ruby
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.2 9/16,2010
#       Refactoring.
#  v1.1 5/18,2008
#       Emacs byte compile.
#  v1.0 4/29,2009
#       Stable.
########################################################################

install_emacs_ruby() {
    test -n "$1" || exit 1
    test -n "$1" && RUBY_MISC=$1
    test -n "$2" || SITE_LISP=/usr/local/share/emacs/site-lisp
    test -n "$2" && SITE_LISP=$2
    test -L $SITE_LISP/ruby-mode.el && sudo rm $SITE_LISP/ruby-mode.el
    sudo ln -s $RUBY_MISC/ruby-mode.el $SITE_LISP/ruby-mode.el
    test -L $SITE_LISP/ruby-style.el && sudo rm $SITE_LISP/ruby-style.el
    sudo ln -s $RUBY_MISC/ruby-style.el $SITE_LISP/ruby-style.el
    test -L $SITE_LISP/ruby-electric.el && sudo rm $SITE_LISP/ruby-electric.el
    sudo ln -s $RUBY_MISC/ruby-electric.el $SITE_LISP/ruby-electric.el
    test -L $SITE_LISP/inf-ruby.el && sudo rm $SITE_LISP/inf-ruby.el
    sudo ln -s $RUBY_MISC/inf-ruby.el $SITE_LISP/inf-ruby.el
    test -L $SITE_LISP/rubydb2x.el && sudo rm $SITE_LISP/rubydb2x.el
    sudo ln -s $RUBY_MISC/rubydb2x.el $SITE_LISP/rubydb2x.el
    test -L $SITE_LISP/rubydb3x.el && sudo rm $SITE_LISP/rubydb3x.el
    sudo ln -s $RUBY_MISC/rubydb3x.el $SITE_LISP/rubydb3x.el
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

install_emacs_ruby $1 $2
