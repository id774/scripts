#!/bin/sh
#
########################################################################
# Install Emacs Ruby
#  $1 = ruby misc path (ex. /usr/local/src/ruby/ruby-1.9.3-p385/misc)
#  $2 = site list path (ex. ~/.emacs.d/elisp/3rd-party/ruby-mode)
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.5 3/15,2013
#       Fix permissions, refactoring.
#  v1.4 11/15,2011
#       Show usage.
#  v1.3 9/13,2011
#       Renew target path and change symlink to filecopy.
#  v1.2 9/16,2010
#       Refactoring.
#  v1.1 5/18,2008
#       Emacs byte compile.
#  v1.0 4/29,2009
#       Stable.
########################################################################

setup_environment() {
    test -n "$1" || echo "Usage: $0 [ruby-misc] [site-lisp]"
    test -n "$1" || exit 1
    test -n "$1" && RUBY_MISC=$1
    test -n "$2" || SITE_LISP=$HOME/.emacs.d/elisp/3rd-party/ruby-mode
    test -n "$2" && SITE_LISP=$2
    case $OSTYPE in
      *darwin*)
        OWNER=root:wheel
        ;;
      *)
        OWNER=root:root
        ;;
    esac
}

install_emacs_ruby() {
    setup_environment $*
    test -f $SITE_LISP/ruby-mode.el && sudo rm $SITE_LISP/ruby-mode.el
    sudo cp $RUBY_MISC/ruby-mode.el $SITE_LISP/ruby-mode.el
    test -f $SITE_LISP/ruby-style.el && sudo rm $SITE_LISP/ruby-style.el
    sudo cp $RUBY_MISC/ruby-style.el $SITE_LISP/ruby-style.el
    test -f $SITE_LISP/ruby-electric.el && sudo rm $SITE_LISP/ruby-electric.el
    sudo cp $RUBY_MISC/ruby-electric.el $SITE_LISP/ruby-electric.el
    test -f $SITE_LISP/inf-ruby.el && sudo rm $SITE_LISP/inf-ruby.el
    sudo cp $RUBY_MISC/inf-ruby.el $SITE_LISP/inf-ruby.el
    test -f $SITE_LISP/rubydb2x.el && sudo rm $SITE_LISP/rubydb2x.el
    sudo cp $RUBY_MISC/rubydb2x.el $SITE_LISP/rubydb2x.el
    test -f $SITE_LISP/rubydb3x.el && sudo rm $SITE_LISP/rubydb3x.el
    sudo cp $RUBY_MISC/rubydb3x.el $SITE_LISP/rubydb3x.el
    cd $RUBY_MISC
    test -f inf-ruby.elc && sudo rm inf-ruby.elc
    sudo emacs --batch --eval '(byte-compile-file "inf-ruby.el")'
    test -f ruby-mode.elc && sudo rm ruby-mode.elc
    sudo emacs --batch --eval '(byte-compile-file "ruby-mode.el")'
    test -f rdoc-mode.elc && sudo rm rdoc-mode.elc
    sudo emacs --batch --eval '(byte-compile-file "rdoc-mode.el")'
    test -f ruby-style.elc && sudo rm ruby-style.elc
    sudo emacs --batch --eval '(byte-compile-file "ruby-style.el")'
    test -f ruby-electric.elc && sudo rm ruby-electric.elc
    sudo emacs --batch --eval '(byte-compile-file "ruby-electric.el")'
    test -f rubydb2x.elc && sudo rm rubydb2x.elc
    sudo emacs --batch --eval '(byte-compile-file "rubydb2x.el")'
    test -f rubydb3x.elc && sudo rm rubydb3x.elc
    sudo emacs --batch --eval '(byte-compile-file "rubydb3x.el")'
    sudo chown -R $OWNER $SITE_LISP
}

install_emacs_ruby $*
