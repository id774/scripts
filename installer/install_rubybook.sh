#!/bin/sh
#
########################################################################
# Install Programming Ruby
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 2010-09-22
#       First.
########################################################################

setup_environment() {
    case $OSTYPE in
      *darwin*)
        OWNER=root:wheel
        ;;
      *)
        OWNER=root:root
        ;;
    esac
}

apache_settings() {
    sudo vi /etc/apache2/sites-available/custom*
    sudo /etc/init.d/apache2 restart
}

create_html_link() {
    sudo chown -R $OWNER /usr/share/doc/rubybook/html
    sudo ln -s /usr/share/doc/rubybook/html /var/www/html
}

install_rubybook() {
    sudo apt-get -y install rubybook
}

main() {
    setup_environment
    install_rubybook
    create_html_link
    apache_settings
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
main
