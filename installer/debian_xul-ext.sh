#!/bin/sh

smart_apt() {
    while [ $# -gt 0 ]
    do
        if [ `aptitude search $1 | awk '/^i/' | wc -l` = 0 ]; then
            sudo apt-get -y install $1
        fi
        shift
    done
}

xul-ext_packages() {
    smart_apt \
      xul-ext-adblock-plus \
      xul-ext-firebug \
      xul-ext-greasemonkey \
      xul-ext-noscript \
      xul-ext-scrapbook \
      xul-ext-toggle-proxy \
      xul-ext-useragentswitcher \
      xul-ext-webdeveloper
}

xul-ext_packages $*
