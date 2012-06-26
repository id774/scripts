#!/bin/sh
#
########################################################################
# Bulk Apt Install Script for Debian
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.3 10/3,2011
#       Implement smart_apt function.
#  v0.2 9/28,2011
#       Cut off desktop suite.
#  v0.1 6/16,2011
#       Forked from Initial Setup Script.
########################################################################

smart_apt() {
    while [ $# -gt 0 ]
    do
        if [ `aptitude search $1 | awk '/^i/' | wc -l` = 0 ]; then
            sudo apt-get -y install $1
        fi
        shift
    done
}

apt_upgrade() {
    sudo apt-get update && \
      sudo apt-get -y upgrade && \
      sudo apt-get autoclean && \
      sudo apt-get -y autoremove
}

basic_packages() {
    smart_apt \
      vim \
      w3m \
      lynx \
      wget \
      openssh-server ssh \
      rsync \
      build-essential \
      gcc g++ \
      scons \
      tar zip gzip unzip bzip2 \
      p7zip p7zip-full \
      zsh \
      screen
#      curl \
#      ncftp \
#      p7zip-rar \
#      lha \
}

system_packages() {
    smart_apt \
      rsyslog \
      ntp \
      keychain \
      fail2ban \
      locales \
      nkf \
      mailx \
      mutt \
      sharutils \
      digitools \
      dnsutils \
      ethtool \
      xdelta \
      sshfs \
      exim4 \
      sysstat \
      dstat \
      anacron \
      clamav \
      chkrootkit \
      vrms \
      manpages-ja \
      manpages-ja-dev \
      xmanpages-ja \
      lm-sensors \
      hddtemp \
      smartmontools
#      linux-source \
#      checkinstall \
#      alien \
}

debian_developer_tools() {
    smart_apt \
      dpkg-dev \
      lintian \
      debhelper \
      yada \
      equivs \
      cvs-buildpackage \
      dupload \
      fakeroot \
      devscripts \
      debget \
      dh-make \
      libgtk2.0-dev
#      apt-listchanges apt-listbugs \
}

editor_packages() {
    smart_apt \
      texinfo \
      emacs23 emacs23-el \
      mew stunnel ca-certificates \
      w3m-el-snapshot w3m-img imagemagick \
      libmagick9-dev \
      vim vim-runtime colordiff \
      ctags
}

exif_tools() {
    smart_apt \
      exif libimage-exiftool-perl jhead
}

kvm() {
    if [ `egrep '^flags.*(vmx|svm)' /proc/cpuinfo | wc -l` != 0 ]; then
        smart_apt \
          kvm libvirt-bin \
          python-libvirt \
          kqemu-source qemu
#          virt-manager \
    fi
}

xvfb_packages() {
    smart_apt \
      xvfb \
      fluxbox \
      x11vnc
}

lang_packages() {
    smart_apt \
      nasm \
      gauche gauche-dev \
      clisp clisp-dev \
      scheme48 cmuscheme48-el \
      gnu-smalltalk \
      ghc \
      global \
      graphviz graphviz-dev
}

scm_packages() {
    smart_apt \
      subversion \
      mercurial \
      git-core git-all
#      svk \
}

samba_packages() {
    smart_apt \
      samba smbfs smbclient swat
}

sqlite_packages() {
    smart_apt \
      sqlite3 \
      libsqlite3-0 \
      libsqlite3-dev
}

optional_packages() {
    smart_apt \
      gnuserv \
      mingw32 mingw32-binutils mingw32-runtime \
      libxml2 libxml2-dev \
      libxslt-dev libxslt1-dev libxml-dev \
      libxslt-ruby python-libxslt1 \
      expat libexpat-dev \
      libssl-dev libio-socket-ssl-perl libnet-ssleay-perl \
      libtemplate-perl libxml-libxml-perl \
      migemo
}

ruby_lang() {
    smart_apt \
      autoconf byacc bison automake \
      libopenssl-ruby libreadline-dev zlib1g-dev ruby \
      ruby1.8-dev ruby1.8 ri1.8 rdoc1.8 irb1.8 \
      libreadline-ruby1.8 libruby1.8 libopenssl-ruby
      # autoconf-doc
}

nodejs_lang() {
    smart_apt \
      nodejs \
      npm \
      coffeescript
}

apache_packages() {
    smart_apt \
      apache2 \
      apache2-mpm-prefork \
      apache2-utils
}

java_packages() {
    smart_apt \
      openjdk-6-jdk
}

increase_debian_packages() {
    sudo apt-get -y install aptitude
    basic_packages
    system_packages
    debian_developer_tools
    editor_packages
    exif_tools
    #kvm
    #xvfb_packages
    lang_packages
    scm_packages
    #samba_packages
    sqlite_packages
    optional_packages
    ruby_lang
    #nodejs_lang
    apache_packages
    java_packages
}

operation() {
    test -n "$SCRIPTS" || export SCRIPTS=$HOME/scripts
    test -n "$PRIVATE" || export PRIVATE=$HOME/private/scripts
    apt_upgrade
    increase_debian_packages
}

operation $*
