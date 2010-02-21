#!/bin/sh

########################################################################
# Create environment of "update-alternatives" for python
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.0 2/21,2010
#       Stable.
########################################################################

remove_alternatives() {
  while [ $# -gt 0 ]
  do
    sudo update-alternatives --remove-all $1
    shift
  done
}

update_alternatives() {
    sudo update-alternatives \
      --install $1/python                  python                  $2/python$4 $3\
      --slave   $1/python-config           python-config           $2/python-config$4\
      --slave   $1/pydoc                   pydoc                   $2/pydoc$4\
      --slave   $1/idle                    idle                    $2/idle$4\
      --slave   $1/ipython                 ipython                 $2/ipython\
      --slave   $1/easy_install            easy_install            $2/easy_install\
      --slave   $1/py_compilefiles         py_compilefiles         $2/py_compilefiles\
      --slave   $1/pygettext               pygettext               $2/pygettext\
      --slave   $1/pygmentize              pygmentize              $2/pygmentize\
      --slave   $1/pycolor                 pycolor                 $2/pycolor\
      --slave   $1/pycentral               pycentral               $2/pycentral\
      --slave   $1/pysupport-parseversions pysupport-parseversions $2/pysupport-parseversions\
      --slave   $1/pysupport-movemodules   pysupport-movemodules   $2/pysupport-movemodules\
      --slave   $1/2to3                    2to3                    $2/2to3\
      --slave   $1/django-admin.py         django                  $2/django-admin.py
}

make_all_alternatives() {
    TARGET=/opt/bin
    SOURCE=/usr/bin
    PRIORITY=90
    SUFFIX=
    update_alternatives $TARGET $SOURCE $PRIORITY $SUFFIX
    SOURCE=/usr/local/bin
    PRIORITY=100
    update_alternatives $TARGET $SOURCE $PRIORITY $SUFFIX
    SOURCE=/opt/python/2.6.4/bin
    PRIORITY=120
    update_alternatives $TARGET $SOURCE $PRIORITY $SUFFIX
    SOURCE=/opt/python/3.1.1/bin
    PRIORITY=150
    SUFFIX=3
    update_alternatives $TARGET $SOURCE $PRIORITY $SUFFIX
}

remove_alternatives python python-config pydoc idle ipython easy_install py_compilefiles pygettext pygmentize pycolor pycentral pysupport-parseversions pysupport-movemodules 2to3 django
make_all_alternatives
sudo update-alternatives --config python
