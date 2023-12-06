#!/bin/sh

########################################################################
# anthy_create_aa_dic.sh: Create Anthy Dictionary for ASCII Art
#
#  Description:
#  This script creates a custom dictionary for Anthy, a Japanese input
#  method, using ASCII art (AA) entries. It converts an MS-IME format
#  dictionary to the Canna format using the convert_msime2cannna.rb script.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2023-12-06
#       Refactored for improved readability and added detailed comments.
#  v1.0 2009-11-17
#       Initial release.
#
#  Usage:
#  ./anthy_create_aa_dic.sh
#
########################################################################

# Ensure necessary files and directories exist
test -f $SCRIPTS/convert_msime2cannna.rb || { echo "Missing convert_msime2cannna.rb"; exit 1; }
test -f $SCRIPTS/etc/aa.txt || { echo "Missing aa.txt"; exit 1; }
test -d $HOME/.anthy || mkdir $HOME/.anthy

# Create Anthy dictionary for ASCII Art
ruby -Ku $SCRIPTS/convert_msime2cannna.rb < $SCRIPTS/etc/aa.txt > $HOME/.anthy/private_words_default
echo "Dictionary created."

