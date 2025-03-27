#!/bin/sh

########################################################################
# install_mecab-stack.sh: Install MeCab, NEologd, and CaboCha to a unified prefix
#
#  Description:
#  This script installs a complete Japanese text processing stack including
#  MeCab (a morphological analyzer), mecab-ipadic-NEologd (an enhanced dictionary
#  with neologism and web-based terms), and CaboCha (a Japanese dependency parser),
#  all into a single, user-specified or default installation directory.
#
#  The installation process downloads source packages for each component,
#  compiles them with the specified prefix (default: /opt/mecab-stack), and installs
#  them with appropriate sudo privileges. Additionally, the NEologd dictionary is
#  built and installed under the correct location in the MeCab prefix.
#
#  All temporary build files are placed in a directory named install_mecab-stack,
#  which is removed after installation unless errors occur. Sources can optionally
#  be preserved under /usr/local/src/mecab-stack unless the '-n' flag is given.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2025-03-27
#       Initial release. Installs MeCab, NEologd, CaboCha with source preservation options.
#
#  Usage:
#      ./install_mecab-stack.sh [INSTALL_PREFIX] [-n]
#
#  INSTALL_PREFIX: Optional. Defaults to /opt/mecab-stack if not specified.
#  -n            : Optional. If specified, skips saving source files.
#
#  Features:
#  - POSIX-compliant script structure
#  - Installs MeCab morphological analyzer (v0.996)
#  - Installs mecab-ipadic-NEologd enhanced dictionary
#  - Installs CaboCha Japanese syntactic analyzer (v0.69)
#  - Installs all binaries, libraries, dictionaries, and configs under a unified prefix
#  - Performs sudo installation steps where required
#  - Includes network and dependency checks
#
#  Warning:
#  - This script modifies your system by installing files under the specified prefix.
#  - Use with caution and test in a controlled environment.
#
#  After Installation:
#  - mecab is available at: $PREFIX/bin/mecab
#  - cabocha is available at: $PREFIX/bin/cabocha
#  - Dictionary path: $PREFIX/lib/mecab/dic/mecab-ipadic-neologd
#
#  Example usage:
#      $PREFIX/bin/mecab -d $PREFIX/lib/mecab/dic/mecab-ipadic-neologd
#      $PREFIX/bin/cabocha -d $PREFIX/lib/mecab/dic/mecab-ipadic-neologd
#
#  For Application Integration:
#  - Set environment variable PATH to include $PREFIX/bin
#  - Set MECABRC to point to mecabrc in $PREFIX/etc if needed
#
#  Python/Ruby Bindings:
#  - This script does not install Python or Ruby language bindings by default.
#    If you want to use CaboCha from Python or Ruby, follow the steps below after installation.
#
#    For Python:
#      1. Navigate to the CaboCha Python binding directory:
#         cd $BUILD_DIR/cabocha-${CABOCHA_VERSION}/python
#      2. Build and install the Python binding:
#         python3 setup.py build
#         sudo python3 setup.py install --prefix=$PREFIX
#      3. Ensure the installed path is included in your PYTHONPATH.
#
#    For Ruby:
#      1. Navigate to the CaboCha root directory:
#         cd $BUILD_DIR/cabocha-${CABOCHA_VERSION}
#      2. Build and install the Ruby binding:
#         ruby extconf.rb
#         make
#         sudo make install
#      3. This installs the CaboCha extension for your Ruby environment.
#
########################################################################

# Install MeCab, NEologd, and CaboCha into a unified directory

# Version information
MECAB_VERSION="0.996"
IPADIC_VERSION="2.7.0-20070801"
NAISTDIC_VERSION="0.6.3b-20111013"
CRF_VERSION="0.58"
CABOCHA_VERSION="0.69"

# Path settings
PREFIX="/opt/mecab-stack"
BUILD_DIR="install_mecab-stack"
SRC_BASE="/usr/local/src/mecab-stack"
SAVE_SOURCE=1

# Display script usage information
usage() {
    awk '
        BEGIN { in_usage = 0 }
        /^#  Usage:/ { in_usage = 1; print substr($0, 4); next }
        /^#{10}/ { if (in_usage) exit }
        in_usage && /^#/ { print substr($0, 4) }
    ' "$0"
    exit 0
}

# Display After Installation Guide
final_message() {
    awk '
        BEGIN { in_usage = 0 }
        /^#  After Installation:/ { in_usage = 1; print substr($0, 4); next }
        /^#{10}/ { if (in_usage) exit }
        in_usage && /^#/ { print substr($0, 4) }
    ' "$0"
    exit 0
}

# Function to check required commands
check_commands() {
    for cmd in "$@"; do
        cmd_path=$(command -v "$cmd" 2>/dev/null)
        if [ -z "$cmd_path" ]; then
            echo "[ERROR] Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif [ ! -x "$cmd_path" ]; then
            echo "[ERROR] Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Function to check network connectivity
check_network() {
    if ! curl -s --head --connect-timeout 5 http://clients3.google.com/generate_204 >/dev/null; then
        echo "[ERROR] No network connection detected. Please check your internet access." >&2
        exit 1
    fi
}

# Check if the user has sudo privileges
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Parse command line arguments
parse_args() {
    case "$1" in
        -h|--help) usage ;;
        -*) : ;;
        "") : ;;
        *) PREFIX="$1"; shift ;;
    esac

    if [ "$1" = "-n" ]; then
        SAVE_SOURCE=0
    fi
}

# Create build and install directories
prepare_dirs() {
    echo "[INFO] Preparing directories..."
    mkdir -p "$BUILD_DIR" || exit 1
    sudo mkdir -p "$PREFIX" || exit 1
}

# Download, extract, and install MeCab core
install_mecab() {
    echo "[INFO] Installing MeCab $MECAB_VERSION"
    wget "http://files.id774.net/archive/mecab-$MECAB_VERSION.tar.gz"
    tar xzvf "mecab-$MECAB_VERSION.tar.gz"
    cd "mecab-$MECAB_VERSION" || exit 1
    ./configure --enable-utf8-only --prefix="$PREFIX"
    make
    sudo make install

    cd .. || exit 1
    save_source "mecab-${MECAB_VERSION}" "mecab-${MECAB_VERSION}"
}

# Download, extract, and install IPADIC dictionary
install_ipadic() {
    echo "[INFO] Installing IPADIC $IPADIC_VERSION"
    wget "http://files.id774.net/archive/mecab-ipadic-$IPADIC_VERSION.tar.gz"
    tar xzvf "mecab-ipadic-$IPADIC_VERSION.tar.gz"
    cd "mecab-ipadic-$IPADIC_VERSION" || exit 1
    ./configure --with-charset=utf8 --prefix="$PREFIX"
    make
    sudo make install

    cd .. || exit 1
    save_source "mecab-ipadic-${IPADIC_VERSION}" "mecab-ipadic-${IPADIC_VERSION}"
}

# Download, extract, and install NAIST-JDIC dictionary
install_naistdic() {
    echo "[INFO] Installing NAIST-JDIC $NAISTDIC_VERSION"
    wget "http://files.id774.net/archive/naistdic.tar.gz"
    tar xzvf "naistdic.tar.gz"
    cd "mecab-naist-jdic-$NAISTDIC_VERSION" || exit 1
    ./configure --with-charset=utf8 --prefix="$PREFIX"
    make
    sudo make install
    cd .. || exit 1
    save_source "mecab-naist-jdic-${NAISTDIC_VERSION}" "mecab-naist-jdic-${NAISTDIC_VERSION}"
}

# Install MeCab and dictionaries
install_mecab_stack() {
    echo "[INFO] Installing MeCab and dictionaries..."
    cd "$BUILD_DIR" || exit 1
    install_mecab
    install_ipadic
    install_naistdic
    cd .. || exit 1
    rm -rf install_mecab
}

# Download and install NEologd dictionary
install_neologd() {
    echo "[INFO] Installing mecab-ipadic-NEologd..."
    cd "$BUILD_DIR" || exit 1
    rm -rf mecab-ipadic-neologd
    git clone --depth 1 https://github.com/neologd/mecab-ipadic-neologd.git || exit 1
    cd mecab-ipadic-neologd || exit 1

    ./bin/install-mecab-ipadic-neologd -n -y -p "$PREFIX/lib/mecab/dic/mecab-ipadic-neologd" || exit 1

    cd .. || exit 1
    save_source "mecab-ipadic-neologd" "mecab-ipadic-neologd"
    cd .. || exit 1
}

# Install CRF++
install_crfpp() {
    echo "[INFO] Installing CRF++ $CRF_VERSION..."
    cd "$BUILD_DIR" || exit 1
    wget "http://files.id774.net/archive/CRF%2B%2B-${CRF_VERSION}.tar.gz" || exit 1
    tar xzvf "CRF++-${CRF_VERSION}.tar.gz" || exit 1
    cd "CRF++-${CRF_VERSION}" || exit 1
    ./configure --prefix="$PREFIX" || exit 1
    make || exit 1
    sudo make install || exit 1

    cd .. || exit 1
    save_source "CRF++-${CRF_VERSION}" "CRF++-${CRF_VERSION}"
    cd .. || exit 1
}

# Install CaboCha
install_cabocha() {
    echo "[INFO] Installing CaboCha ${CABOCHA_VERSION}..."
    cd "$BUILD_DIR" || exit 1
    echo "[INFO] Downloading CaboCha from id774.net..."
    wget "http://files.id774.net/archive/cabocha-${CABOCHA_VERSION}.tar.bz2" || exit 1
    tar xjvf "cabocha-${CABOCHA_VERSION}.tar.bz2" || exit 1
    cd "cabocha-${CABOCHA_VERSION}" || exit 1

    CPPFLAGS="-I$PREFIX/include" \
    LDFLAGS="-L$PREFIX/lib" \
    ./configure --with-charset=UTF8 \
                --with-posset=IPA \
                --with-mecab-config="$PREFIX/bin/mecab-config" \
                --prefix="$PREFIX" || exit 1

    make || exit 1
    sudo make install || exit 1

    cd .. || exit 1
    save_source "cabocha-${CABOCHA_VERSION}" "cabocha-${CABOCHA_VERSION}"
    cd .. || exit 1
}

# Save source code to SRC_BASE
save_source() {
    dir="$1"
    name="$2"
    if [ "$SAVE_SOURCE" -eq 1 ]; then
        echo "[INFO] Saving source for $name to $SRC_BASE/$name"
        sudo mkdir -p "$SRC_BASE"
        sudo rm -rf "$SRC_BASE/$name"
        sudo cp -r "$dir" "$SRC_BASE/$name"
    fi
}

# Remove temporary files
cleanup() {
    echo "[INFO] Cleaning up build directory..."
    rm -rf "$BUILD_DIR"
}

# Main entry point
main() {
    parse_args "$@"
    check_commands curl git make gcc g++ gzip tar wget awk
    check_network
    check_sudo

    echo "[INFO] Starting installation of MeCab stack..."
    echo "[INFO] Installation prefix: $PREFIX"

    # Ensure PREFIX/bin is in PATH so mecab-config and other tools are found
    export PATH="$PREFIX/bin:$PATH"

    prepare_dirs
    install_mecab_stack
    install_neologd
    install_crfpp
    install_cabocha
    cleanup

    echo "[INFO] Installation complete at $PREFIX"
    final_message
}

main "$@"
