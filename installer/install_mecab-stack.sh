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
#  Usage:
#      ./install_mecab-stack.sh [INSTALL_PREFIX] [-n]
#
#  INSTALL_PREFIX: Optional. Defaults to /opt/mecab-stack if not specified.
#  -n            : Optional. If specified, skips saving source files.
#
#  Features:
#  - Installs MeCab morphological analyzer (v0.996).
#  - Installs mecab-ipadic-NEologd enhanced dictionary.
#  - Installs CaboCha Japanese syntactic analyzer (v0.69).
#  - Installs all binaries, libraries, dictionaries, and configs under a unified prefix.
#
#  Warning:
#  - Performs sudo installation steps where required.
#  - Includes network and dependency checks.
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
#    For Python (CaboCha):
#      1. Navigate to the CaboCha Python binding directory:
#         cd /usr/local/src/mecab-stack/cabocha/python
#      2. Build and install the Python binding:
#         python3 setup.py build
#         sudo python3 setup.py install --prefix=$PREFIX
#      3. Ensure the installed path is included in your PYTHONPATH.
#
#    For Ruby (CaboCha):
#      1. Navigate to the CaboCha Ruby binding directory:
#         cd /usr/local/src/mecab-stack/cabocha/ruby
#      2. Build and install the Ruby binding:
#         ruby extconf.rb
#         make
#         sudo make install
#      3. This installs the CaboCha extension for your Ruby environment.
#
#  MeCab bindings are not bundled with the official 0.996 release.
#  Please refer to third-party projects if Python or Ruby integration is needed.
#
#  Version History:
#  v1.3 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.2 2025-04-28
#       Refine error handling: apply checks only where critical, improve readability.
#  v1.1 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.0 2025-03-27
#       Initial release. Installs MeCab, NEologd, CaboCha with source preservation options.
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

# Display full script header information extracted from the top comment block
usage() {
    awk '
        BEGIN { in_header = 0 }
        /^#{10,}$/ { if (!in_header) { in_header = 1; next } else exit }
        in_header && /^# ?/ { print substr($0, 3) }
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
}

# Check if required commands are available and executable
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

# Check network connectivity
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
        -h|--help|-v|--version) usage ;;
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
    sudo mkdir -p "${PREFIX}" || exit 1
}

# Download, extract, and install MeCab core
install_mecab() {
    echo "[INFO] Installing MeCab ${MECAB_VERSION}..."

    if ! wget "http://files.id774.net/archive/mecab-${MECAB_VERSION}.tar.gz"; then
        echo "[ERROR] Failed to download mecab-${MECAB_VERSION}.tar.gz." >&2
        exit 1
    fi

    if ! tar xzvf "mecab-${MECAB_VERSION}.tar.gz"; then
        echo "[ERROR] Failed to extract mecab-${MECAB_VERSION}.tar.gz." >&2
        exit 1
    fi

    cd "mecab-$MECAB_VERSION" || exit 1

    if ! ./configure --enable-utf8-only --prefix="${PREFIX}"; then
        echo "[ERROR] Failed to configure MeCab." >&2
        exit 1
    fi

    if ! make; then
        echo "[ERROR] Failed to build MeCab." >&2
        exit 1
    fi

    if ! sudo make install; then
        echo "[ERROR] Failed to install MeCab." >&2
        exit 1
    fi

    cd .. || exit 1

    save_source "mecab-${MECAB_VERSION}" "mecab-${MECAB_VERSION}"
}

# Download, extract, and install IPADIC dictionary
install_ipadic() {
    echo "[INFO] Installing IPADIC ${IPADIC_VERSION}..."

    if ! wget "http://files.id774.net/archive/mecab-ipadic-${IPADIC_VERSION}.tar.gz"; then
        echo "[ERROR] Failed to download mecab-ipadic-${IPADIC_VERSION}.tar.gz." >&2
        exit 1
    fi

    if ! tar xzvf "mecab-ipadic-${IPADIC_VERSION}.tar.gz"; then
        echo "[ERROR] Failed to extract mecab-ipadic-${IPADIC_VERSION}.tar.gz." >&2
        exit 1
    fi

    cd "mecab-ipadic-${IPADIC_VERSION}" || exit 1

    if ! ./configure --with-charset=utf8 --prefix="${PREFIX}"; then
        echo "[ERROR] Failed to configure mecab-ipadic." >&2
        exit 1
    fi

    if ! make; then
        echo "[ERROR] Failed to build mecab-ipadic." >&2
        exit 1
    fi

    if ! sudo make install; then
        echo "[ERROR] Failed to install mecab-ipadic." >&2
        exit 1
    fi

    cd .. || exit 1

    save_source "mecab-ipadic-${IPADIC_VERSION}" "mecab-ipadic-${IPADIC_VERSION}"
}

# Download, extract, and install NAIST-JDIC dictionary
install_naistdic() {
    echo "[INFO] Installing NAIST-JDIC ${NAISTDIC_VERSION}..."

    if ! wget "http://files.id774.net/archive/naistdic.tar.gz"; then
        echo "[ERROR] Failed to download naistdic.tar.gz." >&2
        exit 1
    fi

    if ! tar xzvf "naistdic.tar.gz"; then
        echo "[ERROR] Failed to extract naistdic.tar.gz." >&2
        exit 1
    fi

    cd "mecab-naist-jdic-${NAISTDIC_VERSION}" || exit 1

    if ! ./configure --with-charset=utf8 --prefix="${PREFIX}"; then
        echo "[ERROR] Failed to configure mecab-naist-jdic." >&2
        exit 1
    fi

    if ! make; then
        echo "[ERROR] Failed to build mecab-naist-jdic." >&2
        exit 1
    fi

    if ! sudo make install; then
        echo "[ERROR] Failed to install mecab-naist-jdic." >&2
        exit 1
    fi

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

    if ! git clone --depth 1 https://github.com/neologd/mecab-ipadic-neologd.git; then
        echo "[WARN] Failed to clone mecab-ipadic-neologd from GitHub. Skipping NEologd installation." >&2
        return 0
    fi

    cd mecab-ipadic-neologd || exit 1

    if ! ./bin/install-mecab-ipadic-neologd -n -y -p "${PREFIX}/lib/mecab/dic/mecab-ipadic-neologd"; then
        echo "[ERROR] Failed to install mecab-ipadic-neologd." >&2
        exit 1
    fi

    cd .. || exit 1
    save_source "mecab-ipadic-neologd" "mecab-ipadic-neologd"

    cd .. || exit 1
}

# Install CRF++
install_crfpp() {
    echo "[INFO] Installing CRF++ $CRF_VERSION..."

    cd "$BUILD_DIR" || exit 1

    if ! wget "http://files.id774.net/archive/CRF%2B%2B-${CRF_VERSION}.tar.gz"; then
        echo "[ERROR] Failed to download CRF++-${CRF_VERSION}.tar.gz." >&2
        exit 1
    fi

    if ! tar xzvf "CRF++-${CRF_VERSION}.tar.gz"; then
        echo "[ERROR] Failed to extract CRF++-${CRF_VERSION}.tar.gz." >&2
        exit 1
    fi

    cd "CRF++-${CRF_VERSION}" || exit 1

    if ! ./configure --prefix="${PREFIX}"; then
        echo "[ERROR] Failed to configure CRF++." >&2
        exit 1
    fi

    if ! make; then
        echo "[ERROR] Failed to build CRF++." >&2
        exit 1
    fi

    if ! sudo make install; then
        echo "[ERROR] Failed to install CRF++." >&2
        exit 1
    fi

    cd .. || exit 1
    save_source "CRF++-${CRF_VERSION}" "CRF++-${CRF_VERSION}"

    cd .. || exit 1
}

# Install CaboCha
install_cabocha() {
    echo "[INFO] Installing CaboCha ${CABOCHA_VERSION}..."

    cd "$BUILD_DIR" || exit 1

    if ! wget "http://files.id774.net/archive/cabocha-${CABOCHA_VERSION}.tar.bz2"; then
        echo "[ERROR] Failed to download cabocha-${CABOCHA_VERSION}.tar.bz2." >&2
        exit 1
    fi

    if ! tar xjvf "cabocha-${CABOCHA_VERSION}.tar.bz2"; then
        echo "[ERROR] Failed to extract cabocha-${CABOCHA_VERSION}.tar.bz2." >&2
        exit 1
    fi

    cd "cabocha-${CABOCHA_VERSION}" || exit 1

    if ! CPPFLAGS="-I${PREFIX}/include" LDFLAGS="-L${PREFIX}/lib" ./configure --with-charset=UTF8 --with-posset=IPA --with-mecab-config="${PREFIX}/bin/mecab-config" --prefix="${PREFIX}"; then
        echo "[ERROR] Failed to configure CaboCha." >&2
        exit 1
    fi

    if ! make; then
        echo "[ERROR] Failed to build CaboCha." >&2
        exit 1
    fi

    if ! sudo make install; then
        echo "[ERROR] Failed to install CaboCha." >&2
        exit 1
    fi

    cd .. || exit 1
    save_source "cabocha-${CABOCHA_VERSION}" "cabocha-${CABOCHA_VERSION}"
    cd .. || exit 1
}

# Save source code to SRC_BASE
save_source() {
    dir="$1"
    name="$2"

    if [ "$SAVE_SOURCE" -eq 1 ]; then
        echo "[INFO] Saving source for $name to $SRC_BASE/$name."

        if ! sudo mkdir -p "$SRC_BASE"; then
            echo "[ERROR] Failed to create directory: $SRC_BASE" >&2
            exit 1
        fi

        if [ -e "$SRC_BASE/$name" ] || [ -L "$SRC_BASE/$name" ]; then
            if ! sudo rm -rf "$SRC_BASE/$name"; then
                echo "[ERROR] Failed to remove existing source directory: $SRC_BASE/$name." >&2
                exit 1
            fi
        fi

        if ! sudo cp -r "$dir" "$SRC_BASE/$name"; then
            echo "[ERROR] Failed to copy source from $dir to $SRC_BASE/$name." >&2
            exit 1
        fi
    fi
}

# Remove temporary files
cleanup() {
    echo "[INFO] Cleaning up build directory..."
    rm -rf "$BUILD_DIR"
}

# Main entry point of the script
main() {
    parse_args "$@"
    check_commands curl git make gcc g++ gzip tar wget awk
    check_network
    check_sudo

    echo "[INFO] Starting installation of MeCab stack..."
    echo "[INFO] Installation prefix: ${PREFIX}."

    # Ensure PREFIX/bin is in PATH so mecab-config and other tools are found
    export PATH="${PREFIX}/bin:$PATH"

    prepare_dirs
    install_mecab_stack
    install_neologd
    install_crfpp
    install_cabocha
    cleanup

    echo "[INFO] Installation complete at ${PREFIX}."
    final_message
    return 0
}

# Execute main function
main "$@"
exit $?
