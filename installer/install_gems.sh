#!/bin/sh

########################################################################
# install_gems.sh: Bulk Ruby Gem Install Script
#
#  Description:
#  This script automates the installation of a comprehensive set of Ruby
#  gems required for web development, data processing, and other purposes.
#  It updates the gem system to the latest version and installs frequently
#  used gems. Proxy settings are supported via the HTTP_PROXY environment
#  variable, ensuring compatibility with various network configurations.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run this script in a terminal to set up your Ruby environment.
#
#  Examples:
#      ./install_gems.sh /path/to/ruby
#      ./install_gems.sh
#
#  Requirements:
#  - Ruby and gem must be installed prior to executing this script.
#
#  Exit Codes:
#  0: Success - All gems were installed successfully.
#  1: Error - A critical issue occurred (e.g., missing dependencies).
#
#  Notes:
#  - If no path is provided, the script assumes default tools in PATH.
#  - Proxy support can be configured using the HTTP_PROXY environment variable.
#
#  Version History:
#  v3.3 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v3.2 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v3.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v3.0 2025-01-16
#       Official release. Removed sudo dependency, improved documentation,
#       and refined error handling.
#  v2.4 2019-01-14
#       Options removed.
#  v2.3 2014-07-08
#       Auto proxy detection.
#  v2.2 2014-01-30
#       No sudo option.
#  v2.1 2013-04-21
#       Options as default.
#  v2.0 2012-10-31
#       Added some gems.
#  v1.0 2008-08-15
#       Stable.
#
########################################################################

# Display full script header information extracted from the top comment block
usage() {
    awk '
        BEGIN { in_header = 0 }
        /^#{10,}$/ { if (!in_header) { in_header = 1; next } else exit }
        in_header && /^# ?/ { print substr($0, 3) }
    ' "$0"
    exit 0
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

# Set up the environment variables for gem
setup_environment() {
    if [ -n "$1" ]; then
        export GEM=$1/bin/gem
    else
        export GEM=gem
    fi

    # Verify that gem is available
    check_commands "$GEM" sed

    # Set proxy if HTTP_PROXY is defined
    if [ -n "$HTTP_PROXY" ]; then
        PROXY="--http-proxy $HTTP_PROXY"
    else
        PROXY=""
    fi
}

# Install a single Ruby gem
install_gem() {
    echo "[INFO] Installing $1..."
    $GEM install $PROXY "$1"
}

# Install the necessary Ruby gems
install_gems() {
    echo "[INFO] Updating gem system to the latest version..."
    $GEM update --system $PROXY

    echo "[INFO] Installing essential Ruby gems..."
    # Define the list of gems as a multi-line string
    gems="
    rails
    debugger
    pry
    pry-doc
    pry-rails
    pry-coolline
    hirb
    awesome_print
    rake
    bundler
    builder
    sqlite3
    sass
    sass-rails
    compass-rails
    coffee-script
    coffee-rails
    jquery-rails
    haml
    haml-rails
    dalli
    backbone-rails
    uglifier
    shotgun
    rails-clean-logs
    devise
    i18n_generators
    jeweler
    gemcutter
    kaminari
    kaminari-bootstrap
    rinku
    yaml_db
    term-ansicolor
    http_configuration
    mechanize
    nokogiri
    anemone
    cosmicrawler
    sanitize
    Selenium
    bson
    bson_ext
    mongo
    net-ssh
    net-scp
    net-sftp
    net-ping
    unicorn
    capistrano
    redgreen
    minitest
    rspec
    rspec-rails
    rspec-mocks
    flexmock
    simplecov
    sequel
    puppet
    gherkin
    cucumber
    capybara
    webtail
    zipruby
    diff-lcs
    msgpack
    log4r
    foreman
    sinatra
    formkeeper-japanese
    redis
    sidekiq
    rubytree
    shoulda
    request-log-analyzer
    apache-loggen
    tzinfo
    rb-fsevent
    iconv
    i18n
    feedbag
    hashie
    gcalapi
    xml-simple
    pocket-ruby
    spreadsheet
    prawn
    pdfkit
    wkhtmltopdf-binary-edge
    gnuplot
    screening
    ctoD
    count_by
    poppler
    gsl
    woothee
    daemons
    eventmachine
    "

    # Loop through each gem and install it
    for gem in $gems; do
        # Remove leading and trailing spaces/tabs
        gem=$(echo "$gem" | sed 's/^[ \t]*//;s/[ \t]*$//')
        $GEM install $PROXY "$gem"
    done

    # Special installation for tomz-liblinear-ruby-swig
    echo "[INFO] Installing tomz-liblinear-ruby-swig from a specific source..."
    $GEM install --source http://gems.github.com tomz-liblinear-ruby-swig $PROXY

    # Conditional installation for rsruby based on Linux distribution
    if [ -f /etc/debian_version ]; then
        $GEM install rsruby $PROXY -- --with-R-include=/usr/include/R --with-R-dir=/usr/lib/R
    elif [ -f /etc/redhat-release ]; then
        $GEM install rsruby $PROXY -- --with-R-include=/usr/share/R/include --with-R-dir=/usr/lib/R
    fi

    echo "[INFO] Listing installed gems..."
    $GEM list --local
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    echo "[INFO] Starting Ruby gem installation..."
    setup_environment "$1"
    install_gems

    echo "[INFO] All specified Ruby gem packages have been installed."
    return 0
}

# Execute main function
main "$@"
