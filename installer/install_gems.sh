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
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
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
#  Usage:
#  Run this script in a terminal to set up your Ruby environment.
#  Examples:
#     ./install_gems.sh /path/to/ruby
#     ./install_gems.sh
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
########################################################################

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


# Function to check required commands
check_commands() {
    for cmd in "$@"; do
        cmd_path=$(command -v "$cmd" 2>/dev/null)
        if [ -z "$cmd_path" ]; then
            echo "Error: Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif [ ! -x "$cmd_path" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Function to set up the environment variables for gem
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

# Function to install a single Ruby gem
install_gem() {
    echo "Installing $1..."
    $GEM install $PROXY "$1"
}

# Function to install the necessary Ruby gems
install_gems() {
    echo "Updating gem system to the latest version..."
    $GEM update --system $PROXY

    echo "Installing essential Ruby gems..."
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
        echo "Installing $gem..."
        $GEM install $PROXY "$gem"
    done

    # Special installation for tomz-liblinear-ruby-swig
    echo "Installing tomz-liblinear-ruby-swig from a specific source..."
    $GEM install --source http://gems.github.com tomz-liblinear-ruby-swig $PROXY

    # Conditional installation for rsruby based on Linux distribution
    if [ -f /etc/debian_version ]; then
        $GEM install rsruby $PROXY -- --with-R-include=/usr/include/R --with-R-dir=/usr/lib/R
    elif [ -f /etc/redhat-release ]; then
        $GEM install rsruby $PROXY -- --with-R-include=/usr/share/R/include --with-R-dir=/usr/lib/R
    fi

    echo "Listing installed gems..."
    $GEM list --local
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    echo "Starting Ruby gem installation..."
    setup_environment "$1"
    install_gems
    echo "All tasks completed successfully."
}

# Execute main function
main "$@"
