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

# Function to check if a command exists in the PATH
check_command() {
    if ! command -v "$1" >/dev/null 2>&1; then
        echo "Error: $1 is not installed or not in PATH." >&2
        exit 1
    fi
}

# Function to set up the environment variables for gem
setup_environment() {
    if [ -n "$1" ]; then
        export GEM=$1/bin/gem
    else
        export GEM=gem
    fi

    # Verify that gem is available
    check_command "$GEM"
    check_command sed

    # Set proxy if HTTP_PROXY is defined
    if [ -n "$HTTP_PROXY" ]; then
        PROXY="--http-proxy $HTTP_PROXY"
    else
        PROXY=""
    fi
}

# Function to install a single Ruby gem
install_gem() {
    local gem=$1
    echo "Installing $gem..."
    $GEM install $PROXY "$gem"
}

# Function to install the necessary Ruby gems
install_gems() {
    echo "Updating gem system to the latest version..."
    $GEM update --system $PROXY

    echo "Installing essential Ruby gems..."
    # Define the list of gems as a multi-line string
    gems="
    rails
    rb-readline
    debugger
    pry
    pry-doc
    pry-debugger
    pry-stack_explorer
    pry-rails
    pry-coolline
    hirb
    hirb-unicode
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
    turn
    rest-client
    http_configuration
    hpricot
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
    coverage
    unicorn
    capistrano
    redgreen
    minitest
    rspec
    rspec-rails
    rspec-mocks
    flexmock
    simplecov
    simplecov-rcov
    sequel
    thrift
    puppet
    json
    rdoc
    gherkin
    cucumber
    capybara
    rubytter
    termtter
    userstream
    webtail
    zipruby
    oauth
    webrat
    diff-lcs
    msgpack
    gettext_rails
    gettext_activerecord
    searchlogic
    fastercsv
    log4r
    foreman
    sinatra
    sinatra_more
    sinatra-formkeeper
    formkeeper-japanese
    redis
    sidekiq
    rubytree
    shoulda
    request-log-analyzer
    apache-loggen
    tzinfo
    guard-coffeescript
    rb-fsevent
    iconv
    i18n
    execjs
    therubyracer
    feedbag
    hashie
    gcalapi
    xml-simple
    pocket-ruby
    weather_hacker
    jpstock
    spreadsheet
    prawn
    pdfkit
    wkhtmltopdf-binary-edge
    gnuplot
    ai4r
    nimbus
    naivebayes
    kmeans
    recommendation
    stdout
    sysadmin
    automatic
    screening
    ctoD
    count_by
    poppler
    rubypython
    narray
    gsl
    rmagick
    graphviz
    wukong
    woothee
    webhdfs
    passenger
    daemons
    eventmachine
    fluentd
    fluent-logger
    fluent-plugin-sqlite3
    fluent-plugin-s3
    fluent-plugin-mongo
    fluent-plugin-webhdfs
    fluent-plugin-parser
    fluent-plugin-rewrite-tag-filter
    fluent-plugin-flowcounter
    fluent-plugin-growthforecast
    fluent-plugin-datacounter
    heroku
    aws-sdk
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

# Main function to coordinate environment setup and gem installation
main() {
    echo "Starting Ruby gem installation..."
    setup_environment "$1"
    install_gems
    echo "All tasks completed successfully."
}

# Start the script with the provided argument (if any)
main "$1"
