#!/usr/bin/env ruby

########################################################################
# html2yaml.rb: HTML to YAML Converter Script
#
# Description:
# This script parses an HTML document and converts it into a structured
# YAML format. It uses Nokogiri for HTML parsing and provides options for
# including or excluding specific elements based on CSS or XPath selectors.
# This is useful for converting HTML structures into a format that can be
# easily processed by scripts or programs.
#
# Author: id774 (More info: http://id774.net)
# Source Code: https://github.com/id774/scripts
# License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
# Contact: idnanashi@gmail.com
#
# Usage:
# Run the script with the URL or path of the HTML file, along with optional
# arguments for inclusion or exclusion of elements:
#   html2yaml.rb [options] URL_OR_PATH
#
# Options:
#   -i, --include=XPATH_OR_CSS  Include elements matching the given selector
#   -x, --exclude=XPATH_OR_CSS  Exclude elements matching the given selector
#
# Example:
#   html2yaml.rb --exclude='//div[@class="footer"]' http://example.com
#   This command will parse the HTML from example.com, excluding divs with class "footer".
#
# Input/Output Example:
# - Given an HTML document like:
#   <!DOCTYPE html>
#   <html>
#   <head>
#       <title>Example Page</title>
#   </head>
#   <body>
#       <div id="main" class="container">
#           <h1>Welcome to the Example Page</h1>
#           <p>This is a sample paragraph.</p>
#       </div>
#   </body>
#   </html>
# - The output will be a YAML format representing the structure of the HTML:
#   :name: html
#   :children:
#     - :name: head
#       :children:
#         - :name: title
#           :children:
#             - :name: text
#               :value: Example Page
#     - :name: body
#       :children:
#         - :name: div
#           :attributes:
#             :id: main
#             :class: container
#           :children:
#             - :name: h1
#               :children:
#                 - :name: text
#                   :value: Welcome to the Example Page
#             - :name: p
#               :children:
#                 - :name: text
#                   :value: This is a sample paragraph.
#
########################################################################

require 'nokogiri'
require 'open-uri'
require 'optparse'
require 'yaml'

# Converts an HTML node to a YAML-structured data object
def pod_from_node(node)
  data = YAML::Omap.new

  # Extract node name
  data[:name] = node.node_name

  # Extract and sort attributes of the node
  attrs = YAML::Omap.new
  node.attribute_nodes.sort_by(&:node_name).each do |attr|
    attrs[attr.node_name] = attr.value
  end
  data[:attributes] = attrs if attrs.size > 0

  # Recursively process children nodes
  children = node.children.select(&:element?).map {|child|
    pod_from_node(child)
  }
  data[:children] = children if children.size > 0

  data
end

# Prunes HTML based on specified rules for inclusion or exclusion
def prune_html(html, rules)
  rules.each do |rule|
    type, selector = *rule
    html.root.search(selector).each do |node|
      # Mark nodes for exclusion or inclusion
      if type == :exclude
        node['html2yaml-exclude'] = 'yes'
      else
        node['html2yaml-exclude'] = 'no'
        node.ancestors.each { |ancestor| ancestor['html2yaml-exclude'] = 'no' }
      end
    end
  end

  # Remove excluded nodes and clean up attributes
  html.root.search('[html2yaml-exclude = yes]').remove
  html.root.traverse { |node| node.remove_attribute('html2yaml-exclude') }

  html
end

def main
  rules = []
  opt = OptionParser.new
  # Define command-line options for inclusion and exclusion
  opt.on('-i', '--include=XPATH_OR_CSS') { |v| rules << [:include, v] }
  opt.on('-x', '--exclude=XPATH_OR_CSS') { |v| rules << [:exclude, v] }
  opt.parse!(ARGV)

  # Load the HTML source
  source = ARGV.shift or abort "Usage: html2yaml.rb URL_OR_PATH"
  html = Nokogiri::HTML(open(source))
  html = prune_html(html, rules)

  # Convert HTML to YAML and output
  data = pod_from_node(html.root)
  puts YAML.dump(data)
end

main

