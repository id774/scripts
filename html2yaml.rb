require 'nokogiri'
require 'open-uri'
require 'optparse'
require 'yaml'

def pod_from_node(node)
  data = YAML::Omap.new

  data[:name] = node.node_name

  attrs = YAML::Omap.new
  node.attribute_nodes.sort_by(&:node_name).each do |attr|
    attrs[attr.node_name] = attr.value
  end
  data[:attributes] = attrs if attrs.size > 0

  children = node.children.select(&:element?).map {|child|
    pod_from_node(child)
  }
  data[:children] = children if children.size > 0

  data
end

def prune_html(html, rules)
  rules.each do |rule|
    type, selector = *rule
    html.root.search(selector).each do |node|
      if type == :exclude
        node['html2yaml-exclude'] = 'yes'
      else
        node['html2yaml-exclude'] = 'no'
        node.ancestors.each do |ancestor|
          ancestor['html2yaml-exclude'] = 'no'
        end
      end
    end
  end

  html.root.search('[html2yaml-exclude = yes]').remove
  html.root.traverse {|node| node.remove_attribute('html2yaml-exclude') }

  html
end

def main
  rules = []
  opt = OptionParser.new
  opt.on('-i', '--include=XPATH_OR_CSS') {|v| rules << [:include, v] }
  opt.on('-x', '--exclude=XPATH_OR_CSS') {|v| rules << [:exclude, v] }
  opt.parse!(ARGV)

  source = ARGV.shift or abort "Usage: html2yaml.rb URL_OR_PATH"
  html = Nokogiri::HTML(open(source))
  html = prune_html(html, rules)
  data = pod_from_node(html.root)
  puts YAML.dump(data)
end

main
