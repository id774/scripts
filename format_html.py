#!/usr/bin/env python
# -*- coding: utf-8 -*-

########################################################################
# format_html.py: Format HTML with Normalized Indentation
#
#  Description:
#  Format HTML into a readable and consistently indented structure.
#
#  Behavior:
#  - Indent elements based on HTML nesting structure
#  - Collapse simple text-only elements (e.g. p, td, th) into a single line
#  - Preserve raw content in preformatted elements (pre, script, style, textarea)
#  - Normalize excessive blank lines to a single blank line
#  - Keep inline elements without introducing unnecessary line breaks
#
#  Notes:
#  - This is a lightweight formatter, not a full HTML validator or repair tool
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Requirements:
#  - Python Version: 3.1 or later
#  - Standard library only
#
#  Usage:
#  format_html.py [OPTIONS] INPUT [OUTPUT]
#
#  Options:
#  -h, --help
#      Display usage information and exit.
#  -v, --version
#      Display version information and exit.
#
#  Version History:
#  v1.0 2026-03-26
#       Initial release.
#
########################################################################

from __future__ import print_function

import io
import os
import sys
from html.parser import HTMLParser

VOID_ELEMENTS = set([
    "area", "base", "br", "col", "embed", "hr", "img", "input",
    "link", "meta", "param", "source", "track", "wbr"
])

TEXT_LINE_ELEMENTS = set([
    "p", "td", "th", "li", "figcaption", "caption", "dd", "dt",
    "h1", "h2", "h3", "h4", "h5", "h6"
])

PRESERVE_WHITESPACE_ELEMENTS = set([
    "pre", "script", "style", "textarea"
])

INLINE_ELEMENTS = set([
    "a", "abbr", "b", "bdi", "bdo", "cite", "code", "data", "del", "dfn",
    "em", "i", "img", "ins", "kbd", "mark", "q", "ruby", "rp", "rt",
    "s", "samp", "small", "span", "strong", "sub", "sup", "time", "u",
    "var", "wbr", "br"
])

INDENT = "  "


def log_error(message):
    """Print an error message."""
    print("[ERROR] {0}".format(message), file=sys.stderr)


def usage():
    """Display the script header as usage information and exit."""
    script_path = os.path.abspath(__file__)
    in_header = False
    try:
        with open(script_path, "r", encoding="utf-8") as f:
            for line in f:
                if line.strip().startswith("#" * 10):
                    if not in_header:
                        in_header = True
                        continue
                    break
                if in_header and line.startswith("#"):
                    if line.startswith("# "):
                        print(line[2:], end="")
                    else:
                        print(line[1:], end="")
    except Exception as e:
        print("Error reading usage information: %s" % str(e), file=sys.stderr)
        sys.exit(2)
    sys.exit(0)


def get_script_version():
    """Extract script version from header."""
    script_path = os.path.abspath(__file__)
    found_history = False

    try:
        with open(script_path, 'r', encoding='utf-8') as handle:
            for line in handle:
                if "Version History" in line:
                    found_history = True
                elif found_history and line.strip().startswith("#  v"):
                    return line.strip().split()[1]
    except Exception:
        return "unknown"

    return "unknown"


def show_version():
    """Display version information."""
    version = get_script_version()
    print("format_html.py %s" % version)
    return 0


def escape_attr(value):
    """Escape an HTML attribute value."""
    return value.replace("&", "&amp;").replace('"', "&quot;")


def format_attrs(attrs):
    """Format attributes in stable order."""
    if not attrs:
        return ""

    parts = []
    items = list(attrs)
    items.sort(key=lambda item: item[0])

    for key, value in items:
        if value is None:
            parts.append(" {0}".format(key))
        else:
            parts.append(' {0}="{1}"'.format(key, escape_attr(value)))

    return "".join(parts)


def collapse_whitespace(text):
    """Collapse internal whitespace into single spaces."""
    return " ".join(text.split())


def normalize_blank_lines(text):
    """Collapse excessive blank lines to a single blank line."""
    lines = text.splitlines()
    result = []
    blank_count = 0
    index = 0

    while index < len(lines):
        line = lines[index].rstrip()

        if line == "":
            blank_count += 1
            if blank_count <= 1:
                result.append("")
        else:
            blank_count = 0
            result.append(line)

        index += 1

    while result and result[0] == "":
        result.pop(0)

    while result and result[-1] == "":
        result.pop()

    return "\n".join(result) + "\n"


class Node(object):
    """Store a parsed HTML node."""

    def __init__(self, node_type, name=None, attrs=None, data=None):
        self.node_type = node_type
        self.name = name
        self.attrs = attrs or []
        self.data = data
        self.children = []

    def append(self, child):
        """Append a child node."""
        self.children.append(child)


class HTMLTreeBuilder(HTMLParser):
    """Build a minimal HTML tree."""

    def __init__(self):
        HTMLParser.__init__(self, convert_charrefs=False)
        self.root = Node("document")
        self.stack = [self.root]

    def current(self):
        """Return the current node."""
        return self.stack[-1]

    def handle_starttag(self, tag, attrs):
        """Handle a start tag."""
        node = Node("element", name=tag, attrs=attrs)
        self.current().append(node)

        if tag not in VOID_ELEMENTS:
            self.stack.append(node)

    def handle_startendtag(self, tag, attrs):
        """Handle a self-closing tag."""
        node = Node("element", name=tag, attrs=attrs)
        self.current().append(node)

    def handle_endtag(self, tag):
        """Handle an end tag."""
        index = len(self.stack) - 1
        while index > 0:
            if self.stack[index].node_type == "element" and self.stack[index].name == tag:
                del self.stack[index:]
                break
            index -= 1

    def handle_data(self, data):
        """Handle text data."""
        self.current().append(Node("text", data=data))

    def handle_comment(self, data):
        """Handle a comment."""
        self.current().append(Node("comment", data=data))

    def handle_decl(self, decl):
        """Handle a declaration."""
        self.current().append(Node("decl", data=decl))

    def handle_pi(self, data):
        """Handle a processing instruction."""
        self.current().append(Node("pi", data=data))

    def unknown_decl(self, data):
        """Handle an unknown declaration."""
        self.current().append(Node("decl", data=data))


def has_only_inline_content(node):
    """Check whether an element contains only inline content."""
    index = 0

    while index < len(node.children):
        child = node.children[index]

        if child.node_type == "text":
            index += 1
            continue

        if child.node_type != "element":
            return False

        if child.name not in INLINE_ELEMENTS:
            return False

        index += 1

    return True


def collect_inline_content(node):
    """Collect inline HTML content without formatter whitespace."""
    parts = []
    index = 0

    while index < len(node.children):
        child = node.children[index]
        parts.append(render_node(child, 0, True))
        index += 1

    return "".join(parts).strip()


def collect_collapsed_inline_content(node):
    """Collect inline HTML content and collapse plain text whitespace."""
    parts = []
    pending_text = []
    index = 0

    while index < len(node.children):
        child = node.children[index]

        if child.node_type == "text":
            pending_text.append(child.data)
            index += 1
            continue

        if pending_text:
            collapsed = collapse_whitespace("".join(pending_text))
            if collapsed != "":
                parts.append(collapsed)
            pending_text = []

        parts.append(render_node(child, 0, True))
        index += 1

    if pending_text:
        collapsed = collapse_whitespace("".join(pending_text))
        if collapsed != "":
            parts.append(collapsed)

    return "".join(parts).strip()


def render_text(node, level, inline_mode):
    """Render a text node."""
    if inline_mode:
        return node.data

    text = node.data.strip()
    if text == "":
        return ""

    return "{0}{1}".format(INDENT * level, text)


def render_comment(node, level, inline_mode):
    """Render a comment node."""
    if inline_mode:
        return ""

    return "{0}<!--{1}-->".format(INDENT * level, node.data)


def render_decl(node, level, inline_mode):
    """Render a declaration node."""
    if inline_mode:
        return ""

    return "{0}<!{1}>".format(INDENT * level, node.data)


def render_pi(node, level, inline_mode):
    """Render a processing instruction node."""
    if inline_mode:
        return ""

    return "{0}<?{1}>".format(INDENT * level, node.data)


def render_preserved_element(node, level):
    """Render an element with preserved internal whitespace."""
    attrs = format_attrs(node.attrs)
    start = "{0}<{1}{2}>".format(INDENT * level, node.name, attrs)
    end = "{0}</{1}>".format(INDENT * level, node.name)

    raw_parts = []
    index = 0

    while index < len(node.children):
        child = node.children[index]
        raw_parts.append(render_node(child, 0, True))
        index += 1

    inner = "".join(raw_parts)

    if inner == "":
        return "{0}<{1}{2}></{1}>".format(INDENT * level, node.name, attrs)

    return "\n".join([start, inner, end])


def render_element(node, level, inline_mode):
    """Render an element node."""
    attrs = format_attrs(node.attrs)

    if node.name in VOID_ELEMENTS:
        if inline_mode:
            return "<{0}{1}>".format(node.name, attrs)
        return "{0}<{1}{2}>".format(INDENT * level, node.name, attrs)

    if inline_mode:
        inner = collect_inline_content(node)
        return "<{0}{1}>{2}</{0}>".format(node.name, attrs, inner)

    if node.name in PRESERVE_WHITESPACE_ELEMENTS:
        return render_preserved_element(node, level)

    if node.name == "td":
        inner = collect_collapsed_inline_content(node)
        return "{0}<{1}{2}>{3}</{1}>".format(INDENT * level, node.name, attrs, inner)

    if node.name == "th":
        inner = collect_collapsed_inline_content(node)
        return "{0}<{1}{2}>{3}</{1}>".format(INDENT * level, node.name, attrs, inner)

    if node.name == "p" and has_only_inline_content(node):
        inner = collect_collapsed_inline_content(node)
        return "{0}<{1}{2}>{3}</{1}>".format(INDENT * level, node.name, attrs, inner)

    if node.name in TEXT_LINE_ELEMENTS and has_only_inline_content(node):
        inner = collect_inline_content(node)
        return "{0}<{1}{2}>{3}</{1}>".format(INDENT * level, node.name, attrs, inner)

    lines = []
    lines.append("{0}<{1}{2}>".format(INDENT * level, node.name, attrs))

    index = 0
    while index < len(node.children):
        child = node.children[index]
        rendered = render_node(child, level + 1, False)
        if rendered != "":
            lines.append(rendered)
        index += 1

    lines.append("{0}</{1}>".format(INDENT * level, node.name))
    return "\n".join(lines)


def render_node(node, level, inline_mode):
    """Render any node."""
    if node.node_type == "text":
        return render_text(node, level, inline_mode)

    if node.node_type == "comment":
        return render_comment(node, level, inline_mode)

    if node.node_type == "decl":
        return render_decl(node, level, inline_mode)

    if node.node_type == "pi":
        return render_pi(node, level, inline_mode)

    if node.node_type == "element":
        return render_element(node, level, inline_mode)

    if node.node_type == "document":
        lines = []
        index = 0
        while index < len(node.children):
            child = node.children[index]
            rendered = render_node(child, level, False)
            if rendered != "":
                lines.append(rendered)
            index += 1
        return "\n\n".join(lines)

    return ""


def format_html(text):
    """Format HTML text."""
    parser = HTMLTreeBuilder()
    parser.feed(text)
    parser.close()

    formatted = render_node(parser.root, 0, False)
    return normalize_blank_lines(formatted)


def read_text(path):
    """Read a UTF-8 text file."""
    with io.open(path, "r", encoding="utf-8") as handle:
        return handle.read()


def write_text(path, text):
    """Write a UTF-8 text file."""
    with io.open(path, "w", encoding="utf-8") as handle:
        handle.write(text)


def validate_input_file(path):
    """Validate the input file."""
    if not os.path.exists(path):
        log_error("Input file not found: {0}".format(path))
        return False

    if not os.path.isfile(path):
        log_error("Input path is not a file: {0}".format(path))
        return False

    if not os.access(path, os.R_OK):
        log_error("Input file is not readable: {0}".format(path))
        return False

    return True


def validate_output_file(path):
    """Validate the output target."""
    directory = os.path.dirname(path)

    if directory == "":
        directory = "."

    if not os.path.exists(directory):
        log_error("Output directory not found: {0}".format(directory))
        return False

    if not os.path.isdir(directory):
        log_error("Output directory is not a directory: {0}".format(directory))
        return False

    if not os.access(directory, os.W_OK):
        log_error("Output directory is not writable: {0}".format(directory))
        return False

    return True


def parse_args(argv):
    """Parse command-line arguments."""
    positional = []
    index = 1

    while index < len(argv):
        arg = argv[index]

        if arg == "-h" or arg == "--help":
            return {"mode": "help"}

        if arg == "-v" or arg == "--version":
            return {"mode": "version"}

        if arg.startswith("-"):
            log_error("Unsupported option: {0}".format(arg))
            return None

        positional.append(arg)
        index += 1

    if len(positional) < 1 or len(positional) > 2:
        log_error("Invalid number of arguments")
        return None

    result = {
        "mode": "run",
        "input": positional[0],
        "output": None
    }

    if len(positional) == 2:
        result["output"] = positional[1]

    return result


def main():
    """Run the formatter."""
    args = parse_args(sys.argv)
    if args is None:
        print("Usage: {0} [OPTIONS] INPUT [OUTPUT]".format(os.path.basename(sys.argv[0])), file=sys.stderr)
        return 1

    if args["mode"] == "help":
        return usage()

    if args["mode"] == "version":
        return show_version()

    input_path = args["input"]
    output_path = args["output"]

    if not validate_input_file(input_path):
        return 1

    if output_path is not None and not validate_output_file(output_path):
        return 1

    try:
        source = read_text(input_path)
        formatted = format_html(source)
    except Exception as exc:
        log_error("Failed to process input file {0}: {1}".format(input_path, exc))
        return 1

    if output_path is None:
        print(formatted, end="")
        return 0

    try:
        write_text(output_path, formatted)
    except Exception as exc:
        log_error("Failed to write output file {0}: {1}".format(output_path, exc))
        return 1

    return 0


if __name__ == "__main__":
    sys.exit(main())
