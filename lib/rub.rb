def rub(path)
  @@rub_loaded_paths ||= {}
  return false if @@rub_loaded_paths[path]
  result = []
  indents = [0]
  ended = false
  open(path).read.split("\n").each do |line|
    if ended
      result << line
    elsif line == "__END__"
      result << line
      ended = true
    else
      indent = line.index(/\S|$/)
      if indent > indents[-1]
        indents << indent
      end
      while indent < indents[-1]
        indents.pop
        unless line =~ /^\s*end\b/
          result << (" " * indents[-1]) + "end"
        end
      end
      result << line
    end
  end
  (indents.size - 1).times{result << "end"}
  eval result.join("\n"), TOPLEVEL_BINDING, $0 = path
  @@rub_loaded_paths[path] = true
end
