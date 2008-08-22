class MessageLogger
  def MessageLogger.msg(messages)
    puts Time.now.strftime("%Y/%m/%d-%X " + messages)
  end
end
