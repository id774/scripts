class GmailSender
  require 'mechanize'
  require 'kconv'
  require 'fileutils'
  require 'uri'

  def send(passwordfile, mailfile)

    sender = open(passwordfile).read.to_a
    username = sender.shift.chomp
    password = sender.shift.chomp

    body = open(mailfile).read.to_a
    to = body.shift.chomp
    cc = body.shift.chomp
    bcc = body.shift.chomp
    attach0 = body.shift.chomp
    subject = body.shift.chomp
    body = body.join

    to = ENV['GSEND_TO'] unless ENV['GSEND_TO'].nil?
    cc = ENV['GSEND_CC'] unless ENV['GSEND_CC'].nil?
    bcc = ENV['GSEND_BCC'] unless ENV['GSEND_BCC'].nil?
    attach0 = ENV['GSEND_ATTACH'] unless ENV['GSEND_ATTACH'].nil?
    subject = ENV['GSEND_SUBJECT'] unless ENV['GSEND_SUBJECT'].nil?

    agent = WWW::Mechanize.new

    if ENV['HTTP_PROXY']
      proxy = URI.parse(ENV['HTTP_PROXY'])
      agent.set_proxy(proxy.host, proxy.port, user = nil, pass = nil)
    end

    agent.user_agent_alias = 'Windows IE 6'

    gmail_page = agent.get("https://mail.google.com/mail/h/")

    login_form = gmail_page.forms.first
    login_form['Email'] = username
    login_form['Passwd'] = password

    agent.submit(login_form)

    agent.get("https://mail.google.com/mail/h/")

    gmail_page = agent.get("?v=b&pv=tl&cs=b")

    mail_form = gmail_page.form("f")

    mail_form['to'] = to
    mail_form['cc'] = cc
    mail_form['bcc'] = bcc
    mail_form['subject'] = subject.toutf8
    mail_form['body'] = body.toutf8

    unless attach0.nil? && attach0.empty?
      if File.exist?(attach0) or File.directory?(attach0)
        mail_form.file_uploads.name("file0").first.file_name = attach0
      end
    end

    send_button = mail_form.buttons.name("nvp_bu_send")

    agent.submit(mail_form, send_button)
  end
end
