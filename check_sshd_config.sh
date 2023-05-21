#!/bin/bash

os=$(uname)

if [ "$os" = "Darwin" ]; then
  test -f /etc/ssh/sshd_config.d/000-sshdconfig.conf || sudo cp -v $SCRIPTS/etc/sshd_config.d/000-sshdconfig.conf /etc/ssh/sshd_config.d/000-sshdconfig.conf
  cat /etc/ssh/sshd_config.d/000-sshdconfig.conf
else
  grep Port /etc/ssh/sshd_config | grep -v "#"
  grep PermitRootLogin /etc/ssh/sshd_config | grep -v "#"
  grep PasswordAuthentication /etc/ssh/sshd_config | grep -v "#"
  grep ChallengeResponseAuthentication /etc/ssh/sshd_config | grep -v "#"
fi

exit 0
