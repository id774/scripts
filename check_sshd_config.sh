#!/bin/bash

grep Port /etc/ssh/sshd_config | grep -v "#"
grep PermitRootLogin /etc/ssh/sshd_config | grep -v "#"
grep PasswordAuthentication /etc/ssh/sshd_config | grep -v "#"
grep ChallengeResponseAuthentication /etc/ssh/sshd_config | grep -v "#"

