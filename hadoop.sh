#!/bin/sh

test -n "$1" || exit 1
export HADOOP_VER=0.20
export JAVA_HOME=/opt/java/jdk
sudo /etc/init.d/hadoop-$HADOOP_VER-namenode $1
sudo /etc/init.d/hadoop-$HADOOP_VER-jobtracker $1
sudo /etc/init.d/hadoop-$HADOOP_VER-datanode $1
sudo /etc/init.d/hadoop-$HADOOP_VER-tasktracker $1

