#!/bin/sh

test -n "$1" || target_device='sda'
test -n "$1" && target_device="$1"
test -n "$2" || mail_to='sysadmin@id774.net'
test -n "$2" && mail_to="$2"
test -n "$3" || mailqueue_file=''
test -n "$3" && mailqueue_file="$3"

target_disk="/dev/${target_device}"
smartctl_output="${HOME}/var/smartctl_${target_device}_output.txt"
monitor_current="${HOME}/var/smartctl_monitor_${target_device}_new.txt"
monitor_previous="${HOME}/var/smartctl_monitor_${target_device}_old.txt"
monitor_difference="${HOME}/var/smartctl_${target_device}_diff.txt"

if  [ -e ${monitor_previous} ]
then
    rm ${monitor_previous}
fi

if  [ -e ${monitor_current} ]
then
    mv ${monitor_current} ${monitor_previous}
fi

/usr/sbin/smartctl -x ${target_disk} > ${smartctl_output}

cat ${smartctl_output} | grep Raw_Read_Error_Rate     >  ${monitor_current}
cat ${smartctl_output} | grep Reallocated_Sector_Ct   >> ${monitor_current}
cat ${smartctl_output} | grep Reallocated_Event_Count >> ${monitor_current}
cat ${smartctl_output} | grep Current_Pending_Sector  >> ${monitor_current}
cat ${smartctl_output} | grep Offline_Uncorrectable   >> ${monitor_current}

rm ${smartctl_output}

if  [ -e ${monitor_previous} ]
then
    diff ${monitor_current} ${monitor_previous} > ${monitor_difference}
fi

if [ -s ${monitor_difference} ]
then
    mail_title="[admin][alert] SMART indicator(s) varied : ${target_disk} at `hostname`"

    if [ -z ${mailqueue_file} ]
    then
        cat ${monitor_current} | mail -s "${mail_title}" ${mail_to}
    else
        echo "From: smart-value-checker@`hostname`" > ${mailqueue_file}
        echo "To: ${mail_to}" >> ${mailqueue_file}
        echo "Subject: ${mail_title}" >> ${mailqueue_file}
        printf "\r\n" >> ${mailqueue_file}
        cat ${monitor_current} >> ${mailqueue_file}
    fi
fi

if [ -e ${monitor_difference} ]
then
    rm ${monitor_difference}
fi
