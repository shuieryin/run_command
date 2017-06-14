#!/bin/bash

#SERVICE_PATH=/etc/systemd/system/run_command.service
START_PATH=/tmp/run_command
PWD=$(pwd)
CUR_USER=$(whoami)

mkdir -p ${START_PATH}

rm -f ${START_PATH}/envs.sh
echo "#!/bin/bash" | tee ${START_PATH}/envs.sh
printenv | sed 's/^\(.*\)$/export \1/g' | sed '/LS_COLORS/d' >> ${START_PATH}/envs.sh
chmod 744 ${START_PATH}/envs.sh

rm -f ${START_PATH}/start.sh
echo "#!/bin/bash" | tee ${START_PATH}/start.sh
echo "${START_PATH}/envs.sh" | tee -a ${START_PATH}/start.sh
echo "echo $HOME >> ${START_PATH}/log" | tee -a ${START_PATH}/start.sh
echo "cd ${PWD}" | tee -a ${START_PATH}/start.sh
echo "${PWD}/_build/default/rel/run_command/bin/run_command start" | tee -a ${START_PATH}/start.sh
chmod 744 ${START_PATH}/start.sh
#chown ${CUR_USER}:${CUR_USER} ${START_PATH}/start.sh

echo "@reboot ${START_PATH}/start.sh" | tee -a /var/spool/cron/crontabs/${CUR_USER}
crontab /var/spool/cron/crontabs/${CUR_USER}

#rm -f ${START_PATH}/stop.sh
#echo "#!/bin/bash" | tee ${START_PATH}/stop.sh
#echo "pwd" | tee -a ${START_PATH}/stop.sh
#echo "${START_PATH}/envs.sh" | tee -a ${START_PATH}/stop.sh
#echo "echo $HOME >> ${START_PATH}/log" | tee -a ${START_PATH}/stop.sh
#echo "cd ${PWD}" | tee -a ${START_PATH}/stop.sh
#echo "${PWD}/_build/default/rel/run_command/bin/run_command stop" | tee -a ${START_PATH}/stop.sh
#chmod 744 ${START_PATH}/stop.sh
#chown ${CUR_USER}:${CUR_USER} ${START_PATH}/stop.sh

#rm -f ${SERVICE_PATH}
#echo "[Unit]" | tee ${SERVICE_PATH}
#echo "After=docker.service" | tee -a ${SERVICE_PATH}
#echo "[Service]" | tee -a ${SERVICE_PATH}
#echo "ExecStart=${START_PATH}/start.sh" | tee -a ${SERVICE_PATH}
#echo "ExecStop=${START_PATH}/stop.sh" | tee -a ${SERVICE_PATH}
#echo "[Install]" | tee -a ${SERVICE_PATH}
#echo "WantedBy=default.target" | tee -a ${SERVICE_PATH}

#chmod 644 ${SERVICE_PATH}
#systemctl daemon-reload
#systemctl enable run_command.service
