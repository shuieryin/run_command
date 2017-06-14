#!/bin/bash

SERVICE_PATH=/etc/systemd/system/run_command.service

rm -f ${SERVICE_PATH}
echo "[Unit]" | tee ${SERVICE_PATH}
echo "After=docker.service" | tee -a ${SERVICE_PATH}
echo "[Service]" | tee -a ${SERVICE_PATH}
echo "ExecStart='$(pwd)/_build/default/rel/run_command/bin/run_command start'" | tee -a ${SERVICE_PATH}
echo "[Install]" | tee -a ${SERVICE_PATH}
echo "WantedBy=default.target" | tee -a ${SERVICE_PATH}

chmod 644 ${SERVICE_PATH}
systemctl daemon-reload
systemctl enable run_command.service
