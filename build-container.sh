#!/usr/bin/bash

set -e

# This file expects:
# - a Dockerfile, even if minimal (with only FROM xxxx)
# - optionnaly a bindpaths file containing directory to bind mount in the container

# to call it ./build-container.sh <a directory containing files>

function log() {
  local message=$1
  echo -e "\e[32m$message\e[0m"
}

log "Switching to $1"
cd $1

function clear_old_unit() {
  local service_name=$1
  if systemctl is-active $service_name || systemctl is-failed $service_name; then
    log "Stopping existing service"
    sudo systemctl stop $service_name
    sudo systemctl daemon-reload
    sleep 1
  fi
  existing_unit_file=$(sudo systemctl cat $service_name | head -n1 | grep "^#" | awk '{print $2}')
  if [[ ! -z "$existing_unit_file" ]]; then
    if ! (echo $existing_unit_file | grep -q Unit); then
      log "Backup of previous unit file: $existing_unit_file"
      sudo mv $existing_unit_file $service_name.service.backup.$(date +%s)
      sudo systemctl daemon-reload
      sleep 1
    fi
  fi
}

if [[ ! -f Dockerfile ]]; then
  log "ERROR: No dockerfile in this directory"
  exit 1
fi

image_name=$(cat Dockerfile | grep ^FROM | tail -n1 | awk '{print $2}')
service_name=$(echo $image_name | cut -d: -f1 | cut -d/ -f2)
log "Will build upon $image_name, service name will be $service_name"

tag_name="$service_name:$(date +%s)"

log "Image built and tagged with $tag_name"
sudo podman build . --tag $tag_name

image_tarball_name=$service_name
sudo rm -f $image_tarball_name.tar
log "Copy image in tar format under the name $image_tarball_name.tar"
sudo skopeo copy containers-storage:localhost/$tag_name docker-archive:$image_tarball_name.tar

log "Extract image in rootfs directory"
(sudo rm rootfs -rf && mkdir -p rootfs && cd rootfs && undocker ../$image_tarball_name.tar - | tar -xv | sponge | head)

sudo rm $image_tarball_name.tar

clear_old_unit $service_name

log "detect env variable from built image"
environment_file=$(pwd)/.${service_name}.env.generated
sudo skopeo inspect containers-storage:localhost/$tag_name | jq .Env[] -r > $environment_file
# cat $environment_file

log "detect entrypoint from original image"
entrypoint=$(sudo podman inspect localhost/$tag_name | jq .[0].Config.Entrypoint[0] -r)
working_directory=$(sudo podman inspect localhost/$tag_name | jq .[0].Config.WorkingDir -r)
log "detected entrypoint is $entrypoint running from $working_directory"


sudo tee /etc/systemd/system/$service_name.service > /dev/null << UNIT
[Unit]
Description=${service_name}
Wants=network.target
After=network-online.target

[Service]
PrivateTmp=yes
ProtectProc=invisible
RootDirectory=$(pwd)/rootfs
WorkingDirectory=$working_directory
EnvironmentFile=
EnvironmentFile=$environment_file
BindReadOnlyPaths=/dev/log:/dev/log:rbind
BindReadOnlyPaths=/run/systemd/journal/socket:/run/systemd/journal/socket:rbind
BindReadOnlyPaths=/run/systemd/journal/stdout:/run/systemd/journal/stdout:rbind
Type=exec
ExecStart=
ExecStart="$entrypoint"

[Install]
WantedBy=multi-user.target default.target
UNIT

unitd_dir=/etc/systemd/system/$service_name.service.d
sudo mkdir -p $unitd_dir

if [[ -f bindpaths ]]; then
  log "Building drop-in file for bind mounts"
  cat bindpaths | while read line; do
  if echo "$line" | grep -q "^#"; then
    continue
  fi
  sudo tee $unitd_dir/data.conf > /dev/null << UNIT
[Service]
BindPaths=$line
UNIT
  done
else
  log "no bindpaths file found"
fi


log "reloading systemd config"
sudo systemctl daemon-reload
log "starting $service_name service"
sudo systemctl start --no-block $service_name
log "Persistent $service_name to be started at next boot"
sudo systemctl enable $service_name.service
