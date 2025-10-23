# will try ping until success. useful to wait for network to come back
function ping_until {
  until ping -c 3 -W 1 -q $1 > /dev/null 2>&1; do echo -n .; sleep 0.4; done
}

function nc_until {
  until (echo "" | nc -w 1 $1 $2) > /dev/null ; do echo -n .; sleep 0.4; done
}

function ssh_until {
  echo -n ping
  ping_until $1
  echo ""
  echo -n "ssh_port"
  nc_until $1 22
  echo ""
}

function gotmp() {
  dir=$(mktemp -d)
  cd $dir
}

function my_private_ipaddress() {
  ip addr | grep 'inet ' | awk '{print $2}' | cut -f1 -d'/' | grep -e 192.168 -e ^172 | sort | tail -n1
}

function webserver() {
  my_ip=$(my_private_ipaddress)
  pkill python3
  python3 -m http.server 8000 > /dev/null &
  echo "server accessible on http://$my_ip:8000/"
  fg > /dev/null 2>&1
}

function scrot() {
  echo "Maybe use 'ksnip' instead, it allows to annotate easily"
  /usr/bin/scrot $@
}

function vlc() {
  echo "Using a function wrapping vlc, see .bashrc file"
  if echo "$@" | grep -q .part$; then
    echo "🚧Trying to read an incomplete file (ending in part), this will likely cause issues"
    echo "Sleeping 10s to make sure this warning has been read"
    sleep 10
  fi
  command vlc $@
}

# yaml_flatten takes any input and transform it into a list of key/value pairs where keys is the full path to reach the value
function yaml_flatten() {
  cat $@ | yq eval '.. | select((tag == "!!map" or tag == "!!seq") | not) | (path | join(".")) + "=" + .'
}
