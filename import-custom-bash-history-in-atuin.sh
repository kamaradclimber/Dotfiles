#!/usr/bin/bash

# based on https://github.com/atuinsh/atuin/issues/2431

# `atuin info` gives the path to the history database

# sqlite> .schema history
# CREATE TABLE history (
# 	id text primary key,
# 	timestamp integer not null,
# 	duration integer not null,
# 	exit integer not null,
# 	command text not null,
# 	cwd text not null,
# 	session text not null,
# 	hostname text not null, deleted_at integer,
# 
# 	unique(timestamp, cwd, command)
# );
# CREATE INDEX idx_history_timestamp on history(timestamp);
# CREATE INDEX idx_history_command on history(command);
# CREATE INDEX idx_history_command_timestamp on history(
# 	command,
# 	timestamp
# );
#
#
# This script will build a csv file with my custom history and import it
#
# my custom history format
# {"date":"2025-01-21.17:51:40","exit_code":"0","command":" k9s --context pignite-a.us1.prod.dog -n logs-storage"}

db_path=$(atuin info | grep "client db path" | cut -d '"' -f2)

cat <<EOF > /tmp/parse_history.rb
require "json"
require "time"
hostname = "`hostname`".strip
user="`whoami`".strip
random = Random.new
all_items = JSON.parse(File.read(ARGV.first))
all_items.each do |j|
  t = Time.strptime(j["date"], "%Y-%m-%d.%H:%M:%S")
  # introduce a bit of random to avoid collisions and preserve exact duplicates
  t_in_nanos = t.to_i * 1000_000 + random.rand(1000)
  id = j["command"].hash + t_in_nanos
  # exclude things that clearly look like corrupted history
  next if j["command"].strip =~ %r{^/ \d+ }
  next if j["command"].strip =~ %r{^/.+ \d{5} }
  command = j["command"].strip.gsub(/'/, "''")
  puts <<~SQL
    INSERT INTO history VALUES(
      "#{id.to_s}",
      #{t_in_nanos},
      0,
      #{j["exit_code"].to_i},
      '#{command}',
      "/tmp",
      "unknown",
      "#{hostname}:#{user}",
      NULL
    );
  SQL
end
EOF

IMPORT_FILE=/tmp/custom_history.import.sql
rm $IMPORT_FILE
# echo "id,timestamp,duration,exit,command,cwd,session,hostname" > $IMPORT_FILE


SLURP_FILE=/tmp/slurp.json
for f in ~/.bash_history_storage/*; do
  # check if extension is .json or .log
  if [[ $f == *.json ]]; then
    echo "Processing $f"
    cat $f | jq --slurp . > $SLURP_FILE
    ruby /tmp/parse_history.rb $SLURP_FILE >> $IMPORT_FILE
  else
    # if the same file exists with .json extension instead of .log, skip it
    extensionless=${f%.*}
    if test -f "$extensionless.json"; then
      echo "Skipping $f"
    else
      echo "Convert $f to json"
      TMP_JSON_FILE=$extensionless.json
      cat $f | while read line; do
        d=$(echo $line | cut -d ' ' -f1)
        c=$(echo $line | cut -d ' ' -f2-)
        jq -n --arg d "$d" --arg c "$c" '{
          date: $d,
          command: $c
        }' >> $TMP_JSON_FILE
      done
      echo "Processing $TMP_JSON_FILE"
      cat $TMP_JSON_FILE | jq --slurp . > $SLURP_FILE
      ruby /tmp/parse_history.rb $SLURP_FILE >> $IMPORT_FILE
    fi
  fi
done

echo "Now type the following commands to import the history:"
echo "sqlite3 $db_path \".read $IMPORT_FILE\" 2>&1 | less"
echo
echo "Then exit the sqlite shell and type the following commands:"
echo "atuin history init-store"
echo "atuin store rebuild history"

