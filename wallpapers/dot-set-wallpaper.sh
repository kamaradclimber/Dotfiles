#!/usr/bin/bash

source=https://www.reddit.com/r/EarthPorn+ImaginaryLandscapes+ImaginaryTechnology+futureporn+spaceporn.json

curl -s -L -H 'User-Agent: custom-timer' "$source" | jq '.data.children' > children.json


cat <<EOR > tmp.script
#!/usr/bin/env ruby

require 'json'
require 'pp'

def extract_source(post)
  return unless post['data']['preview']

  source = post['data']['preview']['images'].find { |image| image['source'] }
  source['source']
end

def download(url)
  dir = File.join(ENV['HOME'], 'wallpapers')
  target_file = File.join(dir, File.basename(url))
  \`curl '#{url}' -f -o '#{target_file}'\` unless File.exist?(target_file)
end

JSON
  .parse(File.read('children.json'))
  .select { |d| d['data']['preview'] }
  .select { |post| extract_source(post) && extract_source(post)['width'] > 1920 }
  .each { |post| download(post['data']['url']) }
  .each { |post| puts post['data']['url'] }
EOR

ruby tmp.script| head -100 | sort -R | xargs feh --bg-fill
