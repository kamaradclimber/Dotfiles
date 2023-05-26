#!/usr/bin/env ruby

require 'net/http'
require 'uri'
require 'json'
require 'highline'

def get_tokens(login, password)
  uri = URI.parse('https://api.gnothiai.com/auth/login')
  request = Net::HTTP::Post.new(uri)
  request.body = URI.encode_www_form(username: login, password: password)

  req_options = {
    use_ssl: uri.scheme == 'https'
  }

  response = Net::HTTP.start(uri.hostname, uri.port, req_options) do |http|
    http.request(request)
  end

  raise "Impossible to get a token. Code was #{response.code}. Body #{response.body}" unless response.code.to_i == 200

  JSON.parse(response.body)
end

def get_fields(tokens)
  uri = URI.parse('https://api.gnothiai.com/fields')
  request = Net::HTTP::Get.new(uri)

  req_options = {
    use_ssl: uri.scheme == 'https'
  }
  request['Authorization'] = "Bearer #{tokens['access_token']}"

  response = Net::HTTP.start(uri.hostname, uri.port, req_options) do |http|
    http.request(request)
  end

  raise "Impossible to list fields from gnothi. Code was #{response.code}" unless response.code.to_i == 200

  JSON.parse(response.body)
end

def ask_value(service)
  cli = HighLine.new
  case service['type']
  when 'fivestar'
    cli.ask("How would you rate your #{service['name']} ? (1-5) ", Integer) { |q| q.in = 1..5 }
  when 'number'
    cli.ask("What is your #{service['name']} ? ", Float)
  when 'check'
    cli.agree("Is '#{service['name']}' true? (y/n)") ? 1 : 0
  else
    raise "Unknown field type #{service['type']}, need to be implemented"
  end
end

def send_value(service, value, tokens)
  uri = URI.parse("https://api.gnothiai.com/field-entries/#{service['id']}")
  request = Net::HTTP::Post.new(uri)
  request['Content-Type'] = "application/json"
  request['Authorization'] = "Bearer #{tokens['access_token']}"
  request.body = {value: value}.to_json

  req_options = {
    use_ssl: uri.scheme == 'https'
  }

  response = Net::HTTP.start(uri.hostname, uri.port, req_options) do |http|
    http.request(request)
  end

  raise "Impossible to send value for #{service['name']}. Code was #{response.code}. Body #{response.body}" unless response.code.to_i == 200

  JSON.parse(response.body)
end

raise 'You must set GNOTHIAI_USERNAME' unless ENV.key?('GNOTHIAI_USERNAME')
raise 'You must set GNOTHIAI_PASSWORD' unless ENV.key?('GNOTHIAI_PASSWORD')

tokens = get_tokens(ENV['GNOTHIAI_USERNAME'], ENV['GNOTHIAI_PASSWORD'])
raise 'Failed to get access_token from gnothi' unless tokens.key?('access_token')

fields = get_fields(tokens)

puts "Found #{fields.size} fields for the following services:"
fields.values.group_by { |f| f['service'] }.each do |service_name, service_fields|
  puts "- #{service_name || '<none>'}: #{service_fields.size} fields (#{service_fields.sample(3).map { |s| s['name'] }})"
end

puts ''
puts "Will now ask to fill all those fields"
puts ''

fields.values.select { |f| f['service'].nil? }.each do |service|
  value = ask_value(service)
  send_value(service, value, tokens)
end
