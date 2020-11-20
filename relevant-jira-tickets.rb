#!/usr/bin/env ruby

require 'jira-ruby'

raise 'Must set CRITEO_USER env var' unless ENV['CRITEO_USER']
raise 'Most set CRITEO_PASSWORD env var' unless ENV['CRITEO_PASSWORD']

options = {
  username:     ENV['CRITEO_USER'],
  password:     ENV['CRITEO_PASSWORD'],
  site:         'http://jira.criteois.com:443/',
  context_path: '',
  auth_type:    :basic
}

client = JIRA::Client.new(options)

issues = client.Issue.jql('(project in (MESOS, LAKE) OR assignee = currentUser()) AND resolution is EMPTY AND project not in (MRM)', max_results: 500)

def status_value(status)
  case status
  when 'In Progress'
    10
  when 'Wait for deployment'
    7
  when 'Open', 'Reopened'
    5
  when 'Closed'
    0
  else
    warn "Unknown status #{status}, setting status value -1"
    -1
  end
end

def reject(issue)
  return true if issue.status.name == 'Resolved'

  false
end

def compare_key(issue)
  status = status_value(issue.status.name)
  [issue.assignee&.name || 'zzzz', status, issue.updated, issue.created]
end

output = issues
  .reject { |issue| reject(issue) }
  .sort { |a, b| compare_key(a) <=> compare_key(b) }.reverse
  .map do |issue|
  labels = []
  labels << issue.status.name
  "#{issue.key} #{issue.summary} (#{labels.join(',')})"
end.join("\n")

File.write("/var/run/user/1000/jira-dump.txt", output)
