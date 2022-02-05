#!/usr/bin/env ruby

require 'jira-ruby'

raise 'Must set CRITEO_USER env var' unless ENV['CRITEO_USER']
raise 'Most set CRITEO_PASSWORD env var' unless ENV['CRITEO_PASSWORD']

options = {
  username:     ENV['JIRA_USER'] || "#{ENV['CRITEO_USER']}@criteo.com",
  password:     ENV['JIRA_PASSWORD'] || ENV['CRITEO_PASSWORD'],
  site:         'https://criteo.atlassian.net:443',
  context_path: '',
  auth_type:    :basic
}

client = JIRA::Client.new(options)

issues = client.Issue.jql('(project in (MESOS, LAKE, KUBE) OR assignee = currentUser()) AND resolution is EMPTY ORDER BY updated DESC', max_results: 500)

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
  assignee_key = case issue.assignee&.emailAddress
                 when ENV['CRITEO_USER']
                   10
                 when nil
                   0
                 else
                   1
                 end
  [assignee_key, status, issue.updated, issue.created]
end

def emoji(issue)
  return '🏰' if issue.issuetype&.name == 'Epic'
  return '🐛' if issue.issuetype&.name == 'Bug'
end

output = issues
  .reject { |issue| reject(issue) }
  .sort { |a, b| compare_key(a) <=> compare_key(b) }.reverse
  .map do |issue|
  labels = []
  labels << issue.status.name
  emji = emoji(issue)
  summary = issue.summary.gsub('&', '_')
  "#{issue.key} #{summary} (#{labels.join(',')}) #{emji}".strip
end.join("\n")

File.write("/var/run/user/1000/jira-dump.txt", output)
