#!/usr/bin/env ruby

require 'json'

# warn "" # force new line for readability
# warn "COMP_WORDS: #{ENV["COMP_WORDS"]}"
# warn "COMP_CWORD: #{ENV["COMP_CWORD"]}"
# warn "COMP_LINE: #{ENV["COMP_LINE"]}"

def find_root
  # find the root of the bazel workspace
  # warn "Finding root of bazel workspace"
  loop do
    break if File.exist?('WORKSPACE')

    Dir.chdir('..')
  end
  Dir.pwd
end

root = find_root
word_index = ENV['COMP_CWORD'].to_i
words = ENV['COMP_LINE'].split(' ')
last_word = ENV['COMP_LINE'].split(' ').last
word_being_completed = if word_index == words.size
                         ''
                       else
                         last_word
                       end

# warn "Word being completed: #{word_being_completed}"

results = {}

if word_index == 1
  # this is the command to run
  comp_reply = 'build test query run'
  results[:words] = comp_reply
  results[:word_count] = 4
end

# warn "Last word: #{last_word}"
if word_index == 2 && word_being_completed == ''
  results[:words] = '...'
  results[:word_count] = 1
end

if words[1] == 'query' && word_being_completed =~ %r{^//}
  # warn "Entering target completion"
  # warn "Root: #{root}"
  current_target_path = last_word.gsub(%r{^//}, '')
  current_path = File.join(root, current_target_path)
  possible_paths = `fd  --full-path #{current_path} --type d`.split("\n").map { |f| '//' + f + '...' }
  comp_reply = possible_paths.join(' ')
  results[:words] = comp_reply
  results[:word_count] = possible_paths.size
end

# warn "Reply: #{comp_reply}"
# warn "----"
puts results.to_json
