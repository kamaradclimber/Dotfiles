#!/usr/bin/env ruby

require "mixlib/shellout"

expected_markers = case Mixlib::ShellOut.new("git config merge.conflictStyle").run_command.tap(&:error!).stdout.strip
          when "diff3", "zdiff3"
            4
          else
            3
          end




diff_check = Mixlib::ShellOut.new("git diff --check").run_command

locations = diff_check.stdout.split("\n").map do |line|
  filename, line, _ = line.split(":")
  [filename, line]
end.group_by(&:first).transform_values { |values| values.map(&:last) }

locations = locations.flat_map do |filename, lines|
  if lines.size % expected_markers != 0
    # not a multiple of the expected number of conflict markers per conflict, adding them all
    refs = lines
  else
    # take the 2nd marker of each slice (this is marker for "previous" version)
    refs = lines.each_slice(expected_markers).map { |slice| slice[1] }
  end
  refs.map { |ref| [filename, ref] }
end

if locations.empty?
  puts "No conflicts found"
  exit 0
end

suffix = locations.drop(1).map do |filename, ref|
    "-c 'e #{filename} | #{ref}'"
end.join(" ")

cmd = "nvim #{locations.first[0]} +#{locations.first[1]} #{suffix} -c first"

puts "Opening all files in different buffers, use :b2, :b3, ... to navigate"
exec(cmd)
