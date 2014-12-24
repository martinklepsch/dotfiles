#!/usr/bin/ruby

require 'fileutils'
home = Dir.home
photo_dir = "#{home}/Dropbox/Camera Uploads"

# Dir.open "#{home}/Dropbox/Camera Test/" do |d|
#   d.glob("*.jpg").each do |f|
#     t = File.mtime(f)
#     m = t.strftime("%B")
#     puts "#{t.year}-#{m}"
#   end
# end

photos = Dir.glob "#{photo_dir}/*.jpg"

puts "#{photos.count} photos will be moved"

photos.each do |f|
  t = File.mtime(f)
  ms = t.strftime("%B")
  mn = t.strftime("%m")

  target_dir = "#{photo_dir}/#{t.year}-#{mn}-#{ms}"
  #target_dir = photo_dir
  target_loc = "#{target_dir}/#{File.basename(f)}"

  unless Dir.exists?(target_dir)
    Dir.mkdir target_dir
  end

  FileUtils.mv(f, target_loc)
  puts "#{f} was moved to #{target_loc}"
end
