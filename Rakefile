# frozen_string_literal: true

desc "Documentation"
task :doc do
  sh "rdoc --encoding=UTF-8 --title=ruby-Z80 --main=README.rdoc README.rdoc lib/z80.rb lib/z80/*.rb lib/z80/utils/*.rb lib/zxlib/*.rb lib/zxlib/*/*.rb lib/zxutils/*.rb"
  sh "cp -v examples/*.{jpg,png} doc/examples/"
end

desc "Build the gem"
task :gem do
  sh "gem build z80.gemspec"
end

desc "Install the library at local machnie"
task :install => :gem do 
  sh "gem install z80 -l"
end

desc "Uninstall the library from local machnie"
task :uninstall do
  sh "gem uninstall z80"
end

desc "Clean"
task :clean do
  sh "rm z80-*.gem"
end

EXAMPLES = %w[
  bfont_demo.rb
  calculator.rb
  dots.rb
  horse.rb
  labyrinth.rb
  mathi_test.rb
  multifill.rb
  stars.rb
]

desc "Compile examples"
task :examples do
  EXAMPLES.each do |example|
    sh "ruby #{File.join("examples", example)}"
  end
end

UTIL_TESTS = %w[
  z80/stdlib.rb
  z80/utils/shuffle.rb
  z80/utils/sincos.rb
  zxlib/math.rb
  zxlib/gfx/draw.rb
  zxlib/ay_sound.rb
  zxutils/ay_music.rb
  zxutils/benchmark.rb
  zxutils/multitasking.rb
  zxutils/multitasking_io.rb
]

desc "Compile utils' tests"
task :utils do
  UTIL_TESTS.each do |path|
    sh "ruby -Ilib #{File.join("lib", path)}"
  end
end
