# frozen_string_literal: true
require_relative 'examples/rakefile.rb'

desc "Documentation"
task :doc do
  sh "rdoc --encoding=UTF-8 --title=ruby-Z80 --main=README.rdoc README.rdoc LICENSE.md CHANGELOG.md lib/z80.rb lib/z80/*.rb lib/z80/utils/*.rb lib/zxlib/*.rb lib/zxlib/*/*.rb lib/zxutils/*.rb lib/zxutils/*/*.rb lib/z80lib3d/*.rb"
  sh "cp -v examples/*.{jpg,png,tap} doc/examples/"
end

desc "Build the gem"
task :gem do
  sh "gem build z80.gemspec"
end

desc "Update the local gem library with the current z80rb sources"
task :update => [:uninstall, :clean, :gem, :install]

desc "Install the library at local machnie"
task :install => :gem do 
  sh "gem install z80-*.gem"
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
  bfont_demo_hires.rb
  calculator.rb
  dots.rb
  ghosts.rb
  horse.rb
  labyrinth.rb
  mathi_test.rb
  multifill.rb
  palette64.rb
  quat3d128.rb
  sprites.rb
  stars.rb
  test_ham256.rb
  test_music.rb
]

desc "Compile examples"
task :examples do
  EXAMPLES.each do |example|
    puts '','='*60, "Example: #{example}".center(40), '-'*60
    sh "ruby -Ilib #{File.join("examples", example)}"
  end
  Rake::Task['example:gallery'].invoke
end

UTIL_TESTS = %w[
  z80/stdlib.rb
  z80/utils/shuffle.rb
  z80/utils/sincos.rb
  z80/utils/sort.rb
  z80/utils/vec_deque.rb
  zxlib/ay_sound.rb
  zxlib/math.rb
  zxlib/gfx/draw.rb
  zxutils/ay_music.rb
  zxutils/ay_music_player.rb
  zxutils/benchmark.rb
  zxutils/gallery.rb
  zxutils/multitasking.rb
  zxutils/multitasking_io.rb
]

desc "Compile utils' tests"
task :utils do
  UTIL_TESTS.each do |path|
    puts '','='*60, "Utility: #{path}".center(40), '-'*60
    sh "ruby -Ilib #{File.join("lib", path)}"
  end
end

TESTS = %w[
  test.math_i.mul16_9.rb
  test.math_i.mul8_24.rb
  test.math_i.mul8_signed.rb
  test.math_i.mul9_24.rb
  test.math_i.mul_const.rb
  test.math_i.mul_const8_24.rb
  test.math_i.mul_signed9.rb
  test.math_i.mul_signed9_24.rb
  test.zxlib.basic.se.rb
]

desc "Compile external tests"
task :test do
  TESTS.each do |path|
    puts '','='*60, "Test: #{path}".center(40), '-'*60
    sh "ruby -Ilib #{File.join("tests", path)}"
  end
end

BENCHES = %w[
  bench.divmod.rb
  bench.mul8_24.rb
  bench.mul_signed9.rb
  bench.mul_signed9_lim.rb
  bench.rnd.rb
  bench.sincos_mul.rb
  bench.sort.rb
  bench.ubcd.rb
]

desc "Compile benchmarks"
task :bench do
  BENCHES.each do |path|
    puts '','='*60, "Benchmark: #{path}".center(40), '-'*60
    sh "ruby -Ilib #{File.join("tests", path)}"
  end
end
