require File.expand_path("../lib/z80/version", __FILE__)

Gem::Specification.new do |s|
  s.name = "z80"
  s.version = Z80::Program::VERSION
  s.required_ruby_version = ">= 2.1.0"
  s.executables = %w[zxconv zxinit zxrun]
  s.summary = "Z80 CPU and ZX Spectrum programming tools"
  s.homepage = "https://royaltm.github.io/z80-rb/"
  s.license = "LGPL-3.0"
  s.require_path = "lib"
  s.description = "Z80 assembler DSL in ruby, build system, ZX Spectrum basic parser and more"
  s.author = "Rafał Michalski"
  s.email = "r-type@yeondir.com"
  s.files = Dir["{lib}/**/*.rb", "bin/zxconv*", "LICENSE", "*.rdoc"]
  s.rdoc_options << "--encoding" << "UTF-8" << 
    "--title" << "ruby-Z80" << "--main" << "README.rdoc"
  s.extra_rdoc_files = ["README.rdoc"]
  s.metadata = { "source_code_uri" => "https://github.com/royaltm/z80-rb" }
end