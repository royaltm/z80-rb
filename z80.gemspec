require File.expand_path("../lib/z80/version", __FILE__)

Gem::Specification.new do |s|
  s.name = "z80"
  s.version = Z80::Program::VERSION
  s.required_ruby_version = ">= 2.1.0"
  s.executables = %w[zxconv zxinit zxrun zxgallery]
  s.summary = "Z80 CPU and ZX Spectrum programming tools"
  s.homepage = "https://royaltm.github.io/z80-rb/"
  s.license = "Nonstandard"
  s.require_path = "lib"
  s.description = "A powerfull Z80 Cpu macro assembler DSL and a build automation tool, a ZX Spectrum Basic parser and more."
  s.author = "Rafał Michalski"
  s.email = "royaltm75@gmail.com"
  s.files = Dir["{lib}/**/*.rb", "bin/zx*", "*.rdoc", "LICENSE.md", "licensezero.json"]
  s.rdoc_options << "--encoding" << "UTF-8" << 
    "--title" << "ruby-Z80" << "--main" << "README.rdoc" << "LICENSE.md"
  s.extra_rdoc_files = ["README.rdoc"]
  s.metadata = { "source_code_uri" => "https://github.com/royaltm/z80-rb" }
end