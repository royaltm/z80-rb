require 'rbconfig'

module ZXEmu
  EMU_DEFAULT = 'fuse'.freeze
  WIN_EMU_SEARCH_PATHS = [
    %w[Spectaculator SpecStub.exe],
    %w[Fuse fuse.exe]
  ]
  ##
  # Searches for a ZX Spectrum emulator program.
  # Returns a path to the executable on success.
  def ZXEmu.find_emulator
    @@default_zxemu_path ||= (if RbConfig::CONFIG['host_os'] =~ /mswin|mingw|cygwin/
      ['PROGRAMFILES', 'ProgramFiles(x86)'].flat_map do |env_var|
        program_files = ENV[env_var] || 'C:\Program Files'
        WIN_EMU_SEARCH_PATHS.map { |parts| File.join(program_files, *parts) }
      end
    end || begin
      exts = ENV['PATHEXT'] ? ENV['PATHEXT'].split(';') : ['']
      ENV['PATH'].split(File::PATH_SEPARATOR).flat_map do |path|
        exts.map { |ext| File.join(path, EMU_DEFAULT + ext) }
      end
    end).uniq.find {|path| !File.directory?(path) && File.executable?(path) }
  end

  ##
  # Run ZX Spectrum emulator program with the given +file+.
  #
  # Unless +ZXEMU_PATH+ environment variable is set the emulator
  # is being searched for with a +ZXEmu.find_emulator+ method.
  def ZXEmu.run(file)
    emu_path = ENV['ZXEMU_PATH'] || ZXEmu.find_emulator
    unless emu_path
      raise "ZX Spectrum emulator not found. Define ZXEMU_PATH environment variable to help me find it."
    end
    exec emu_path, File.expand_path(file)
  end
end
