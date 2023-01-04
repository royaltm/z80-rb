require 'rbconfig'

module ZXUtils
	##
	# =ZXUtils::Emu
	#
	# Simple tools for finding and running ZX Spectrum emulator.
	#
	# Compile your program, save to tap and then:
	#
	#   ZXUtils::Emu.run("foo.tap")
	module Emu
		EMU_DEFAULT = 'fuse'.freeze
		WIN_EMU_SEARCH_PATHS = [
			%w[Spectaculator SpecStub.exe],
			%w[Fuse fuse.exe]
		]
		##
		# Searches for an installed ZX Spectrum emulator program in the system.
		# Returns a path to the executable on success.
		#
		# Currently it looks, in the following order, for:
		#
		# * Spectaculator: http://www.spectaculator.com (Windows only)
		# * Fuse: http://fuse-emulator.sourceforge.net
		def Emu.find_emulator
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
		# Returns a path to the executable file of a ZX Spectrum emulator.
		#
		# The path is being determined by +ZXEMU_PATH+ environment variable.
		# If +ZXEMU_PATH+ is not defined Emu.find_emulator is being consulted.
		#
		# Raises an error when the path wasn't found.
		def Emu.get_emulator_path
			emu_path = ENV['ZXEMU_PATH'] || Emu.find_emulator
			unless emu_path
				raise "ZX Spectrum emulator not found. Define ZXEMU_PATH environment variable to help me find it."
			end
			emu_path
		end
		##
		# Runs a ZX Spectrum emulator program with the given +file+ as its argument.
		#
		# Provides additional +args+ to the emulator program before the +file+ argument.
		#
		# _NOTE_:: Replaces the ruby process with the one of an emulator.
		def Emu.run(file, *args)
			Kernel.exec Emu.get_emulator_path, *args, File.expand_path(file)
		end
		##
		# Spawns a ZX Spectrum emulator program with the given +file+ as its argument.
		#
		# Provides additional +args+ to the emulator program before the +file+ argument.
		#
		# _NOTE_:: Ruby process is being detached from the spawned emulator process.
		def Emu.spawn(file, *args)
			Process.detach Kernel.spawn(Emu.get_emulator_path, *args, File.expand_path(file))
		end
	end
end
