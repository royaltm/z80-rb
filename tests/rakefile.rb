# frozen_string_literal: true
require 'fileutils'
# OPTIONS = {
#     clrrem: true,
#     check0: true, #:check0,
#     check0_far: false,
#     check1: true, #:check1,
#     check1_far: false,
#     k_leq_m: true,
#     modulo: false,
#     ignore_cf: false,
#     optimize: :time,
#     subroutine: false,
#     skip_clra: false
# }.freeze

OPTIMIZATIONS = [
  :size,
  :time,
  :time_alt,
  :unroll,
  :unroll_alt,
]

MATRIX = {
  "check_0_1" => {
    clrrem: true, check0: true, check0_far: false, check1: true, check1_far: false, k_leq_m: false, ignore_cf: false, skip_clra: false
  },
  "check_0_1_label" => {
    clrrem: true, check0: :check0, check0_far: false, check1: :check1, check1_far: false, k_leq_m: false, ignore_cf: false, skip_clra: false
  },
  "check_0_1_label_far" => {
    clrrem: true, check0: :check0, check0_far: true, check1: :check1, check1_far: true, k_leq_m: false, ignore_cf: false, skip_clra: false
  },
  "check_0_1_label_nocf" => {
    clrrem: true, check0: :check0, check0_far: false, check1: :check1, check1_far: false, k_leq_m: false, ignore_cf: true, skip_clra: false
  },
  "check_0_1_label_far_nocf" => {
    clrrem: true, check0: :check0, check0_far: true, check1: :check1, check1_far: true, k_leq_m: false, ignore_cf: true, skip_clra: false
  },

  "check_0_1_km" => {
    clrrem: true, check0: true, check0_far: false, check1: true, check1_far: false, k_leq_m: true, ignore_cf: false, skip_clra: false
  },
  "check_0_1_km_label" => {
    clrrem: true, check0: :check0, check0_far: false, check1: :check1, check1_far: false, k_leq_m: true, ignore_cf: false, skip_clra: false
  },
  "check_0_1_km_label_far" => {
    clrrem: true, check0: :check0, check0_far: true, check1: :check1, check1_far: true, k_leq_m: true, ignore_cf: false, skip_clra: false
  },
  "check_0_1_km_label_nocf" => {
    clrrem: true, check0: :check0, check0_far: false, check1: :check1, check1_far: false, k_leq_m: true, ignore_cf: true, skip_clra: false
  },
  "check_0_1_km_label_far_nocf" => {
    clrrem: true, check0: :check0, check0_far: true, check1: :check1, check1_far: true, k_leq_m: true, ignore_cf: true, skip_clra: false
  },

  "check_0" => {
    clrrem: true, check0: true, check0_far: false, check1: false, check1_far: false, k_leq_m: false, ignore_cf: false, skip_clra: false
  },
  "check_0_label" => {
    clrrem: true, check0: :check0, check0_far: false, check1: false, check1_far: false, k_leq_m: false, ignore_cf: false, skip_clra: false
  },
  "check_0_label_far" => {
    clrrem: true, check0: :check0, check0_far: true, check1: false, check1_far: false, k_leq_m: false, ignore_cf: false, skip_clra: false
  },
  "check_0_label_nocf" => {
    clrrem: true, check0: :check0, check0_far: false, check1: false, check1_far: false, k_leq_m: false, ignore_cf: true, skip_clra: false
  },
  "check_0_label_far_nocf" => {
    clrrem: true, check0: :check0, check0_far: true, check1: false, check1_far: false, k_leq_m: false, ignore_cf: true, skip_clra: false
  },

  "check_0_km" => {
    clrrem: true, check0: true, check0_far: false, check1: false, check1_far: false, k_leq_m: true, ignore_cf: false, skip_clra: false
  },
  "check_0_km_label" => {
    clrrem: true, check0: :check0, check0_far: false, check1: false, check1_far: false, k_leq_m: true, ignore_cf: false, skip_clra: false
  },
  "check_0_km_label_far" => {
    clrrem: true, check0: :check0, check0_far: true, check1: false, check1_far: false, k_leq_m: true, ignore_cf: false, skip_clra: false
  },
  "check_0_km_label_nocf" => {
    clrrem: true, check0: :check0, check0_far: false, check1: false, check1_far: false, k_leq_m: true, ignore_cf: true, skip_clra: false
  },
  "check_0_km_label_far_nocf" => {
    clrrem: true, check0: :check0, check0_far: true, check1: false, check1_far: false, k_leq_m: true, ignore_cf: true, skip_clra: false
  },

  "check_1" => {
    clrrem: true, check0: false, check0_far: false, check1: true, check1_far: false, k_leq_m: false, ignore_cf: false, skip_clra: false
  },
  "check_1_label" => {
    clrrem: true, check0: false, check0_far: false, check1: :check1, check1_far: false, k_leq_m: false, ignore_cf: false, skip_clra: false
  },
  "check_1_label_far" => {
    clrrem: true, check0: false, check0_far: false, check1: :check1, check1_far: true, k_leq_m: false, ignore_cf: false, skip_clra: false
  },

  "check_1_km" => {
    clrrem: true, check0: false, check0_far: false, check1: true, check1_far: false, k_leq_m: true, ignore_cf: false, skip_clra: false
  },
  "check_1_km_label" => {
    clrrem: true, check0: false, check0_far: false, check1: :check1, check1_far: false, k_leq_m: true, ignore_cf: false, skip_clra: false
  },
  "check_1_km_label_far" => {
    clrrem: true, check0: false, check0_far: false, check1: :check1, check1_far: true, k_leq_m: true, ignore_cf: false, skip_clra: false
  },

  "check_km" => {
    clrrem: true, check0: false, check0_far: false, check1: false, check1_far: false, k_leq_m: true, ignore_cf: false, skip_clra: false
  },
  "nochecks" => {
    clrrem: true, check0: false, check0_far: false, check1: false, check1_far: false, k_leq_m: false, ignore_cf: false, skip_clra: false
  },
  "nochecks_skip_clra" => {
    clrrem: true, check0: false, check0_far: false, check1: false, check1_far: false, k_leq_m: false, ignore_cf: false, skip_clra: true
  },
  "noclrrem" => {
    clrrem: false, check0: false, check0_far: false, check1: false, check1_far: false, k_leq_m: false, ignore_cf: false, skip_clra: false
  },
}

namespace :benches do
  desc "Prepare bench divmod all"
  task :divmod do
    OPTIMIZATIONS.each do |optimize|
      File.open("bench.#{optimize.to_s}.txt", "w") do |script|
        [false, true].each do |modulo|
          part1 = if modulo then 'modulo' else 'divmod' end
          [false, true].each do |subroutine|
            part2 = if subroutine then 'sub' else 'inline' end
            destpath = File.join('tests', 'benches', 'divmod', optimize.to_s, part1, part2)
            puts "="*60, destpath
            FileUtils.mkdir_p destpath
            MATRIX.each do |part3, opts|
              next if optimize == :unroll_alt &&
                ((opts[:check0] == :check0 && !opts[:check0_far]) ||
                 (opts[:check1] == :check1 && !opts[:check1_far]))
              base_name = "#{optimize.to_s}.#{part1}.#{part2}.#{part3}"
              puts "  #{base_name}"
              options = opts.merge({modulo: modulo, optimize: optimize, subroutine: subroutine})
              opt_head = options.map {|k, v| "#{k}: #{v.inspect}"}.join(", ")
              out_path = File.join(destpath, base_name + '.out')
              command = "OPTIONS=#{options.inspect};load '#{File.join("tests", "bench.divmod.base.rb")}'"
              File.open(out_path, "w") {|f| f.puts opt_head }
              sh "ruby -Ilib -e \"#{command}\" >>#{out_path}"
              FileUtils.mv("bench.divmod.tap", "bench.#{base_name}.tap")
              txt_dest = File.join(destpath, base_name) + ".txt"
              prog_file = "bench.#{base_name}.tap"
              script.puts "#{prog_file} >#{txt_dest}"
              # zxrun bench.#{base_name}.tap -- -m 48k -i resources/if1-2.rom -p >>#{txt_dest}
            end
          end
        end
      end
    end
  end
end
