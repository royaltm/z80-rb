OPTIONS = {
    clrrem: true,
    check0: true, #:check0,
    check0_far: false,
    check1: true, #:check1,
    check1_far: nil,
    k_leq_m: true,
    modulo: false,
    ignore_cf: false,
    optimize: :time,
    subroutine: false,
    skip_clra: false
}.freeze

require_relative 'bench.divmod.base'
