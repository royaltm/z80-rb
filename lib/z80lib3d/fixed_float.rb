class Float
  ##
  # Converts a float to a 16-bit fixed point twos complement number formatted: iiiiiiiiffffffff
  # where i represents the absolute integer part bits and f represents the fractional part bits.
  def to_fixed16_8
    n = (self * 256.0).round
    raise ArgumentError if n >= 32768 || n <= -32768
    n & 0xffff
  end
end
