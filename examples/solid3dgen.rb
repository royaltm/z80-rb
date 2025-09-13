#!/usr/bin/env ruby
# -*- coding: BINARY -*-

PI = Math::PI

def fixed_float(f, n)
  f.round.to_s.rjust n
end

# Parses a *binary* STL and returns:
#   [ [ [x,y,z], [x,y,z], [x,y,z] ],  # face 1
#     [ [x,y,z], [x,y,z], [x,y,z] ],  # face 2
#     ... ]
#
# `source` can be a filepath (String), an IO (e.g., File), or a raw binary String.
def parse_binary_stl(source)
  require "stringio"
  io =
    if source.respond_to?(:read)
      source
    elsif source.is_a?(String) && File.file?(source)
      File.open(source, "rb")
    elsif source.is_a?(String)
      StringIO.new(source, "rb")
    else
      raise ArgumentError, "Pass a path, IO, or binary String."
    end

  # Binary STL layout:
  # 80-byte header, 4-byte LE uint32 triangle count,
  # then for each triangle: 12B normal + 36B vertices + 2B attr = 50B
  header = io.read(80) or raise "Invalid STL: missing 80-byte header"
  puts header
  tri_count_bytes = io.read(4) or raise "Invalid STL: missing triangle count"
  tri_count = tri_count_bytes.unpack1("V") # little-endian uint32

  faces = []
  tri_count.times do |i|
    record = io.read(50)
    raise "Invalid STL: unexpected EOF at triangle #{i}" if record.nil? || record.bytesize < 50

    floats = record[0, 48].unpack("e12") # 12 little-endian float32: nx,ny,nz, v1x,v1y,v1z, v2x, v2y, v2z, v3x, v3y, v3z
    v1 = floats[3, 3]
    v2 = floats[6, 3]
    v3 = floats[9, 3]
    faces << [v1, v2, v3]
    # attribute byte count (record[48,2]) is ignored
  end

  faces
ensure
  io.close if io.is_a?(File)
end

def gen_boing(ysect=5, zsect=7, radius=60)
  faces = []
  top_v = [0, radius, 0]
  bot_v = [0, -radius, 0]
  rings = []
  (1...ysect).each do |si|
    a = PI*si / ysect
    r = Math.sin(a)*radius
    y = Math.cos(a)*radius
    iring = []
    zsect.times do |i|
      a = PI*2*i / zsect
      x = Math.cos(a) * r
      z = -Math.sin(a) * r
      iring.push([x, y, z])
    end
    rings.push(iring)
  end
  top_ring = rings.first
  zsect.times do |i|
    a = top_ring[i]
    b = top_ring[(i+1) % zsect]
    faces.push([top_v, a, b])
  end
  (0...rings.length-1).each do |si|
    zsect.times do |i|
      a = rings[si][i]
      b = rings[si+1][i]
      c = rings[si+1][(i+1) % zsect]
      d = rings[si][(i+1) % zsect]
      faces.push([a, b, c, d])
    end
  end
  bot_ring = rings.last
  zsect.times do |i|
    a = bot_ring[(i+1) % zsect]
    b = bot_ring[i]
    faces.push([bot_v, a, b])
  end
  faces
end

def gen_diamond(sides=8, radius=30, height=100)
  top_r = radius
  mid_r = radius*1.5
  low_r = radius*1.75
  top_y = height * 0.5
  bot_y = -height * 0.5
  mid_y = (top_y - bot_y) * 8.0 / 9.0 + bot_y
  low_z = (top_y - bot_y) * 7.0 / 9.0 + bot_y
  faces = []
  # TOP RING
  top_ring = []
  mid_ring = []
  low_ring = []
  mid_shift = PI / sides
  sides.times do |i|
    a = PI*2*i / sides
    x = Math.cos(a) * top_r
    z = -Math.sin(a) * top_r
    top_ring.push([x, top_y, z])
    x = Math.cos(a+mid_shift) * mid_r
    z = -Math.sin(a+mid_shift) * mid_r
    mid_ring.push([x, mid_y, z])
    x = Math.cos(a) * low_r
    z = -Math.sin(a) * low_r
    low_ring.push([x, low_z, z])
  end
  faces.push(top_ring)
  sides.times do |i|
    a = mid_ring[i]
    b = top_ring[(i+1) % sides]
    c = top_ring[i]
    faces.push([a, b, c])
  end
  sides.times do |i|
    a = top_ring[(i+1) % sides]
    b = mid_ring[i]
    c = low_ring[(i+1) % sides]
    faces.push([a, b, c])
    d = mid_ring[(i+1) % sides]
    faces.push([c, d, a])
  end
  sides.times do |i|
    a = mid_ring[i]
    b = low_ring[i]
    c = low_ring[(i+1) % sides]
    faces.push([a, b, c])
  end
  sides.times do |i|
    a = [0, bot_y, 0]
    b = low_ring[(i+1) % sides]
    c = low_ring[i]
    faces.push([a, b, c])
  end
  faces
end

def gen_station(sect=8, radius=50, height=80)
  faces = []
  front_yb = height/2
  front_ya = front_yb - height/10
  back_y = -height/2
  front_ring_a = []
  front_ring_b = []
  back_ring = []
  radius_b = radius * 0.75
  sect.times do |i|
    a = PI*2*i / sect
    x = Math.cos(a) * radius
    z = -Math.sin(a) * radius
    front_ring_a.push([x,  front_ya, z])
    back_ring.push([x, back_y, z])
    x = Math.cos(a) * radius_b
    z = -Math.sin(a) * radius_b
    front_ring_b.push([x,  front_yb, z])
  end
  faces.push(back_ring.reverse)
  sect.times do |i|
    a = back_ring[i]
    b = back_ring[(i+1) % sect]
    c = front_ring_a[(i+1) % sect]
    d = front_ring_a[i]
    faces.push([a, b, c, d])
  end
  sect.times do |i|
    a = front_ring_a[i]
    b = front_ring_a[(i+1) % sect]
    c = front_ring_b[(i+1) % sect]
    d = front_ring_b[i]
    faces.push([a, b, c, d])
  end
  faces.push(front_ring_b)
  faces
end

# faces => [ [ [x,y,z],[x,y,z],[x,y,z] ], ... ]
def print_solid(name, faces)
  # vertex -> index
  vertices = Hash.new
  mins = [nil, nil, nil]
  maxs = [nil, nil, nil]

  puts "solid #{name}"
  puts
  faces.each_with_index do |face|
    puts "facet normal 0 0 0"
    puts "    outer loop"
    face.each do |v|
      v.each_with_index do |f, i|
        mins[i] = f if mins[i].nil? || f < mins[i]
        maxs[i] = f if maxs[i].nil? || f > maxs[i]
      end
      x, y, z = v
      puts "        vertex #{x} #{y} #{y}"
      unless vertices.has_key?(v)
        vertices[v] = vertices.length
      end
    end
    puts "    endloop"
    puts "endfacet"
  end
  puts
  puts "endsolid"
  puts
  puts

  origin = mins.zip(maxs).map do |min, max|
    (min + max) / 2.0
  end

  puts '                      dc!'
  puts '                      dc!"*********************************************"'
  puts '                      dc!"***' + (name + " 3D").upcase.center(39) + '***"'
  puts '                      dc!"*********************************************"'
  puts "  Object3D.make("
  vertices.length.times do |i|
    v = vertices.key(i)
    x, y, z = v.map.with_index do |f, i|
      fixed_float(f - origin[i], 3)
    end
    puts " "*4 + "[#{x}, #{y}, #{z}], ##{i}"
  end
  puts "  ) do |obj|"
  puts "    obj.scale!(1.0)"
  faces.each_with_index do |face|
    puts "    obj.face!(" + face.map {|v|
      i = vertices[v]
      raise "no such vertex: #{v}" if i.nil?
      i
    }.join(", ") + ")"
  end
  puts "    obj.to_prog(self, :#{name})"
  puts "  end"
end

file = ARGV.shift

case file
when "boing"
  print_solid "boing", gen_boing
when "diamond"
  print_solid "diamond", gen_diamond
when "station"
  print_solid "station", gen_station
when nil
  puts "solid3dgen boing|diamond|station"
  puts "solid3dgen FILE name"
else
  solid = ARGV.shift
  raise "expected solid name" unless solid
  print_solid solid, parse_binary_stl(file)
end
