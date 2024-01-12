module Z80Lib3D
  ##
  # =Z80Lib3D::Quaternion
  #
  # Quaternion related helper methods.
  module Quaternion
    include Math
    ## Returns a rotor quaternion from radians and a unit vector.
    def rotor_quaternion(rad, av)
      sn = sin(rad/2)
      x, y, z = av
      [cos(rad/2),sn*x,sn*y,sn*z]
    end

    ## Returns a quaternion square norm.
    def quaternion_norm_q(q)
      s, x, y, z = q
      s*s + x*x + y*y + z*z
    end

    ## Returns a quaternion norm.
    def quaternion_norm(q)
      sqrt(quaternion_norm_q(q))
    end

    ## Returns a normalized quaternion.
    def normalize_quaternion(q)
      n = quaternion_norm(q)
      s, x, y, z = q
      [s/n, x/n, y/n, z/n]
    end

    ## Returns a quaternion as a cross product of quaternions a and b.
    def quaternion_cross_product(a, b)
      sa, xa, ya, za = a
      sb, xb, yb, zb = b
      [sa*sb - xa*xb - ya*yb - za*zb,
       sa*xb + sb*xa + ya*zb - yb*za,
       sa*yb + sb*ya + za*xb - zb*xa,
       sa*zb + sb*za + xa*yb - xb*ya]
    end

    ## Returns a transformation matrix from a quaternion.
    def quaternion2matrix(q)
      sq, xq, yq, zq = normalize_quaternion(q)

      xq2 = xq*xq
      yq2 = yq*yq
      zq2 = zq*zq

      xqyq = xq*yq
      sqzq = sq*zq
      xqzq = xq*zq
      sqyq = sq*yq
      yqzq = yq*zq
      sqxq = sq*xq

      qxx = 1.0 - 2.0*(yq2 + zq2)
      qyy = 1.0 - 2.0*(xq2 + zq2)
      qzz = 1.0 - 2.0*(xq2 + yq2)

      qxy = (xqyq - sqzq)*2.0
      qxz = (xqzq + sqyq)*2.0
      qyx = (xqyq + sqzq)*2.0
      qyz = (yqzq - sqxq)*2.0
      qzx = (xqzq - sqyq)*2.0
      qzy = (yqzq + sqxq)*2.0

      [qxx, qxy, qxz,
       qyx, qyy, qyz,
       qzx, qzy, qzz]
    end
  end
end
