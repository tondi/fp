roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c) = ( (-b - d) / e, (-b + d) / e )
   where d = sqrt (b * b - 4 * a * c)
         e = 2 * a


unitVec2D :: (Double, Double) -> (Double, Double)
unitVec2D (a, b) = (1 / len * a, 1 / len * b)
    where len = sqrt(a^2 + b^2)