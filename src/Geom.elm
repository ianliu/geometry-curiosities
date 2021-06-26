module Geom exposing (..)
import Html exposing (b)


type alias Vec2 =
    ( Float, Float )


{-| Adds two vectors together

    add ( 1, 2 ) ( 3, 4 ) == ( 4, 6 )

-}
add : Vec2 -> Vec2 -> Vec2
add ( ax, ay ) ( bx, by ) =
    ( ax + bx, ay + by )


{-| Subtracts the second vector from the first

    sub ( 10, 10 ) ( 5, 10 ) == ( 5, 0 )

-}
sub : Vec2 -> Vec2 -> Vec2
sub ( ax, ay ) ( bx, by ) =
    ( ax - bx, ay - by )


{-| Negates a vector
-}
neg : Vec2 -> Vec2
neg ( x, y ) =
    ( -x, -y )


{-| Multiply a scalar with a vector

    mul 10 ( 1, 2 ) == ( 10, 20 )

-}
mul : Float -> Vec2 -> Vec2
mul s ( x, y ) =
    ( s * x, s * y )


{-| Dot product of two vectors

    dot ( 1, 2 ) ( 3, 4 ) == 11

-}
dot : Vec2 -> Vec2 -> Float
dot ( ax, ay ) ( bx, by ) =
    ax * bx + ay * by


{-| Magnitude of the cross product of two vectors

    cross ( 1, 2 ) ( 3, 4 ) == -5

-}
cross : Vec2 -> Vec2 -> Float
cross ( ax, ay ) ( bx, by ) =
    ax * by - ay * bx


{-| Squared magnitude of a vector

    norm2 ( 3, 4 ) == 25

-}
norm2 : Vec2 -> Float
norm2 v =
    dot v v


{-| Magnitude of a vector

    norm2 ( 3, 4 ) == 5

-}
norm : Vec2 -> Float
norm v =
    sqrt (norm2 v)


{-| Normalizes a vector to magnitude 1

    unit ( 10, 0 ) == ( 1, 0 )

-}
unit : Vec2 -> Vec2
unit v =
    mul (1 / norm v) v


{-| Projects the first vector onto the second

    project ( 3, 4 ) ( 5, 0 ) == ( 3, 0 )

-}
project : Vec2 -> Vec2 -> Vec2
project u v =
    mul (dot u v / norm2 v) v


{-| Rotates 90 degrees counter clockwise

    rot90 ( 1, 0 ) == ( 0, 1 )

-}
rot90 : Vec2 -> Vec2
rot90 ( x, y ) =
    ( -y, x )


{-| Rotates 90 degrees clockwise

    rot90 ( 1, 0 ) == ( 0, -1 )

-}
irot90 : Vec2 -> Vec2
irot90 ( x, y ) =
    ( y, -x )


{-| Computes a vector bisecting the angle between a and b
-}
bisect : Vec2 -> Vec2 -> Vec2
bisect a b =
    add (unit a) (unit b)


{-| Computes the interception of a ray, given by a point and a direction, and a
line, given by two points
-}
rayInterceptLine : Vec2 -> Vec2 -> Vec2 -> Vec2 -> Vec2
rayInterceptLine p d a b =
    let
        ba =
            sub b a

        det =
            cross d ba
    in
    add p (mul (dot (irot90 ba) (sub b p) / det) d)


{-| Computes the interception of two lines, given by two points each
-}
lineInterceptLine : Vec2 -> Vec2 -> Vec2 -> Vec2 -> Vec2
lineInterceptLine a b c d =
    rayInterceptLine a (sub a b) c d


{-| Computes the interception of two rays, given by a point and a direction each
-}
rayInterceptRay : Vec2 -> Vec2 -> Vec2 -> Vec2 -> Vec2
rayInterceptRay pa da pb db =
    rayInterceptLine pa da pb (add pb db)


{-| Computes the orthocenter of a triangle given by three points
-}
orthocenter : Vec2 -> Vec2 -> Vec2 -> Vec2
orthocenter a b c =
    let
        ha = rot90 (sub b c)
        hb = rot90 (sub a c)
    in
    rayInterceptRay a ha b hb


{-| Computes the centroid of a triangle given by three points
-}
centroid : Vec2 -> Vec2 -> Vec2 -> Vec2
centroid a b c =
    let
        ma = mul 0.5 (add b c)
        mb = mul 0.5 (add a c)
    in
    lineInterceptLine a ma b mb


{-| Computes the circumcenter of a triangle given by three points
-}
circumcenter : Vec2 -> Vec2 -> Vec2 -> Vec2
circumcenter a b c =
    let
        ma = mul 0.5 (add b c)
        mb = mul 0.5 (add a c)
        ha = rot90 (sub b c)
        hb = rot90 (sub a c)
    in
    rayInterceptRay ma ha mb hb
