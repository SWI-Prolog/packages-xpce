# class transform {#class-transform}

2-D affine transformation.  A `transform` represents the matrix

    [ x' ]   [ xx  xy ] [ x ]   [ tx ]
    [    ] = [        ] [   ] + [    ]
    [ y' ]   [ yx  yy ] [ y ]   [ ty ]

that maps points and areas from a source coordinate space into a
target space.  Composition follows the cairo convention: methods such
as `->scale` and `->rotate` post-multiply the operation into self
(`self := self * Op`), so the operation is applied to the input
*first* and the receiver afterwards.  A sequence

    send(T, scale, 2), send(T, rotate, 30)

therefore rotates a point first and then scales the rotated result.

@see class point
@see class area
@see class device


## Instance variables {#class-transform-instvars}

- transform<->xx: num
    Top-left coefficient of the 2x2 linear part (x-scale).

- transform<->xy: num
    Top-right coefficient (y contribution to x; shear).

- transform<->yx: num
    Bottom-left coefficient (x contribution to y; shear).

- transform<->yy: num
    Bottom-right coefficient (y-scale).

- transform<->tx: num
    Translation along x.

- transform<->ty: num
    Translation along y.


## Send methods {#class-transform-send}

- transform->initialise: rotate=[num], scale=[num|tuple], shear=[tuple]
    Build from optional rotate (degrees), scale (uniform `num` or
    `tuple(sx, sy)`) and shear (`tuple(kx, ky)`).  The fresh transform
    starts at identity; the supplied operations are applied in the
    fixed order scale, rotate, shear.  Following the post-multiplication
    convention this means an input point is transformed shear-first,
    then rotated, then scaled.

    For an arbitrary transform whose six coefficients are known, use

        new(T, transform), send(T, set, XX, XY, YX, YY, TX, TY)

- transform->set: xx=num, xy=num, yx=num, yy=num, tx=num, ty=num
    Set all six matrix coefficients directly.

- transform->copy: from=transform
    Copy the six coefficients from another transform.

- transform->identity
    Reset to the identity transform.

- transform->translate: dx=num, dy=num
    Post-multiply by a translation: `self := self * translate(dx,dy)`.

- transform->scale: sx=num, sy=[num]
    Post-multiply by a (possibly non-uniform) scale.  If `sy` is
    omitted the same factor is used for both axes.

- transform->rotate: degrees=num
    Post-multiply by a rotation around the origin.

- transform->shear: kx=num, ky=num
    Post-multiply by a shear matrix `[1 kx; ky 1]`.

- transform->compose: transform
    Post-multiply by the argument: `self := self * argument`.

- transform->invert
    Invert in place.  Fails when the transform is singular (the
    determinant of the linear part is zero).


## Get methods {#class-transform-get}

- transform<-copy: -> transform
    Independent copy of this transform.

- transform<-inverse: -> transform
    New transform that is the inverse of the receiver.  Fails when
    the receiver is singular.

- transform<-determinant: -> num
    Determinant of the 2x2 linear part.  Zero (or very near zero)
    means the transform is singular and cannot be inverted.

- transform<-apply: point|area -> point|area
    Map a `point` through the transform, or compute the axis-aligned
    bounding box of an `area` mapped through it.  Results that are
    within float-roundoff of an integer are snapped to that integer
    so that exact 90/180/270 degree rotations of integer-aligned
    inputs yield integer-aligned outputs.
