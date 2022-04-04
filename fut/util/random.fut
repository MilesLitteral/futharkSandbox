import "../hoas"
import "cpprandom"

module Scalar = f32
module dist = uniform_real_distribution f32 minstd_rand

type Rng = minstd_rand.rng
let mkRng (seed:i64):Rng = minstd_rand.rng_from_seed [i32.i64 seed]
let splitRng (i:i64) (rng:Rng):[i]Rng = minstd_rand.split_rng i rng
let joinRng [i] (rs:[i]Rng):Rng = minstd_rand.join_rng rs

let uniformField (rng:Rng) = dist.rand (0,1) rng

let randomField_1d (w:i64)
                   (rng:Rng):
                   (Rng, [w](Scalar.t)) =
    let rngs = minstd_rand.split_rng w rng
    let (rngs', rs) = unzip (map uniformField rngs)
    let rng' = minstd_rand.join_rng rngs'
    in  (rng', rs)

let randomField_2d (h:i64) (w:i64)
                   (rng:Rng):
                   (Rng, [h][w](Scalar.t)) =
    let (rng', rs) = randomField_1d (h*w) rng
    in  (rng', unflatten h w rs)

let randomField_3d (h:i64) (w:i64) (d:i64)
                   (rng:Rng):
                   (Rng, [h][w][d](Scalar.t)) =
    let (rng', rs) = randomField_1d (h*w*d) rng
    in  (rng', unflatten_3d h w d rs)
