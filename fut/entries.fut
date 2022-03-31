import "manifut/util/random"
import "manifut/util/radix_sort"

module Scalar = f32

entry randomField (seed:i64)
                  (b:i64)
                  (g:i64):
                  ([][]f32) =
  let rng0 = mkRng seed
  let (_rng1, field) = randomField_2d b g rng0
  in  field

entry sortBag [b][g]
              (keys:[b][g]f32)
              (values:[b][g]f32):
              ([b][g]f32, [b][g]f32) =
  let compBag = map2 zip values (map (map (Scalar.max 0)) keys)
  let sorted = map (radix_sort_float_by_key (.1) Scalar.num_bits Scalar.get_bit) compBag
  let values' = map (map (.0)) sorted
  let keys' = map (map (.1)) sorted
  in  (values', keys')

entry sortBagAndDoStuff [b][g]
                        (keys:[b][g]f32)
                        (values:[b][g]f32):
                        ([b][g]f32,[b][g]f32,[b]f32,[g]f32) =
  let compBag = map2 zip values (map (map (Scalar.max 0)) keys)
  let sorted = map (radix_sort_float_by_key (.1) Scalar.num_bits Scalar.get_bit) compBag
  let keys' = map (map (.1)) sorted
  let values' = map (map (.0)) sorted
  let averagePerRow = map ((/Scalar.i64 g) <-< Scalar.sum) values'
  let averagePerCol = map ((/Scalar.i64 b) <-< Scalar.sum) (transpose values')
  in  (keys', values', averagePerRow, averagePerCol)
