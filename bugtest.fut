-- import "lib/github.com/diku-dk/sorts/radix_sort"
import "fut/radix_sort"

module Scalar = f32

def main [b][g] (bag:[b][g]Scalar.t)
                (orderBag:[b][g]Scalar.t):
                [b][g](Scalar.t,Scalar.t) =
  let compBag = (map2 zip bag (map (map (Scalar.max 0)) orderBag))
  in  map (radix_sort_float_by_key (.1) Scalar.num_bits Scalar.get_bit) compBag

-- ==
-- random input { [49152][128]f32 [49152][128]f32 }
