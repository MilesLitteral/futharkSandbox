let fst  'a 'b ( x:a, _y:b):a = x
let snd  'a 'b (_x:a,  y:b):b = y

let fst3  'a 'b 'c ( x:a, _y:b, _z:c):a = x
let snd3  'a 'b 'c (_x:a,  y:b, _z:c):b = y
let thrd3 'a 'b 'c (_x:a, _y:b,  z:c):c = z

let fst4  'a 'b 'c 'd ( x:a, _y:b, _z:c, _w:d):a = x
let snd4  'a 'b 'c 'd (_x:a,  y:b, _z:c, _w:d):b = y
let thrd4 'a 'b 'c 'd (_x:a, _y:b,  z:c, _w:d):c = z
let frth4 'a 'b 'c 'd (_x:a, _y:b, _z:c,  w:d):d = w

let map_2d 'a 'b [h][w] (f:a->b) (grid:[h][w]a):[h][w]b =
    map (\(row: [w]a): [w]b ->
           map (\(x:a):b ->
                  f x
               ) row
        ) grid

let map_3d 'a 'b [h][w][j] (f:a->b) (grid:[h][w][j]a):[h][w][j]b =
    map (\(row: [w][j]a): [w][j]b ->
        map (\(cell:[j]a):[j]b ->
            map (\(x:a):b ->
                  f x
            ) cell
        ) row
    ) grid

let map2_2d 'a 'b 'c [h][w] (f:a->b->c) (aGrid:[h][w]a) (bGrid:[h][w]b):[h][w]c =
    map2 (\(aRow: [w]a) (bRow: [w]b) : [w]c ->
           map2 (\(x:a) (y:b) : c ->
                  f x y
               ) aRow bRow
        ) aGrid bGrid

let map2_3d 'a 'b 'c [h][w][j] (f:a->b->c) (aGrid:[h][w][j]a) (bGrid:[h][w][j]b):[h][w][j]c =
    map2 (\(aRow: [w][j]a) (bRow: [w][j]b) : [w][j]c ->
           map2 (\(aCell:[j]a) (bCell:[j]b) :[j]c ->
                map2 (\(x:a) (y:b) : c ->
                       f x y
                    ) aCell bCell
              )  aRow bRow
        ) aGrid bGrid

let map3_2d 'a 'b 'c 'd [h][w]
            (f:a->b->c->d)
            (aGrid:[h][w]a)
            (bGrid:[h][w]b)
            (cGrid:[h][w]c)
            :[h][w]d =
    map3 (\(aRow: [w]a) (bRow: [w]b) (cRow: [w]c) : [w]d ->
           map3 (\(x:a) (y:b) (z:c): d ->
                  f x y z
               ) aRow bRow cRow
        ) aGrid bGrid cGrid

let replicate_2d 't
                 (h:i64)
                 (w:i64)
                 (x:t):
                 [h][w]t =
    -- tabulate_2d h w (\_ _ -> x)
    replicate h (replicate w x)

let replicate_3d 't
                 (h:i64)
                 (w:i64)
                 (d:i64)
                 (x:t):
                 [h][w][d]t =
    -- tabulate_3d h w d (\_ _ _ -> x)
    replicate h (replicate_2d w d x)

let tupleOp1 't (f:t->t) (a:(t,t,t,t)):(t,t,t,t) =
    let (a0, a1, a2, a3) = a
    in  (f a0, f a1, f a2, f a3)

let tupleOp2 't (f:t->t->t) (a:(t,t,t,t)) (b:(t,t,t,t)):(t,t,t,t) =
    let (a0, a1, a2, a3) = a
    let (b0, b1, b2, b3) = b
    in  (f a0 b0, f a1 b1, f a2 b2, f a3 b3)

let reduceTuple1 't (f:t->t->t) (a:t,b:t,c:t,d:t):t =
    f a (f b (f c d))

let zipWith [n] 'a 'b 'c
             (f:a->b->c)
             (as: [n]a)
             (bs: [n]b):
                  [n]c =
  map (uncurry f) (zip as bs)

let zipWith3 [n] 'a 'b 'c 'd
             (f:a->b->c->d)
             (as: [n]a)
             (bs: [n]b)
             (cs: [n]c):
                  [n]d =
  map (\(a,(b,c)) -> f a b c) (zip as (zip2 bs cs))


let tabulate_4d 'a (n: i64) (m: i64) (o: i64) (p:i64) (f: i64 -> i64 -> i64 -> i64 -> a): *[n][m][o][p]a =
  map1 (f >-> tabulate_3d m o p) (iota n)

let tabulate_5d 'a (n: i64) (m: i64) (o: i64) (p:i64) (q:i64) (f: i64 -> i64 -> i64 -> i64 -> i64 -> a): *[n][m][o][p][q]a =
  map1 (f >-> tabulate_4d m o p q) (iota n)

let overwrite 't [g][s] (new:[s]t) (old:[g]t):[g]t =
    tabulate g (\i -> if i < s then new[i] else old[i])

let overwrite_columns 't [g][s][b] (new:[b][s]t) (old:[b][g]t):[b][g]t =
    tabulate_2d b g (\bs i -> if i < s then new[bs,i] else old[bs,i])

let take_columns 't [h][w] (i:i64) (xs:[h][w]t):([h][i]t) =
    tabulate_2d h i (\row col->xs[row,col])

let overwrite_2d 't [g][s][b] (new:[s][b]t) (old:[g][b]t):[g][b]t =
    tabulate_2d g b (\i bs -> if i < s then new[i,bs] else old[i,bs])

let unzip_2d 'a 'b [h][w] (frame:[h][w](a,b)):([h][w]a,[h][w]b) =
    (map_2d fst frame, map_2d snd frame)

let zip_2d 'a 'b [h][w] (xs:[h][w]a) (ys:[h][w]b):[h][w](a,b) =
    tabulate_2d h w (\row column -> (xs[row,column], ys[row,column]))

let zipWith_2d 'a 'b 'c [h][w] (f:a->b->c) (as:[h][w]a) (bs:[h][w]b):[h][w]c =
    tabulate_2d h w (\row col -> f as[row,col] bs[row,col])

let zipWith_3d 'a 'b 'c [h][w][d] (f:a->b->c) (as:[h][w][d]a) (bs:[h][w][d]b):[h][w][d]c =
    tabulate_3d h w d (\row col elem -> f as[row,col,elem] bs[row,col,elem])

let flatten_3d_to 't [h][w][d] (l:i64) (xs:[h][w][d]t):[l]t =
    flatten_3d xs :> [l]t

let flatten_4d_to 't [h][w][d][e] (l:i64) (xs:[h][w][d][e]t):[l]t =
    flatten_4d xs :> [l]t

let matmul [n][m][p] 'a
           (add: a -> a -> a) (mul: a -> a -> a) (zero: a)
           (A: [n][m]a) (B: [m][p]a) : [n][p]a =
  map (\A_row ->
         map (\B_col ->
                reduce add zero (map2 mul A_row B_col))
             (transpose B))
      A
