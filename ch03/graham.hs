--考虑三个二维的点 a, b，和c．如果我们观察沿着线段ＡＢ（由a,b节点组成）和线段ＢＣ（由b,c节点组成）形成的角度，它或者转向（turn）左边，或者转向右边，或者组成一条直线．
import Data.List
import Data.Ord(comparing)
import Data.Graph.Inductive.Internal.Heap(heapsort)
import Debug.Trace (traceShow)
data Vector2d = Vector2d {
                  x::  Double,
                  y::  Double
                }  
                deriving(Show,Read)

--定义一个 Direction（方向）的数据类型反映这些可能的情况．

data Direction = LEFT | RIGHT | STRAIGHT
                 deriving(Show,Eq,Read)
-- 
directionOf  v1 v2 v3 
             | ccw == 0 = STRAIGHT
             | ccw > 0  = LEFT
             | ccw < 0  = RIGHT
      where  ccw = (x v2 - x v1) * ( y v3 - y v1) - (y v2 - y v1) * (x v3- x v1)



--To get lowest Y
sortByY        = sortBy (\(Vector2d x1 _) (Vector2d x2 _) -> compare x1 x2)
               . sortBy (\(Vector2d _ y1) (Vector2d _ y2) -> compare y1 y2)

slope v1 v2 = (y v1   - y v2) / (x v1 - x v2)

lowest xs= head (sortByY xs)

sortBySlope vs = lowest: (sortBy cp  (tail ordered) )
            where cp v1 v2 = compare  (slope  lowest v1) (slope lowest v2)
                  ordered= sortByY vs
                  lowest = head ordered

grahamScan  xs 
            =  scan ( (sortByDP xs ))
          where scan (v1:v2:v3:vs)
                  | directionOf v1 v2 v3 == LEFT = v1 : scan(v2:v3:vs)
                  | otherwise  = scan(v1:v3:vs)
                scan vs =vs
-- /*
--     6    |       d
--     5    |     b   g
--     4    |   a   e   i
--     3    |     c   h
--     2    |       f
--     1    |
--     0    '------------
--     -1
--     -2
--       -1 0 1 2 3 4 5 6
-- */
     
a =   Vector2d 2 4 
b =   Vector2d 3 5 
c =   Vector2d 3 3 
d =   Vector2d 4 6 
e =   Vector2d 4 4 
f =   Vector2d 4 2 
g =   Vector2d 5 5 
h =   Vector2d 5 3 
i =   Vector2d 6 4 
vectors = [
            a,b,c,d,e,f,g,h,i
          ]            



--  
--             6    |
--             5    |
--             4    |
--             3    |
--             2    g   a
--             1    f b
--             0    c-e-d--------
--            -1
--            -2
--               -1 0 1 2 3 4 5 6
--        
a1 =  Vector2d 2 2 
b1 =  Vector2d 1 1 
c1 =  Vector2d 0 0 
d1 =  Vector2d 2 0 
e1 =  Vector2d 1 0 
f1 =  Vector2d 0 1 
g1 =  Vector2d 0 2 
  
vectors1 = [a1,b1,c1,d1,e1,f1,g1]



--  
--             9       |             d
--             8       |               c
--             7       |                 e
--             6       |
--             5       |
--             4       |       b
--             3       |
--             2       |
--             1   h   | a
--             0     g '------------------
--            -1       f
--            -2
--               -2 -1 0 1 2 3 4 5 6 7 8 9
--         
ta = Vector2d 1 1
tb = Vector2d 4 4
tc = Vector2d 8 8
td = Vector2d 7 9
te = Vector2d 9 7
tf = Vector2d 0 (-1)
tg = Vector2d (-1) 0
th = Vector2d (-2) 1
t3= [ta,tb,tc,te,td,tf,tg,th]
