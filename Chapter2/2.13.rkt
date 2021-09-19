#lang racket

Let a be the center of the interval x and ta the tolerance.
Let b be the center of the interval y and tb the tolerance.

x = [a - ta, a + ta]
y = [b - tb, b + tb]

xy = [(a - ta)(b - tb), (a + ta)(b + tb)]

p_xy = w/c

        ab + atb bta + tatb - ab + atb + bta - tatb
p_xy = ---------------------------------------------
        ab - atb -bta + tatb + ab + atb + bta +tatb


   2atb + 2bta 
= -------------
   2ab  + 2tatb

which can be simplified to

   atb + bta
= ----------- * 100
   ab + tatb

ta = (a * pa/100)
tb = (b * pb/100)


hence


 ab* pb/100 + ba *pa/100 
------------------------- * 100
 ab + a * pa/100 * b * pb/100

 which can be simplified to


 ab * (pb/100 + pa/100)
------------------------ * 100
 ab * (1 + papb/10000)

and further

     (pb + pa)
    ------------
        100
------------------------ * 100
   1 + papb
      -----
      10000

as papa/10000 is very small we ignore them

thus

p_xy = pa + pb





