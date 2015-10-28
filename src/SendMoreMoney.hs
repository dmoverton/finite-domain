module SendMoreMoney (sendMoreMoney) where

import Control.Monad (guard)
import FD

sendMoreMoney = runFD $ do
    vars@[s, e, n, d, m, o, r, y] <- newVars 8 (0, 9)
    let
        n0 = 0::Int
        n10 = 10::Int
        n100 = 100::Int
        n1000 = 1000::Int
        n10000 = 1000::Int
    s #\= n0
    m #\= n0
    allDifferent vars

    n1000 #* s #+ n100 #* e #+ n10 #* n #+ d
       #+ n1000 #* m #+ n100 #* o #+ n10 #* r #+ e
       #== n10000 #* m #+ n1000 #* o #+ n100 #* n #+ n10 #* e #+ y

    labelling vars

bruteForce :: [[Int]]
bruteForce = do
    s <- [1..9]
    e <- [0..9]
    n <- [0..9]
    d <- [0..9]
    let m = 1
    o <- [0..9]
    r <- [0..9]
    y <- [0..9]
    guard (s /= e)
    guard (s /= n)
    guard (s /= d)
    guard (s /= m)
    guard (s /= o)
    guard (s /= r)
    guard (s /= y)
    guard (e /= n)
    guard (e /= d)
    guard (e /= m)
    guard (e /= o)
    guard (e /= r)
    guard (e /= y)
    guard (n /= d)
    guard (n /= m)
    guard (n /= o)
    guard (n /= r)
    guard (n /= y)
    guard (d /= m)
    guard (d /= o)
    guard (d /= r)
    guard (d /= y)
    guard (m /= o)
    guard (m /= r)
    guard (m /= y)
    guard (o /= r)
    guard (o /= y)
    guard (r /= y)
    guard (1000*s + 100*e + 10*n + d + 1000*m + 100*o + 10*r + e ==
        10000*m + 1000*o + 100*n + 10*e + y)
    return [s,e,n,d,m,o,r,y]
