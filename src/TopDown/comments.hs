{-



==================
Starting!
Arguments: fromList [("arg0",a)]
Goal: b
==================

running dfs on <b> . <a> . (a -> b) at size 13
--------------------
current trace (remaining quota: 13): (GHC.List.head (?? :: alpha0))
sub = {
        alpha0 ==> [b] (size 2)
        tau0 ==> b (size 1)
        tau1 ==> b (size 1)
      } (size 2)

--------------------
current trace (remaining quota: 12): (GHC.List.head (GHC.List.head (?? :: alpha1)))
sub = {
        alpha0 ==> [b] (size 2)
        alpha1 ==> [[b]] (size 3)
        tau0 ==> b (size 1)
        tau1 ==> [b] (size 2)
        tau2 ==> [b] (size 2)
      } (size 5)

--------------------
current trace (remaining quota: 13): (GHC.List.head (GHC.List.head (?? :: alpha1)))
sub = {
        alpha0 ==> [b] (size 2)
        alpha1 ==> [[b]] (size 3)
        tau0 ==> b (size 1)
        tau1 ==> [b] (size 2)
        tau2 ==> [b] (size 2)
      } (size 5)

--------------------
current trace (remaining quota: 11): (GHC.List.head (GHC.List.head (GHC.List.head (?? :: alpha2))))
sub = {
        alpha0 ==> [b] (size 2)
        alpha1 ==> [[b]] (size 3)
        alpha2 ==> [[[b]]] (size 4)
        tau0 ==> b (size 1)
        tau1 ==> [b] (size 2)
        tau2 ==> [[b]] (size 3)
        tau3 ==> [[b]] (size 3)
      } (size 9)

--------------------
current trace (remaining quota: 12): (GHC.List.head (GHC.List.head (GHC.List.head (?? :: alpha2))))
sub = {
        alpha0 ==> [b] (size 2)
        alpha1 ==> [[b]] (size 3)
        alpha2 ==> [[[b]]] (size 4)
        tau0 ==> b (size 1)
        tau1 ==> [b] (size 2)
        tau2 ==> [[b]] (size 3)
        tau3 ==> [[b]] (size 3)
      } (size 9)

--------------------
current trace (remaining quota: 13): (GHC.List.head (GHC.List.head (GHC.List.head (?? :: alpha2))))
sub = {
        alpha0 ==> [b] (size 2)
        alpha1 ==> [[b]] (size 3)
        alpha2 ==> [[[b]]] (size 4)
        tau0 ==> b (size 1)
        tau1 ==> [b] (size 2)
        tau2 ==> [[b]] (size 3)
        tau3 ==> [[b]] (size 3)
      } (size 9)

--------------------
current trace (remaining quota: 11): (GHC.List.head (GHC.List.head (GHC.List.head (?? :: alpha2))))
sub = {
        alpha0 ==> [b] (size 2)
        alpha1 ==> [[b]] (size 3)
        alpha2 ==> [[[b]]] (size 4)
        tau0 ==> b (size 1)
        tau1 ==> [b] (size 2)
        tau2 ==> [[b]] (size 3)
        tau3 ==> [[b]] (size 3)
      } (size 9)

--------------------
current trace (remaining quota: 11): (GHC.List.head ((GHC.List.head (?? :: alpha2)) (?? :: alpha1)))
sub = {
        alpha0 ==> [b] (size 2)
        alpha2 ==> [alpha1 -> [b]] (size 4)
        tau0 ==> b (size 1)
        tau1 ==> alpha1 -> [b] (size 3)
        tau2 ==> alpha1 -> [b] (size 3)
      } (size 7)

--------------------
current trace (remaining quota: 12): (GHC.List.head (GHC.List.head [] (?? :: alpha1)))
sub = {
        alpha0 ==> [b] (size 2)
        alpha1 ==> a (size 1)
        alpha2 ==> [a -> [b]] (size 4)
        tau0 ==> b (size 1)
        tau1 ==> a -> [b] (size 3)
        tau2 ==> a -> [b] (size 3)
      } (size 7)

--------------------
current trace (remaining quota: 13): (GHC.List.head (GHC.List.head [] (?? :: alpha1)))
sub = {
        alpha0 ==> [b] (size 2)
        alpha1 ==> a (size 1)
        alpha2 ==> [a -> [b]] (size 4)
        tau0 ==> b (size 1)
        tau1 ==> a -> [b] (size 3)
        tau2 ==> a -> [b] (size 3)
      } (size 7)

--------------------
current trace (remaining quota: 12): (GHC.List.head (GHC.List.head [] (?? :: alpha1)))
sub = {
        alpha0 ==> [b] (size 2)
        alpha1 ==> [tau3] (size 2)
        alpha2 ==> [[tau3] -> [b]] (size 5)
        tau0 ==> b (size 1)
        tau1 ==> [tau3] -> [b] (size 4)
        tau2 ==> [tau3] -> [b] (size 4)
      } (size 9)

--------------------
current trace (remaining quota: 13): (GHC.List.head (GHC.List.head [] (?? :: alpha1)))
sub = {
        alpha0 ==> [b] (size 2)
        alpha1 ==> [tau3] (size 2)
        alpha2 ==> [[tau3] -> [b]] (size 5)
        tau0 ==> b (size 1)
        tau1 ==> [tau3] -> [b] (size 4)
        tau2 ==> [tau3] -> [b] (size 4)
      } (size 9)

--------------------
current trace (remaining quota: 11): (GHC.List.head ((GHC.List.head (?? :: alpha2)) (?? :: alpha1)))
sub = {
        alpha0 ==> [b] (size 2)
        alpha2 ==> [alpha1 -> [b]] (size 4)
        tau0 ==> b (size 1)
        tau1 ==> alpha1 -> [b] (size 3)
        tau2 ==> alpha1 -> [b] (size 3)
      } (size 7)

--------------------
current trace (remaining quota: 12): (GHC.List.head (GHC.List.head (?? :: alpha1)))
sub = {
        alpha0 ==> [b] (size 2)
        alpha1 ==> [[b]] (size 3)
        tau0 ==> b (size 1)
        tau1 ==> [b] (size 2)
        tau2 ==> [b] (size 2)
      } (size 5)

--------------------
current trace (remaining quota: 12): (GHC.List.head (GHC.List.head (?? :: alpha1)))
sub = {
        alpha0 ==> [b] (size 2)
        alpha1 ==> [[b]] (size 3)
        alpha2 ==> [[[b]]] (size 4)
        tau0 ==> b (size 1)
        tau1 ==> [b] (size 2)
        tau2 ==> [[b]] (size 3)
        tau3 ==> [[b]] (size 3)
      } (size 9)

--------------------
current trace (remaining quota: 12): (GHC.List.head (GHC.List.head [] (?? :: alpha1)))
sub = {
        alpha0 ==> [b] (size 2)
        alpha1 ==> a (size 1)
        alpha2 ==> [a -> [b]] (size 4)
        tau0 ==> b (size 1)
        tau1 ==> a -> [b] (size 3)
        tau2 ==> a -> [b] (size 3)
      } (size 7)

--------------------
current trace (remaining quota: 12): (GHC.List.head (GHC.List.head [] (?? :: alpha1)))
sub = {
        alpha0 ==> [b] (size 2)
        alpha1 ==> [tau3] (size 2)
        alpha2 ==> [[tau3] -> [b]] (size 5)
        tau0 ==> b (size 1)
        tau1 ==> [tau3] -> [b] (size 4)
        tau2 ==> [tau3] -> [b] (size 4)
      } (size 9)

--------------------
current trace (remaining quota: 12): ((GHC.List.head (?? :: alpha1)) (?? :: alpha0))
sub = {
        alpha1 ==> [alpha0 -> b] (size 3)
        tau0 ==> alpha0 -> b (size 2)
        tau1 ==> alpha0 -> b (size 2)
      } (size 4)

--------------------
current trace (remaining quota: 13): (GHC.List.head [] (?? :: alpha0))
sub = {
        alpha0 ==> a (size 1)
        alpha1 ==> [a -> b] (size 3)
        tau0 ==> a -> b (size 2)
        tau1 ==> a -> b (size 2)
      } (size 4)

--------------------
current trace (remaining quota: 13): (GHC.List.head [] (?? :: alpha0))
sub = {
        alpha0 ==> [tau2] -> tau2 (size 3)
        alpha1 ==> [([tau2] -> tau2) -> b] (size 5)
        tau0 ==> ([tau2] -> tau2) -> b (size 4)
        tau1 ==> ([tau2] -> tau2) -> b (size 4)
      } (size 8)

--------------------
current trace (remaining quota: 13): (GHC.List.head [] (?? :: alpha0))
sub = {
        alpha0 ==> [tau2] (size 2)
        alpha1 ==> [[tau2] -> b] (size 4)
        tau0 ==> [tau2] -> b (size 3)
        tau1 ==> [tau2] -> b (size 3)
      } (size 6)

--------------------
current trace (remaining quota: 11): (GHC.List.head [] (GHC.List.head (?? :: alpha2)))
sub = {
        alpha1 ==> [alpha0 -> b] (size 3)
        alpha2 ==> [alpha0] (size 2)
        tau0 ==> alpha0 -> b (size 2)
        tau1 ==> alpha0 -> b (size 2)
        tau2 ==> alpha0 (size 1)
        tau3 ==> alpha0 (size 1)
      } (size 6)

--------------------
current trace (remaining quota: 13): (GHC.List.head [] (GHC.List.head (?? :: alpha2)))
sub = {
        alpha1 ==> [alpha0 -> b] (size 3)
        alpha2 ==> [alpha0] (size 2)
        tau0 ==> alpha0 -> b (size 2)
        tau1 ==> alpha0 -> b (size 2)
        tau2 ==> alpha0 (size 1)
        tau3 ==> alpha0 (size 1)
      } (size 6)

--------------------
current trace (remaining quota: 10): (GHC.List.head [] ((GHC.List.head (?? :: alpha3)) (?? :: alpha2)))
sub = {
        alpha1 ==> [alpha0 -> b] (size 3)
        alpha3 ==> [alpha2 -> alpha0] (size 3)
        tau0 ==> alpha0 -> b (size 2)
        tau1 ==> alpha0 -> b (size 2)
        tau2 ==> alpha2 -> alpha0 (size 2)
        tau3 ==> alpha2 -> alpha0 (size 2)
      } (size 8)

--------------------
current trace (remaining quota: 11): (GHC.List.head [] (GHC.List.head [] (?? :: alpha2)))
sub = {
        alpha1 ==> [alpha0 -> b] (size 3)
        alpha2 ==> a (size 1)
        alpha3 ==> [a -> alpha0] (size 3)
        tau0 ==> alpha0 -> b (size 2)
        tau1 ==> alpha0 -> b (size 2)
        tau2 ==> a -> alpha0 (size 2)
        tau3 ==> a -> alpha0 (size 2)
      } (size 8)

--------------------
current trace (remaining quota: 13): (GHC.List.head [] (GHC.List.head [] (?? :: alpha2)))
sub = {
        alpha1 ==> [alpha0 -> b] (size 3)
        alpha2 ==> a (size 1)
        alpha3 ==> [a -> alpha0] (size 3)
        tau0 ==> alpha0 -> b (size 2)
        tau1 ==> alpha0 -> b (size 2)
        tau2 ==> a -> alpha0 (size 2)
        tau3 ==> a -> alpha0 (size 2)
      } (size 8)

--------------------
current trace (remaining quota: 10): (GHC.List.head [] ((GHC.List.head (?? :: alpha3)) (?? :: alpha2)))
sub = {
        alpha1 ==> [alpha0 -> b] (size 3)
        alpha3 ==> [alpha2 -> alpha0] (size 3)
        tau0 ==> alpha0 -> b (size 2)
        tau1 ==> alpha0 -> b (size 2)
        tau2 ==> alpha2 -> alpha0 (size 2)
        tau3 ==> alpha2 -> alpha0 (size 2)
      } (size 8)

--------------------
current trace (remaining quota: 11): (GHC.List.head [] (GHC.List.head (?? :: alpha2)))
sub = {
        alpha1 ==> [alpha0 -> b] (size 3)
        alpha2 ==> [alpha0] (size 2)
        tau0 ==> alpha0 -> b (size 2)
        tau1 ==> alpha0 -> b (size 2)
        tau2 ==> alpha0 (size 1)
        tau3 ==> alpha0 (size 1)
      } (size 6)

--------------------
current trace (remaining quota: 11): (GHC.List.head [] (GHC.List.head [] (?? :: alpha2)))
sub = {
        alpha1 ==> [alpha0 -> b] (size 3)
        alpha2 ==> a (size 1)
        alpha3 ==> [a -> alpha0] (size 3)
        tau0 ==> alpha0 -> b (size 2)
        tau1 ==> alpha0 -> b (size 2)
        tau2 ==> a -> alpha0 (size 2)
        tau3 ==> a -> alpha0 (size 2)
      } (size 8)

--------------------
current trace (remaining quota: 11): ((GHC.List.head (GHC.List.head (?? :: alpha2))) (?? :: alpha0))
sub = {
        alpha1 ==> [alpha0 -> b] (size 3)
        alpha2 ==> [[alpha0 -> b]] (size 4)
        tau0 ==> alpha0 -> b (size 2)
        tau1 ==> [alpha0 -> b] (size 3)
        tau2 ==> [alpha0 -> b] (size 3)
      } (size 8)

--------------------
current trace (remaining quota: 12): ((GHC.List.head (GHC.List.head (?? :: alpha2))) (?? :: alpha0))
sub = {
        alpha1 ==> [alpha0 -> b] (size 3)
        alpha2 ==> [[alpha0 -> b]] (size 4)
        tau0 ==> alpha0 -> b (size 2)
        tau1 ==> [alpha0 -> b] (size 3)
        tau2 ==> [alpha0 -> b] (size 3)
      } (size 8)

--------------------
current trace (remaining quota: 13): (GHC.List.head (GHC.List.head []) (?? :: alpha0))
sub = {
        alpha0 ==> a (size 1)
        alpha1 ==> [a -> b] (size 3)
        alpha2 ==> [[a -> b]] (size 4)
        tau0 ==> a -> b (size 2)
        tau1 ==> [a -> b] (size 3)
        tau2 ==> [a -> b] (size 3)
      } (size 8)

--------------------
current trace (remaining quota: 11): ((GHC.List.head (GHC.List.head (?? :: alpha2))) (?? :: alpha0))
sub = {
        alpha1 ==> [alpha0 -> b] (size 3)
        alpha2 ==> [[alpha0 -> b]] (size 4)
        tau0 ==> alpha0 -> b (size 2)
        tau1 ==> [alpha0 -> b] (size 3)
        tau2 ==> [alpha0 -> b] (size 3)
      } (size 8)

--------------------
current trace (remaining quota: 11): (((GHC.List.head (?? :: alpha2)) (?? :: alpha1)) (?? :: alpha0))
sub = {
        alpha2 ==> [alpha1 -> alpha0 -> b] (size 4)
        tau0 ==> alpha1 -> alpha0 -> b (size 3)
        tau1 ==> alpha1 -> alpha0 -> b (size 3)
      } (size 6)

--------------------
current trace (remaining quota: 12): ((GHC.List.head [] (?? :: alpha1)) (?? :: alpha0))
sub = {
        alpha1 ==> a (size 1)
        alpha2 ==> [a -> alpha0 -> b] (size 4)
        tau0 ==> a -> alpha0 -> b (size 3)
        tau1 ==> a -> alpha0 -> b (size 3)
      } (size 6)

--------------------
current trace (remaining quota: 13): (GHC.List.head [] arg0 (?? :: alpha0))
sub = {
        alpha0 ==> a (size 1)
        alpha1 ==> a (size 1)
        alpha2 ==> [a -> a -> b] (size 4)
        tau0 ==> a -> a -> b (size 3)
        tau1 ==> a -> a -> b (size 3)
      } (size 6)

--------------------
current trace (remaining quota: 13): (GHC.List.head [] arg0 (?? :: alpha0))
sub = {
        alpha0 ==> [tau2] (size 2)
        alpha1 ==> a (size 1)
        alpha2 ==> [a -> [tau2] -> b] (size 5)
        tau0 ==> a -> [tau2] -> b (size 4)
        tau1 ==> a -> [tau2] -> b (size 4)
      } (size 8)

--------------------
current trace (remaining quota: 10): (GHC.List.head [] arg0 (GHC.List.head (?? :: alpha3)))
sub = {
        alpha1 ==> a (size 1)
        alpha2 ==> [a -> alpha0 -> b] (size 4)
        alpha3 ==> [alpha0] (size 2)
        tau0 ==> a -> alpha0 -> b (size 3)
        tau1 ==> a -> alpha0 -> b (size 3)
        tau2 ==> alpha0 (size 1)
        tau3 ==> alpha0 (size 1)
      } (size 8)

--------------------
current trace (remaining quota: 13): (GHC.List.head [] arg0 (GHC.List.head (?? :: alpha3)))
sub = {
        alpha1 ==> a (size 1)
        alpha2 ==> [a -> alpha0 -> b] (size 4)
        alpha3 ==> [alpha0] (size 2)
        tau0 ==> a -> alpha0 -> b (size 3)
        tau1 ==> a -> alpha0 -> b (size 3)
        tau2 ==> alpha0 (size 1)
        tau3 ==> alpha0 (size 1)
      } (size 8)

--------------------
current trace (remaining quota: 10): (GHC.List.head [] arg0 (GHC.List.head (?? :: alpha3)))
sub = {
        alpha1 ==> a (size 1)
        alpha2 ==> [a -> alpha0 -> b] (size 4)
        alpha3 ==> [alpha0] (size 2)
        tau0 ==> a -> alpha0 -> b (size 3)
        tau1 ==> a -> alpha0 -> b (size 3)
        tau2 ==> alpha0 (size 1)
        tau3 ==> alpha0 (size 1)
      } (size 8)

--------------------
current trace (remaining quota: 12): ((GHC.List.head [] (?? :: alpha1)) (?? :: alpha0))
sub = {
        alpha1 ==> [tau2] (size 2)
        alpha2 ==> [[tau2] -> alpha0 -> b] (size 5)
        tau0 ==> [tau2] -> alpha0 -> b (size 4)
        tau1 ==> [tau2] -> alpha0 -> b (size 4)
      } (size 8)

--------------------
current trace (remaining quota: 13): (GHC.List.head [] [] (?? :: alpha0))
sub = {
        alpha0 ==> a (size 1)
        alpha1 ==> [tau2] (size 2)
        alpha2 ==> [[tau2] -> a -> b] (size 5)
        tau0 ==> [tau2] -> a -> b (size 4)
        tau1 ==> [tau2] -> a -> b (size 4)
      } (size 8)

--------------------
current trace (remaining quota: 10): ((GHC.List.head [] (GHC.List.head (?? :: alpha3))) (?? :: alpha0))
sub = {
        alpha2 ==> [alpha1 -> alpha0 -> b] (size 4)
        alpha3 ==> [alpha1] (size 2)
        tau0 ==> alpha1 -> alpha0 -> b (size 3)
        tau1 ==> alpha1 -> alpha0 -> b (size 3)
        tau2 ==> alpha1 (size 1)
        tau3 ==> alpha1 (size 1)
      } (size 8)

--------------------
current trace (remaining quota: 12): ((GHC.List.head [] (GHC.List.head (?? :: alpha3))) (?? :: alpha0))
sub = {
        alpha2 ==> [alpha1 -> alpha0 -> b] (size 4)
        alpha3 ==> [alpha1] (size 2)
        tau0 ==> alpha1 -> alpha0 -> b (size 3)
        tau1 ==> alpha1 -> alpha0 -> b (size 3)
        tau2 ==> alpha1 (size 1)
        tau3 ==> alpha1 (size 1)
      } (size 8)

--------------------
current trace (remaining quota: 13): (GHC.List.head [] (GHC.List.head []) (?? :: alpha0))
sub = {
        alpha0 ==> a (size 1)
        alpha2 ==> [alpha1 -> a -> b] (size 4)
        alpha3 ==> [alpha1] (size 2)
        tau0 ==> alpha1 -> a -> b (size 3)
        tau1 ==> alpha1 -> a -> b (size 3)
        tau2 ==> alpha1 (size 1)
        tau3 ==> alpha1 (size 1)
      } (size 8)

--------------------
current trace (remaining quota: 10): ((GHC.List.head [] (GHC.List.head (?? :: alpha3))) (?? :: alpha0))
sub = {
        alpha2 ==> [alpha1 -> alpha0 -> b] (size 4)
        alpha3 ==> [alpha1] (size 2)
        tau0 ==> alpha1 -> alpha0 -> b (size 3)
        tau1 ==> alpha1 -> alpha0 -> b (size 3)
        tau2 ==> alpha1 (size 1)
        tau3 ==> alpha1 (size 1)
      } (size 8)

--------------------
current trace (remaining quota: 10): ((((GHC.List.head (?? :: alpha3)) (?? :: alpha2)) (?? :: alpha1)) (?? :: alpha0))
sub = {
        alpha3 ==> [alpha2 -> alpha1 -> alpha0 -> b] (size 5)
        tau0 ==> alpha2 -> alpha1 -> alpha0 -> b (size 4)
        tau1 ==> alpha2 -> alpha1 -> alpha0 -> b (size 4)
      } (size 8)

--------------------
current trace (remaining quota: 11): (((GHC.List.head [] (?? :: alpha2)) (?? :: alpha1)) (?? :: alpha0))
sub = {
        alpha2 ==> a (size 1)
        alpha3 ==> [a -> alpha1 -> alpha0 -> b] (size 5)
        tau0 ==> a -> alpha1 -> alpha0 -> b (size 4)
        tau1 ==> a -> alpha1 -> alpha0 -> b (size 4)
      } (size 8)

--------------------
current trace (remaining quota: 12): ((GHC.List.head [] arg0 (?? :: alpha1)) (?? :: alpha0))
sub = {
        alpha1 ==> a (size 1)
        alpha2 ==> a (size 1)
        alpha3 ==> [a -> a -> alpha0 -> b] (size 5)
        tau0 ==> a -> a -> alpha0 -> b (size 4)
        tau1 ==> a -> a -> alpha0 -> b (size 4)
      } (size 8)

--------------------
current trace (remaining quota: 13): (GHC.List.head [] arg0 arg0 (?? :: alpha0))
sub = {
        alpha0 ==> a (size 1)
        alpha1 ==> a (size 1)
        alpha2 ==> a (size 1)
        alpha3 ==> [a -> a -> a -> b] (size 5)
        tau0 ==> a -> a -> a -> b (size 4)
        tau1 ==> a -> a -> a -> b (size 4)
      } (size 8)

--------------------
current trace (remaining quota: 10): ((((GHC.List.head (?? :: alpha3)) (?? :: alpha2)) (?? :: alpha1)) (?? :: alpha0))
sub = {
        alpha3 ==> [alpha2 -> alpha1 -> alpha0 -> b] (size 5)
        tau0 ==> alpha2 -> alpha1 -> alpha0 -> b (size 4)
        tau1 ==> alpha2 -> alpha1 -> alpha0 -> b (size 4)
      } (size 8)

--------------------
current trace (remaining quota: 11): (((GHC.List.head (?? :: alpha2)) (?? :: alpha1)) (?? :: alpha0))
sub = {
        alpha2 ==> [alpha1 -> alpha0 -> b] (size 4)
        tau0 ==> alpha1 -> alpha0 -> b (size 3)
        tau1 ==> alpha1 -> alpha0 -> b (size 3)
      } (size 6)

--------------------
current trace (remaining quota: 11): (((GHC.List.head [] (?? :: alpha2)) (?? :: alpha1)) (?? :: alpha0))
sub = {
        alpha2 ==> a (size 1)
        alpha3 ==> [a -> alpha1 -> alpha0 -> b] (size 5)
        tau0 ==> a -> alpha1 -> alpha0 -> b (size 4)
        tau1 ==> a -> alpha1 -> alpha0 -> b (size 4)
      } (size 8)

--------------------
current trace (remaining quota: 12): ((GHC.List.head (?? :: alpha1)) (?? :: alpha0))
sub = {
        alpha1 ==> [alpha0 -> b] (size 3)
        tau0 ==> alpha0 -> b (size 2)
        tau1 ==> alpha0 -> b (size 2)
      } (size 4)

--------------------
current trace (remaining quota: 12): ((GHC.List.head (?? :: alpha1)) (?? :: alpha0))
sub = {
        alpha1 ==> [alpha0 -> b] (size 3)
        alpha2 ==> [[alpha0 -> b]] (size 4)
        tau0 ==> alpha0 -> b (size 2)
        tau1 ==> [alpha0 -> b] (size 3)
        tau2 ==> [alpha0 -> b] (size 3)
      } (size 8)

--------------------
current trace (remaining quota: 12): ((GHC.List.head [] (?? :: alpha1)) (?? :: alpha0))
sub = {
        alpha1 ==> a (size 1)
        alpha2 ==> [a -> alpha0 -> b] (size 4)
        tau0 ==> a -> alpha0 -> b (size 3)
        tau1 ==> a -> alpha0 -> b (size 3)
      } (size 6)

--------------------
current trace (remaining quota: 12): ((GHC.List.head [] (?? :: alpha1)) (?? :: alpha0))
sub = {
        alpha1 ==> [tau2] (size 2)
        alpha2 ==> [[tau2] -> alpha0 -> b] (size 5)
        tau0 ==> [tau2] -> alpha0 -> b (size 4)
        tau1 ==> [tau2] -> alpha0 -> b (size 4)
      } (size 8)

--------------------
current trace (remaining quota: 12): ((GHC.List.head [] (?? :: alpha1)) (?? :: alpha0))
sub = {
        alpha2 ==> [alpha1 -> alpha0 -> b] (size 4)
        alpha3 ==> [alpha1] (size 2)
        tau0 ==> alpha1 -> alpha0 -> b (size 3)
        tau1 ==> alpha1 -> alpha0 -> b (size 3)
        tau2 ==> alpha1 (size 1)
        tau3 ==> alpha1 (size 1)
      } (size 8)

--------------------
current trace (remaining quota: 12): ((GHC.List.head [] arg0 (?? :: alpha1)) (?? :: alpha0))
sub = {
        alpha1 ==> a (size 1)
        alpha2 ==> a (size 1)
        alpha3 ==> [a -> a -> alpha0 -> b] (size 5)
        tau0 ==> a -> a -> alpha0 -> b (size 4)
        tau1 ==> a -> a -> alpha0 -> b (size 4)
      } (size 8)

--------------------
current trace (remaining quota: 13): (GHC.List.head (?? :: alpha0))
sub = {
        alpha0 ==> [b] (size 2)
        tau0 ==> b (size 1)
        tau1 ==> b (size 1)
      } (size 2)

--------------------
current trace (remaining quota: 13): (GHC.List.head (?? :: alpha0))
sub = {
        alpha0 ==> [b] (size 2)
        alpha1 ==> [[b]] (size 3)
        tau0 ==> b (size 1)
        tau1 ==> [b] (size 2)
        tau2 ==> [b] (size 2)
      } (size 5)

--------------------
current trace (remaining quota: 13): (GHC.List.head (?? :: alpha0))
sub = {
        alpha0 ==> [b] (size 2)
        alpha1 ==> [[b]] (size 3)
        alpha2 ==> [[[b]]] (size 4)
        tau0 ==> b (size 1)
        tau1 ==> [b] (size 2)
        tau2 ==> [[b]] (size 3)
        tau3 ==> [[b]] (size 3)
      } (size 9)

--------------------
current trace (remaining quota: 13): (GHC.List.head (?? :: alpha0))
sub = {
        alpha0 ==> [b] (size 2)
        alpha1 ==> a (size 1)
        alpha2 ==> [a -> [b]] (size 4)
        tau0 ==> b (size 1)
        tau1 ==> a -> [b] (size 3)
        tau2 ==> a -> [b] (size 3)
      } (size 7)

--------------------
current trace (remaining quota: 13): (GHC.List.head (?? :: alpha0))
sub = {
        alpha0 ==> [b] (size 2)
        alpha1 ==> [tau3] (size 2)
        alpha2 ==> [[tau3] -> [b]] (size 5)
        tau0 ==> b (size 1)
        tau1 ==> [tau3] -> [b] (size 4)
        tau2 ==> [tau3] -> [b] (size 4)
      } (size 9)

--------------------
current trace (remaining quota: 13): (GHC.List.head [] (?? :: alpha0))
sub = {
        alpha0 ==> a (size 1)
        alpha1 ==> [a -> b] (size 3)
        tau0 ==> a -> b (size 2)
        tau1 ==> a -> b (size 2)
      } (size 4)

--------------------
current trace (remaining quota: 13): (GHC.List.head [] (?? :: alpha0))
sub = {
        alpha0 ==> [tau2] -> tau2 (size 3)
        alpha1 ==> [([tau2] -> tau2) -> b] (size 5)
        tau0 ==> ([tau2] -> tau2) -> b (size 4)
        tau1 ==> ([tau2] -> tau2) -> b (size 4)
      } (size 8)

--------------------
current trace (remaining quota: 13): (GHC.List.head [] (?? :: alpha0))
sub = {
        alpha0 ==> [tau2] (size 2)
        alpha1 ==> [[tau2] -> b] (size 4)
        tau0 ==> [tau2] -> b (size 3)
        tau1 ==> [tau2] -> b (size 3)
      } (size 6)

--------------------
current trace (remaining quota: 13): (GHC.List.head [] (?? :: alpha0))
sub = {
        alpha1 ==> [alpha0 -> b] (size 3)
        alpha2 ==> [alpha0] (size 2)
        tau0 ==> alpha0 -> b (size 2)
        tau1 ==> alpha0 -> b (size 2)
        tau2 ==> alpha0 (size 1)
        tau3 ==> alpha0 (size 1)
      } (size 6)

--------------------
current trace (remaining quota: 13): (GHC.List.head [] (?? :: alpha0))
sub = {
        alpha1 ==> [alpha0 -> b] (size 3)
        alpha2 ==> a (size 1)
        alpha3 ==> [a -> alpha0] (size 3)
        tau0 ==> alpha0 -> b (size 2)
        tau1 ==> alpha0 -> b (size 2)
        tau2 ==> a -> alpha0 (size 2)
        tau3 ==> a -> alpha0 (size 2)
      } (size 8)

--------------------
current trace (remaining quota: 13): (GHC.List.head (GHC.List.head []) (?? :: alpha0))
sub = {
        alpha0 ==> a (size 1)
        alpha1 ==> [a -> b] (size 3)
        alpha2 ==> [[a -> b]] (size 4)
        tau0 ==> a -> b (size 2)
        tau1 ==> [a -> b] (size 3)
        tau2 ==> [a -> b] (size 3)
      } (size 8)

--------------------
current trace (remaining quota: 13): (GHC.List.head [] arg0 (?? :: alpha0))
sub = {
        alpha0 ==> a (size 1)
        alpha1 ==> a (size 1)
        alpha2 ==> [a -> a -> b] (size 4)
        tau0 ==> a -> a -> b (size 3)
        tau1 ==> a -> a -> b (size 3)
      } (size 6)

--------------------
current trace (remaining quota: 13): (GHC.List.head [] arg0 (?? :: alpha0))
sub = {
        alpha0 ==> [tau2] (size 2)
        alpha1 ==> a (size 1)
        alpha2 ==> [a -> [tau2] -> b] (size 5)
        tau0 ==> a -> [tau2] -> b (size 4)
        tau1 ==> a -> [tau2] -> b (size 4)
      } (size 8)

--------------------
current trace (remaining quota: 13): (GHC.List.head [] arg0 (?? :: alpha0))
sub = {
        alpha1 ==> a (size 1)
        alpha2 ==> [a -> alpha0 -> b] (size 4)
        alpha3 ==> [alpha0] (size 2)
        tau0 ==> a -> alpha0 -> b (size 3)
        tau1 ==> a -> alpha0 -> b (size 3)
        tau2 ==> alpha0 (size 1)
        tau3 ==> alpha0 (size 1)
      } (size 8)

--------------------
current trace (remaining quota: 13): (GHC.List.head [] [] (?? :: alpha0))
sub = {
        alpha0 ==> a (size 1)
        alpha1 ==> [tau2] (size 2)
        alpha2 ==> [[tau2] -> a -> b] (size 5)
        tau0 ==> [tau2] -> a -> b (size 4)
        tau1 ==> [tau2] -> a -> b (size 4)
      } (size 8)

--------------------
current trace (remaining quota: 13): (GHC.List.head [] (GHC.List.head []) (?? :: alpha0))
sub = {
        alpha0 ==> a (size 1)
        alpha2 ==> [alpha1 -> a -> b] (size 4)
        alpha3 ==> [alpha1] (size 2)
        tau0 ==> alpha1 -> a -> b (size 3)
        tau1 ==> alpha1 -> a -> b (size 3)
        tau2 ==> alpha1 (size 1)
        tau3 ==> alpha1 (size 1)
      } (size 8)

--------------------
current trace (remaining quota: 13): (GHC.List.head [] arg0 arg0 (?? :: alpha0))
sub = {
        alpha0 ==> a (size 1)
        alpha1 ==> a (size 1)
        alpha2 ==> a (size 1)
        alpha3 ==> [a -> a -> a -> b] (size 5)
        tau0 ==> a -> a -> a -> b (size 4)
        tau1 ==> a -> a -> a -> b (size 4)
      } (size 8)






















------------------
Type-based weights
------------------


The goal is to discourage the search from building programs
with unnaturally large types, like `head (head (head …))` or `fst (fst (fst
…))`

The main idea is to, conceptually, treat type parameters just like extra
parameters to a component and make type applications explicit in the terms.

E.g. you can think of a signature of a polymorphic function being: 
    
    `head :: a:Type -> [a] -> a` 
             Type -> [a] -> a

(it takes in a type, the a list of values of that type,
and returns a value of that type). Then if `xs :: Int`, then instead of
writing `head xs` you must write `head Int xs`. Hence the size of `head Int
xs` is now 3 and not 2. On the contrary, the size of `sum xs` is 2 (assuming
monomorphic `sum :: [Int] -> Int`).

        head xs   (size 2)   ==>   head Int xs  (size 3)

Now if we rewrite the unnatural term like `head (head (head []))`  for goal
type `Int` in this way, we get: 

    `head (head (head []))`   ==>
    `head Int (head [Int] (head [[Int]] ([] [[[Int]]])))`

Assuming size([T]) = 1 + size(T), the size of this term is now (1 + 1) + (1 +
2) + (1 + 3) + (1 + 4) = 14, so hopefully we will encounter this program much
later than a more reasonable program with 4 components.



      (head :: alpha -> Int) (?? :: alpha)
        (Int : Type) -> [Int] -> Int

  * for every forall in the type signature of the componenet (before unification)
      * look in sub to see if it is there
            * get the type of its sub and add that to regular size of program




Implementation: The `head` example from above is easy to implement: For the
outermost `head` your goal type is `alpha → Int`, so you immediately know
`alpha = [Int]`, and `head`s type param `a = Int`, so `size(a) = 1` and you
count the size of `head` as 1 + 1 = 2. For the next `head` the goal type is
`beta → [Int]`, so `beta = [[Int]]` and `a = [Int]` so the size of this head
is 1 + 2 = 3. And so on, and the same for [].

Challenge: The problem arises when you don’t immediately know the
instantiation of all type parameters when you pick a component. For example,
for a goal `beta → alpha → [Int]` and component: `map :: (a -> b) -> [a] ->
[b]`, you know what b is but not a, so you can’t immediate determine the size
of this `map` instance. 
          `map :: (a -> b) -> [a] -> [b]`
          `map :: forall a . forall b . (a -> b) -> [a] -> [b]`
          `beta → alpha → [Int]`
    unified:   forall a . (a -> Int) -> [a] -> [Int]
    size of this is ???????? 

One idea is to handle this just like you handle real
arguments, which are always unknown: i.e. partition the size budget a-priori.

For example, if my total budget is 10, and I picked `map` with `b = Int`, then
I used up 2 units (map and Int), and have 8 remaining units to distribute between 3 things
now: 

      1) the type to replace a           :: type of a
      2) the first argument to `map`     :: a -> Int
      3) the second argument to `map`.   :: [a]

If I decided to allocate, say, 1, 9, 1, then I start
enumerating functions of the type `a -> Int`, but I also have to keep in mind
that `a` is limited to size 1 (i.e. make it part of my spec, and the memo
key). Alternatively, I can a-priori enumerate all possible types of size 1 as
candidates for a; then I don’t have to modify the notion of goal and I gain a
nice property that all my I-goals will always be ground (have no free type
variables).

way 1a:
      dfs on `a -> Int`
      check what a unifies with after solving
      discard all programs that have a be something larger than 1

way 1b:
    typeA <- choices (all size 1 types)
    dfs on `typeA -> Int` like a normal person

On the flip side, enumerating all these types might be expensive.
One mitigation strategy is to limit the maximum type size to some small
heuristic bound (say 3 or 4). 

      ==> limit the size of a to be at max size 3 or 4

way 2:

Another approach is to enumerate first arg to
map disregarding the bound on type `a` and then take its size into account
later. You can think of it as switching the order of arguments to map to
something like: 

        ((?? :: beta → alpha → [Int]) (?? :: beta)) (?? :: alpha)
        `map :: forall a . forall b . (a -> b) -> [a] -> [b]`
        `map :: b:Type -> (a -> b) -> a: Type -> [a] -> [b]`, 
        
        first we look at [b] --> this decides b:Type
        then we look at (a->b) --> this decides a:Type
        then we look at [a]
        so the order ends up being
        `map :: b:Type -> (a -> b) -> a: Type -> [a] -> [b]`, 

        query: [Int] -> [Int]
        
        [Int]                                                         quota 10
            splits (?? :: alpha -> [Int]) (?? :: alpha)
        (?? :: alpha -> [Int])                                        quota 9
            slits (?? :: beta → alpha → [Int]) (?? :: beta)
        (?? :: beta → alpha → [Int])                                  quota 8
                  map :: forall a b. (a->b)->[a]->[b]
              ==> map (a:Type) (Int:Type)   size: 1 (map) + 1 (Int) + 1 (a)
                                                  3
                  beta ~ a -> Int
                  alpha ~ [a]
        (?? :: (a -> Int))                                            quota 9 - 3 = 6
                  inc :: Int->Int
              ==> inc :: (Int -> Int)       size: 1 (inc) + 0 for type of inc bc no forall's in it
        (?? :: [a])                                                   quota 9 - 3 - 1 = 5
                  arg0 :: [Int]
              ==> arg0 :: [Int]
                  a ~ Int                   size: 1 (arg0) + 0 for type of arg0 bc no forall's in it
                  (need to make sure a is size 1!!! because that's what we assumed previously)
        map   inc    arg0

questions:
    * how do we keep track of a having to be size 1???
          keep a map in our state :: String -> Int

where the a:Type argument is already determined by the choice of (a -> b), so
there’s only one choice for it, but the size accounting still works as usual
(we subtract its size from the total budget before looking for the [a]
argument). This actually shows that this strategy is not very different from
the previous strategy (where we explicitly enumerate type argos): in both, the
type arg is just treated as normal arg, it’s just the order of args is swapped
(which is something you can always control in your synthesizer!). Which one is
more efficient, essentially depends on which one constrains the search more
(ideally we’d like to try both).



Way 3:

Alternative approaches:  I think what you currently do with limiting the size
of the whole substitution is similar to limiting the size of type
instantiation to 3. But not quite because you also include the instantiations
for the `alpha`s, i.e. the type variables invented by the synthesizer in
E-mode, rather than those present in the components, and these instantiations
we don’t necessarily want to keep small, because they correspond to inherent
types of the components.

sub

tau2 ==> [tau1]
tau3 ==> tau1 -> tau1

tau1 ==> Int
tau2 ==> [Int]
tau3 ==> Int -> Int

sub

2 types of taus
    * ones that come from freshVars
            care about these
    * ones that come from alpha -> T & alpha
            don't care about these

E.g. for a components like `catMaybes :: [Maybe a] -> a`, 
we care to make `a` small, but we don’t necessarily care that it will be
matched with `alpha → Int` and then `alpha` has to be `[Maybe a]`, which is
relatively complex (size 3). This is not necessarily a bad thing: we are just
using a complex component, but we are not using it in a “complex” way. Well,
this is just my hunch, this is actually something that would be good to check:

  ==> do we want to discourage the synthesizer from using complex types 
    * in general (let’s call it metric A)?
          what we currently do with limiting size of sub
    * just using polymorphic components in complex ways (let’s call it metric B)?
          what she suggested above 

The approach above with explicit type applications implements metric B. (!!!!)
If you want to implement metric A, then one way to do this is to define the size of a
term to include the size of type at each node (or only at variable nodes?)

E.g. the size of `head [] :: Int` = 
                size(type(head)) + size(type([])) + size(type(whole thing))
              = size([?] -> Int) + size([?])      + size(Int) 
              = 4                + 2              + 1
              = 7.

Here I write ? for the type of list element because it remains
uninstantiated. I consider `size(?) = 1` but actually now that I think about
it: uninstantiated types should be discouraged or even disallowed out right!

Way 4: 

Side idea: prune programs with uninstantiated types. Similarly to how we prune
programs with unused arguments, perhaps we should prune programs with “unused
type argument” (i.e. uninstantiated type variables). This would outlaw
programs like `head []` (which is good), but also programs like `length []`,
which might be good or bad. Worth checking whether this technique would work
on our benchmarks?

    head :: [a] -> a
    Nil  :: <a> . [a]
    when they're unified, 

to check this:
    after we unify, check if the schema has any foralls left???





------------------------------------------------------------------------------------------------

questions!!!

Nadia quote: 

Hello! Nadia, we know that you are on vacation so we don't expect an answer until you get back. We wanted to message here for your return (and while our question is still fresh in our heads). 

We are trying to understand the document you gave us, specifically the following part of it: 

"Do we want to discourage the synthesizer from using complex types in general (let’s call it metric A)? or just using polymorphic components in complex ways (let’s call it metric B)?"

We want to make sure our understanding of the difference between Metric A and B are correct. 

* is Metric A congruent with our old definition of "size of program's type": add all the sizes of types assigned to every fresh type variable
* is Metric B congruent with the following definition of "size of program's type": add all the sizes of types assigned to every fresh type variable, *excluding* those alphas that come from splitting into `(?? :: alpha -> T) (?? :: alpha)`





TODO for the rest of the day:

- rewrite sizeOfProg to take our new sizeOfSub into acct
- test if that works (print out the size of programs while synthesizing "Maybe(a->b)->a->b")
- wrap up


------------------------------------------------------------------------------------------------


Now if we rewrite the unnatural term like `head (head (head []))`  for goal
type `Int` in this way, we get: 

    `head (head (head []))`   ==>
    `head Int (head [Int] (head [[Int]] ([] [[Int]])))`

    head (head (head [])) with...
    head :: <a> . [a] -> a
    head :: [tau0] -> tau0

    Nil :: <a> . [a]
    Nil :: tau3 . [tau3]

    (head :: alpha0 -> Int) (?? :: alpha0)
    (head :: [Int] -> Int) ((head :: alpha1 -> [Int]) (?? :: alpha1))
    (head :: [Int] -> Int) ((head :: [[Int]] -> [Int]) (head :: alpha2 -> [[Int]]) (?? :: alpha2)))
    (head :: [Int] -> Int) ((head :: [[Int]] -> [Int]) (head :: alpha2 -> [[Int]]) ([] :: [tau3])))
      alpha2 ~ [tau3]
      alpha2 ~ [[[Int]]]
      so tau3 ~ [[Int]]
    (head :: [Int] -> Int) ((head :: [[Int]] -> [Int]) (head :: alpha2 -> [[Int]]) ([] :: [[[Int]]])))
    

    alpha0 ==> [Int]
    alpha1 ==> nothing
    tau0 ==> Int        1
    tau1 ==> [Int]      2
    tau2 ==> [[Int]]    3
    tau3 ==> [[Int]]    3

    size of `head (head (head []))` is 4

    total = 13

synGuard "a -> b" ["Nil", "head"]



Assuming size([T]) = 1 + size(T), the size of this term is now (1 + 1) + (1 +
2) + (1 + 3) + (1 + 4) = 14, so hopefully we will encounter this program much
later than a more reasonable program with 4 components.



------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------

TODO later stuff
for after we finish stuff above

(can ignore for now since we're ignoring memo for now lol)
-- Memo and free variables in goals: A related thing to think about is how we
-- memoize goals with free type variables. One question we already discussed: we
-- might be missing out on memoized results because free type variables might be
-- too fresh. Another facet is: we might have a goal like `Int → Int` which is a
-- “sub-goal” of `alpha → Int` (i.e. the results for the former are a subset of
-- the results for the latter). Can we store our memo results in some kind of
-- lattice so that we can efficiently recall not only `alpha → Int` but also
-- results for all instantiations thereof? 


------------------------
Other sources of weights
------------------------

We can also try assigning unequal weights to components based on their
simplicity / popularity / partiality / efficiency etc. These weights could be
designed manually or automatically extracted from a corpus or from the
libraries themselves, for example: analyze some Haskell code and collect
frequencies of each component, and assign weights proportional to negative log
frequencies [David Justo started doing that at some point and even has some
data, you could talk to him] same as above but also collect frequencies of
type constructors, which will give us weights on types same as above but
collect frequencies of instances of polymorphic components, meaning we would
have a co-occurrence matrix of components and types. We might not have enough
data for this.





























HAIKU!!!!

                                  program synthesis        
                              "it's all algorithmic art?"  
                                "yep always has been"      

aaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbccc
aaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbccc
aaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbccc
aaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbccc
aaabbbccaaabbbccaaabbbccaaabbbccaaabbbccaaabbbccaaabbbccaaabbbccaaabbbccaaabbbccaaabbbccaaabbbccaaa
cccbbbaaacccbbbaaacccbbbaaacccbbbaaacccbbbaaacccbbbaaacccbbbaaacccbbbaaacccbbbaaacccbbbaaacccbbbaaa
aabbbcccaabbbcccaabbbcccaabbbcccaabbbcccaabbbcccaabbbcccaabbbcccaabbbcccaabbbcccaabbbcccaabbbcccaab
aaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbccc
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
aabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbccca
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
aaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbccc
aaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbccc
aaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbccc
aaabbbccaaabbbccaaabbbccaaabbbccaaabbbccaaabbbccaaabbbccaaabbbccaaabbbccaaabbbccaaabbbccaaabbbccaaa
cccbbbaaacccbbbaaacccbbbaaacccbbbaaacccbbbaaacccbbbaaacccbbbaaacccbbbaaacccbbbaaacccbbbaaacccbbbaaa
aabbbcccaabbbcccaabbbcccaabbbcccaabbbcccaabbbcccaabbbcccaabbbcccaabbbcccaabbbcccaabbbcccaabbbcccaab
aaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbccc
aaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbccc
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
aaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbccc
aaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbccc
aaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbccc
aaabbbccaaabbbccaaabbbccaaabbbccaaabbbccaaabbbccaaabbbccaaabbbccaaabbbccaaabbbccaaabbbccaaabbbccaaa
cccbbbaaacccbbbaaacccbbbaaacccbbbaaacccbbbaaacccbbbaaacccbbbaaacccbbbaaacccbbbaaacccbbbaaacccbbbaaa
aabbbcccaabbbcccaabbbcccaabbbcccaabbbcccaabbbcccaabbbcccaabbbcccaabbbcccaabbbcccaabbbcccaabbbcccaab
aaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbccc
aaabbbccaaabbbccaaabbbccaaabbbccaaabbbccaaabbbccaaabbbccaaabbbccaaabbbccaaabbbccaaabbbccaaabbbccaaa
aaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbccc
aaabbbcaaabbbcaaabbbcaaabbbcaaabbbcaaabbbcaaabbbcaaabbbcaaabbbcaaabbbcaaabbbcaaabbbcaaabbbcaaabbbca
cccbbbaacccbbbaacccbbbaacccbbbaacccbbbaacccbbbaacccbbbaacccbbbaacccbbbaacccbbbaacccbbbaacccbbbaaccc
aabbbccaabbbccaabbbccaabbbccaabbbccaabbbccaabbbccaabbbccaabbbccaabbbccaabbbccaabbbccaabbbccaabbbcca
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccbbbaaacccbbbaaacccbbbaaacccbbbaaacccbbbaaacccbbbaaacccbbbaaacccbbbaaacccbbbaaacccbbbaaacccbbbaaa
cccbbbaacccbbbaacccbbbaacccbbbaacccbbbaacccbbbaacccbbbaacccbbbaacccbbbaacccbbbaacccbbbaacccbbbaaccc
aabbbcccaabbbcccaabbbcccaabbbcccaabbbcccaabbbcccaabbbcccaabbbcccaabbbcccaabbbcccaabbbcccaabbbcccaab
aabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbcccaaabbbccca


































-------------------------------------------------------




indexesOf
synGuardO' "([(a,Int)] -> [(a,Int)]) -> [a] -> [Int] -> [Int]" ["GHC.List.map", "Data.Tuple.snd", "GHC.List.zip"] [(["map (\\(x, y) -> (x, y * y))", "[1,2,3]", "[9,8,7]"], "[81, 64, 49]")]
solution: "\\f xs ys -> Data.List.map Data.Tuple.snd (f (Data.List.zip xs ys))"

    (Quota 8) Done with <a> . ((([(a , Int)] -> [(a , Int)])) -> ([a] -> ([Int] -> [Int])))!
    size    subSize solution
    6       24      GHC.List.map Data.Tuple.snd (arg0 (GHC.List.zip arg1 arg2))
    -----------------
    --- BACKTRACE ---
    -----------------
    (?? :: [Int])
    (?? :: (tau0 -> [Int])) (?? :: tau0)
    (?? :: (tau1 -> (tau0 -> [Int]))) (?? :: tau1) (?? :: tau0)
    GHC.List.map (?? :: tau1) (?? :: tau0)
    GHC.List.map Data.Tuple.snd (?? :: tau0)
    GHC.List.map Data.Tuple.snd (?? :: (tau6 -> [(tau5 , Int)])) (?? :: tau6)
    GHC.List.map Data.Tuple.snd arg0 (?? :: tau6)
    GHC.List.map Data.Tuple.snd arg0 (?? :: (tau7 -> [(a , Int)])) (?? :: tau7)
    GHC.List.map Data.Tuple.snd arg0 (?? :: (tau8 -> (tau7 -> [(a , Int)]))) (?? :: tau8) (?? :: tau7)
    GHC.List.map Data.Tuple.snd arg0 GHC.List.zip (?? :: tau8) (?? :: tau7)
    GHC.List.map Data.Tuple.snd arg0 GHC.List.zip arg1 (?? :: tau7)
    GHC.List.map Data.Tuple.snd (arg0 (GHC.List.zip arg1 arg2))
    -----------------
    (5.45 secs, 1,671,473,336 bytes)

mergeEither
synGuardO' "Either a (Either a b) -> Either a b" ["Data.Either.either", ".Left", "Data.Either.either", ".Left", ".Right"] [(["Left 2"], "Left 2"), (["Right (Left 2)"], "Left 2"), (["Right (Right 2.2)"], "Right 2.2")]
synO' "Either a (Either a b) -> Either a b" [(["Left 2"], "Left 2"), (["Right (Left 2)"], "Left 2"), (["Right (Right 2.2)"], "Right 2.2")]
solution: "\\arg0 -> Data.Either.either Left (Data.Either.either Left Right) arg0"

    (Quota 8) Done with <b> . <a> . (Either (a) ((Either (a) (b))) -> Either (a) (b))!
    size    subSize solution
    5       24      Data.Either.either Data.Either.Left (\arg1 -> arg1) arg0
    -----------------
    --- BACKTRACE ---
    -----------------
    (?? :: Either (a) (b))
    (?? :: (tau0 -> Either (a) (b))) (?? :: tau0)
    (?? :: (tau1 -> (tau0 -> Either (a) (b)))) (?? :: tau1) (?? :: tau0)
    (?? :: (tau2 -> (tau1 -> (tau0 -> Either (a) (b))))) (?? :: tau2) (?? :: tau1) (?? :: tau0)
    Data.Either.either (?? :: tau2) (?? :: tau1) (?? :: tau0)
    Data.Either.either Data.Either.Left (?? :: tau1) (?? :: tau0)
    Data.Either.either Data.Either.Left (\arg1 -> (?? :: tau4)) (?? :: tau0)
    Data.Either.either Data.Either.Left (\arg1 -> arg1) (?? :: tau0)
    Data.Either.either Data.Either.Left (\arg1 -> arg1) arg0
    -----------------
    (3.63 secs, 979,275,584 bytes)

dedupe
synGuard' "Eq a => [a] -> [a]" ["GHC.List.map", "GHC.List.head", "Data.List.group"] [(["\"aaabbbccc\""], "\"abc\"")]
synGuardO' "Eq a => [a] -> [a]" ["GHC.List.map", "GHC.List.head", "Data.List.group", "@@hplusTC@@Eq"] [(["\"aaabbbccc\""], "\"abc\"")]
synO' "Eq a => [a] -> [a]"  [(["\"aaabbbccc\""], "\"abc\"")]
solution: "\\xs -> Data.List.map Data.List.head (Data.List.group xs)"
our expecteed solution: "\\arg0 -> Data.List.map (\arg1 -> Data.List.head arg1) (Data.List.group arg0)"
    | current goal (sizeQuota 1): GHC.List.map (\arg1 -> GHC.List.head arg0) (?? :: (tau6 -> [tau3])) (?? :: tau6)
    | current goal (sizeQuota 1): GHC.List.map (\arg1 -> GHC.List.head arg0) GHC.List.head (?? :: tau6) <-
    | current goal (sizeQuota 1): GHC.List.map (\arg1 -> GHC.List.head arg0) GHC.List.head (?? :: tau6)<-
    | current goal (sizeQuota 1): GHC.List.map (\arg1 -> GHC.List.head arg0) (?? :: (tau6 -> [tau3])) (?? :: tau6)
    | current goal (sizeQuota 2): GHC.List.map (\arg1 -> GHC.List.head arg0) (?? :: tau0)<-
    | current goal (sizeQuota 2): GHC.List.map (\arg1 -> GHC.List.head arg1) (?? :: tau0)<-
    | current goal (sizeQuota 1): GHC.List.map (\arg1 -> GHC.List.head arg1) (?? :: (tau6 -> [[a]])) (?? :: tau6)
    | current goal (sizeQuota 1): GHC.List.map (\arg1 -> GHC.List.head arg1) GHC.List.head (?? :: tau6)<-
    | current goal (sizeQuota 1): GHC.List.map (\arg1 -> GHC.List.head arg1) GHC.List.head (?? :: tau6)<-
    | current goal (sizeQuota 1): GHC.List.map (\arg1 -> GHC.List.head arg1) (?? :: (tau6 -> [[a]])) (?? :: tau6)
    | current goal (sizeQuota 2): GHC.List.map (\arg1 -> GHC.List.head arg1) (?? :: tau0)<-
    | current goal (sizeQuota 1): GHC.List.map (\arg1 -> GHC.List.head (?? :: (tau6 -> [a])) (?? :: tau6)) (?? :: tau0)
    | current goal (sizeQuota 1): GHC.List.map (\arg1 -> GHC.List.head arg1 (?? :: tau6)) (?? :: tau0)
    | current goal (sizeQuota 1): GHC.List.map (\arg1 -> GHC.List.head (arg1 arg0)) (?? :: tau0)
    | current goal (sizeQuota 1): GHC.List.map (\arg1 -> GHC.List.head (arg1 arg0)) (?? :: tau0)
    | current goal (sizeQuota 1): GHC.List.map (\arg1 -> GHC.List.head (arg1 tcarg0)) (?? :: tau0)
    | current goal (sizeQuota 1): GHC.List.map (\arg1 -> GHC.List.head (arg1 tcarg0)) (?? :: tau0)
    | current goal (sizeQuota 1): GHC.List.map (\arg1 -> GHC.List.head (arg1 GHC.List.head)) (?? :: tau0)
    | current goal (sizeQuota 1): GHC.List.map (\arg1 -> GHC.List.head (arg1 GHC.List.head)) (?? :: tau0)
    ---> loops and becomes unkillable

splitAtFirst
synGuardO' "a -> [a] -> ([a], [a])" ["GHC.List.splitAt", "Data.Maybe.fromMaybe", "GHC.List.elemIndex"] [(["1", "[2,3,1,4,1,5]"], "([2,3],[4,1,5])")]
solution: "\\x xs -> Data.List.splitAt (Data.Maybe.fromMaybe 0 (Data.List.elemIndex x xs)) xs"

    ---> doesn't seem to terminate (probably since solution contains 0)

mbElem
synGuardO' "Eq a => a -> [a] -> Maybe a" [".bool", ".Nothing", ".Just", "GHC.List.elem"] [(["2", "[1,3,5,7,9]"], "Nothing"), (["3", "[1,3,5,7,9]"], "Just 3")]
solution: "\\x xs -> bool Nothing (Just x) (GHC.List.elem x xs)"

    (Quota 8) Done with <a> . (@@hplusTC@@Eq (a) -> (a -> ([a] -> Maybe (a))))!
    size    subSize solution
    8       16      Data.Bool.bool Data.Maybe.Nothing (Data.Maybe.Just arg0) (GHC.List.elem tcarg0 arg0 arg1)
    -----------------
    --- BACKTRACE ---
    -----------------
    (?? :: Maybe (a))
    (?? :: (tau0 -> Maybe (a))) (?? :: tau0)
    (?? :: (tau1 -> (tau0 -> Maybe (a)))) (?? :: tau1) (?? :: tau0)
    (?? :: (tau2 -> (tau1 -> (tau0 -> Maybe (a))))) (?? :: tau2) (?? :: tau1) (?? :: tau0)
    Data.Bool.bool (?? :: tau2) (?? :: tau1) (?? :: tau0)
    Data.Bool.bool Data.Maybe.Nothing (?? :: tau1) (?? :: tau0)
    Data.Bool.bool Data.Maybe.Nothing (?? :: (tau5 -> Maybe (a))) (?? :: tau5) (?? :: tau0)
    Data.Bool.bool Data.Maybe.Nothing Data.Maybe.Just (?? :: tau5) (?? :: tau0)
    Data.Bool.bool Data.Maybe.Nothing (Data.Maybe.Just arg0) (?? :: tau0)
    Data.Bool.bool Data.Maybe.Nothing (Data.Maybe.Just arg0) (?? :: (tau7 -> Bool)) (?? :: tau7)
    Data.Bool.bool Data.Maybe.Nothing (Data.Maybe.Just arg0) (?? :: (tau8 -> (tau7 -> Bool))) (?? :: tau8) (?? :: tau7)
    Data.Bool.bool Data.Maybe.Nothing (Data.Maybe.Just arg0) (?? :: (tau9 -> (tau8 -> (tau7 -> Bool)))) (?? :: tau9) (?? :: tau8) (?? :: tau7)
    Data.Bool.bool Data.Maybe.Nothing (Data.Maybe.Just arg0) GHC.List.elem (?? :: tau9) (?? :: tau8) (?? :: tau7)
    Data.Bool.bool Data.Maybe.Nothing (Data.Maybe.Just arg0) GHC.List.elem tcarg0 (?? :: tau8) (?? :: tau7)
    Data.Bool.bool Data.Maybe.Nothing (Data.Maybe.Just arg0) GHC.List.elem tcarg0 arg0 (?? :: tau7)
    Data.Bool.bool Data.Maybe.Nothing (Data.Maybe.Just arg0) (GHC.List.elem tcarg0 arg0 arg1)
    -----------------
    (6.96 secs, 2,634,667,288 bytes)

areEq
synGuardO' "Eq a => a -> a -> Maybe a" [".bool", ".Nothing", ".Just", "=="] [(["1", "2"], "Nothing"), (["1", "1"], "Just 1")]
solution: "\\x y -> bool Nothing (Just x) ((==) x y)"

    (Quota 8) Done with <a> . (@@hplusTC@@Eq (a) -> (a -> (a -> Maybe (a))))!
    size    subSize solution
    8       15      Data.Bool.bool Data.Maybe.Nothing (Data.Maybe.Just arg0) (arg0 == arg1)
    -----------------
    --- BACKTRACE ---
    -----------------
    (?? :: Maybe (a))
    (?? :: (tau0 -> Maybe (a))) (?? :: tau0)
    (?? :: (tau1 -> (tau0 -> Maybe (a)))) (?? :: tau1) (?? :: tau0)
    (?? :: (tau2 -> (tau1 -> (tau0 -> Maybe (a))))) (?? :: tau2) (?? :: tau1) (?? :: tau0)
    Data.Bool.bool (?? :: tau2) (?? :: tau1) (?? :: tau0)
    Data.Bool.bool Data.Maybe.Nothing (?? :: tau1) (?? :: tau0)
    Data.Bool.bool Data.Maybe.Nothing (?? :: (tau5 -> Maybe (a))) (?? :: tau5) (?? :: tau0)
    Data.Bool.bool Data.Maybe.Nothing Data.Maybe.Just (?? :: tau5) (?? :: tau0)
    Data.Bool.bool Data.Maybe.Nothing (Data.Maybe.Just arg0) (?? :: tau0)
    Data.Bool.bool Data.Maybe.Nothing (Data.Maybe.Just arg0) (?? :: (tau7 -> Bool)) (?? :: tau7)
    Data.Bool.bool Data.Maybe.Nothing (Data.Maybe.Just arg0) (?? :: (tau8 -> (tau7 -> Bool))) (?? :: tau8) (?? :: tau7)
    Data.Bool.bool Data.Maybe.Nothing (Data.Maybe.Just arg0) (?? :: (tau9 -> (tau8 -> (tau7 -> Bool)))) (?? :: tau9) (?? :: tau8) (?? :: tau7)
    Data.Bool.bool Data.Maybe.Nothing (Data.Maybe.Just arg0) (Data.Eq.==) (?? :: tau9) (?? :: tau8) (?? :: tau7)
    Data.Bool.bool Data.Maybe.Nothing (Data.Maybe.Just arg0) (Data.Eq.==) tcarg0 (?? :: tau8) (?? :: tau7)
    Data.Bool.bool Data.Maybe.Nothing (Data.Maybe.Just arg0) (Data.Eq.==) tcarg0 arg0 (?? :: tau7)
    Data.Bool.bool Data.Maybe.Nothing (Data.Maybe.Just arg0) (arg0 == arg1)
    -----------------
    (30.57 secs, 11,791,510,528 bytes)

takeNdropM
synGuardO' "Int -> Int -> [a] -> ([a], [a])" ["GHC.List.take", "Pair", "GHC.List.drop"] [(["3", "5", "[1,2,3,4,5,6,7]"], "([1,2,3], [6,7])")]
solution: "\\n m l -> (Data.List.take n l, Data.List.drop m l)"

    (Quota 7) Done with <a> . (Int -> (Int -> ([a] -> ([a] , [a]))))!
    size    subSize solution
    7       16      ((GHC.List.take arg0 arg2) , (GHC.List.drop arg1 arg2))
    -----------------
    --- BACKTRACE ---
    -----------------
    (?? :: ([a] , [a]))
    (?? :: (tau0 -> ([a] , [a]))) (?? :: tau0)
    (?? :: (tau1 -> (tau0 -> ([a] , [a])))) (?? :: tau1) (?? :: tau0)
    (,) (?? :: tau1) (?? :: tau0)
    (,) (?? :: (tau4 -> [a])) (?? :: tau4) (?? :: tau0)
    (,) GHC.List.take arg0 (?? :: tau4) (?? :: tau0)
    (,) (GHC.List.take arg0 arg2) (?? :: tau0)
    ((GHC.List.take arg0 arg2) , (GHC.List.drop arg1 arg2))
    -----------------
    (15.15 secs, 6,774,323,224 bytes)

indexesOf
synGuardO' "([(a,Int)] -> [(a,Int)]) -> [a] -> [Int] -> [Int]" ["GHC.List.map", "Data.Tuple.snd", "GHC.List.zip"] [(["map (\\(x, y) -> (x, y * y))", "[1,2,3]", "[9,8,7]"], "[81, 64, 49]")]
solution: "\\f xs ys -> Data.List.map Data.Tuple.snd (f (Data.List.zip xs ys))"

    (Quota 8) Done with <a> . ((([(a , Int)] -> [(a , Int)])) -> ([a] -> ([Int] -> [Int])))!
    size    subSize solution
    6       24      GHC.List.map Data.Tuple.snd (arg0 (GHC.List.zip arg1 arg2))
    -----------------
    --- BACKTRACE ---
    -----------------
    (?? :: [Int])
    (?? :: (tau0 -> [Int])) (?? :: tau0)
    (?? :: (tau1 -> (tau0 -> [Int]))) (?? :: tau1) (?? :: tau0)
    GHC.List.map (?? :: tau1) (?? :: tau0)
    GHC.List.map Data.Tuple.snd (?? :: tau0)
    GHC.List.map Data.Tuple.snd (?? :: (tau6 -> [(tau5 , Int)])) (?? :: tau6)
    GHC.List.map Data.Tuple.snd arg0 (?? :: tau6)
    GHC.List.map Data.Tuple.snd arg0 (?? :: (tau7 -> [(a , Int)])) (?? :: tau7)
    GHC.List.map Data.Tuple.snd arg0 (?? :: (tau8 -> (tau7 -> [(a , Int)]))) (?? :: tau8) (?? :: tau7)
    GHC.List.map Data.Tuple.snd arg0 GHC.List.zip (?? :: tau8) (?? :: tau7)
    GHC.List.map Data.Tuple.snd arg0 GHC.List.zip arg1 (?? :: tau7)
    GHC.List.map Data.Tuple.snd (arg0 (GHC.List.zip arg1 arg2))
    -----------------
    (5.01 secs, 1,669,507,456 bytes)

containsEdge
synGuardO' "[Int] -> (Int,Int) -> Bool" ["Pair", "GHC.List.elem", "&&", "GHC.List.elem", "Data.Tuple.fst", "Data.Tuple.snd"] [(["[1,2,3,4]", "(1,2)"], "True"), (["[1,2,3,4]", "(1,5)"], "False")]
solution: "\\xs p -> ((fst p) `Data.List.elem` xs) && ((snd p) `Data.List.elem` xs)"

    ---> dies at quota 12 (out of memory)
















stack run -- hplus topdown --query='Maybe(a->b)->a->b' --disableHO --


Flags 
  json :: String, -- query, examples, guards?????????????
  disableHO :: Bool, -- a
  altIMode :: Bool, -- should we do imode w/ env first or regular i-mode
  memoize :: Bool -- should we memoize? 
  backtrace :: Bool -- should we print backtracing? 







memoize makes it faster!

syn' "[a] -> [b] -> [[(a,b)]]" [(["[1,2,3]","[2,3,4]"], "[[(1,2), (1,3), (1,4)], [(2,2), (2,3), (2,4)], [(3,2), (3,3), (3,4)]]")]

-- with memoize:
(Quota 7) Done with <b> . <a> . ([a] -> ([b] -> [[(a , b)]]))!
size    subSize solution
7       20      GHC.List.map (\arg2 ->
    GHC.List.zip (GHC.List.repeat arg2) arg1) arg0

(244.39 secs, 105,026,657,520 bytes)

-- without memoize:
(Quota 7) Done with <b> . <a> . ([a] -> ([b] -> [[(a , b)]]))!
size    subSize solution
7       20      GHC.List.map (\arg2 ->
    GHC.List.zip (GHC.List.repeat arg2) arg1) arg0

(791.19 secs, 128,242,005,744 bytes)






  let aType = ScalarT (DatatypeT "a" [] []) ftrue :: RType
  let readAType = ScalarT (DatatypeT "@@hplusTC@@Read" [aType] []) ftrue :: RType
  let strType = ScalarT (DatatypeT "String" [] []) ftrue :: RType
  
  let readComp = ForallT "a" $ Monotype $ FunctionT "" readAType (FunctionT "" strType aType) :: RSchema -- Read a => String -> a


  -- let rawSyms = Map.insert "Text.Read.read" readComp $ Map.filterWithKey (\k v -> any (`isInfixOf` (show k)) guards) $ env ^. symbols



Yeah, I think the general idea here is correct. I don’t know why you need a
RType here, but there is a method called addTrue somewhere in the code (it
might be in Database.Convert or Database.Util) to help you turn an SType into
a RType . Since we are not using any predicate in the search, so we always add
True as the refinement when we need a RType (edited) 



"(Data.Bool.&&)"                            (Bool -> (Bool -> Bool))
"(Data.Bool.||)"                            (Bool -> (Bool -> Bool))
"(Data.Eq./=)"                              <a> . (@@hplusTC@@Eq (a) -> (a -> (a -> Bool)))
"(Data.Eq.==)"                              <a> . (@@hplusTC@@Eq (a) -> (a -> (a -> Bool)))
"(Data.Function.$)"                         <b> . <a> . (((a -> b)) -> (a -> b))
"(Data.Function.&)"                         <b> . <a> . (a -> (((a -> b)) -> b))
"(Data.Function..)"                         <c> . <b> . <a> . (((b -> c)) -> (((a -> b)) -> (a -> c)))
"(GHC.List.!!)"                             <a> . ([a] -> (Int -> a))
"(GHC.List.++)"                             <a> . ([a] -> ([a] -> [a]))
"@@hplusTCInstance@@0EqBool"               @@hplusTC@@Eq (Bool)
"@@hplusTCInstance@@0EqChar"               @@hplusTC@@Eq (Char)
"@@hplusTCInstance@@0EqDouble"             @@hplusTC@@Eq (Double)
"@@hplusTCInstance@@0EqFloat"              @@hplusTC@@Eq (Float)
"@@hplusTCInstance@@0EqInt"                @@hplusTC@@Eq (Int)
"@@hplusTCInstance@@0EqUnit"               @@hplusTC@@Eq (Unit)
"@@hplusTCInstance@@0NumDouble"            @@hplusTC@@Num (Double)
"@@hplusTCInstance@@0NumFloat"             @@hplusTC@@Num (Float)
"@@hplusTCInstance@@0NumInt"               @@hplusTC@@Num (Int)
"@@hplusTCInstance@@0OrdBool"              @@hplusTC@@Ord (Bool)
"@@hplusTCInstance@@0OrdChar"              @@hplusTC@@Ord (Char)
"@@hplusTCInstance@@0OrdDouble"            @@hplusTC@@Ord (Double)
"@@hplusTCInstance@@0OrdFloat"             @@hplusTC@@Ord (Float)
"@@hplusTCInstance@@0OrdInt"               @@hplusTC@@Ord (Int)
"@@hplusTCInstance@@0Show"                  <b> . <a> . (@@hplusTC@@Show (a) -> (@@hplusTC@@Show (b) -> @@hplusTC@@Show ((Either (a) (b)))))
"@@hplusTCInstance@@0ShowBool"             @@hplusTC@@Show (Bool)
"@@hplusTCInstance@@0ShowChar"             @@hplusTC@@Show (Char)
"@@hplusTCInstance@@0ShowDouble"           @@hplusTC@@Show (Double)
"@@hplusTCInstance@@0ShowFloat"            @@hplusTC@@Show (Float)
"@@hplusTCInstance@@0ShowInt"              @@hplusTC@@Show (Int)
"@@hplusTCInstance@@0ShowUnit"             @@hplusTC@@Show (Unit)
"@@hplusTCInstance@@1Read"                  <b> . <a> . (@@hplusTC@@Read (a) -> (@@hplusTC@@Read (b) -> @@hplusTC@@Read ((Either (a) (b)))))
"@@hplusTCInstance@@2Ord"                   <b> . <a> . (@@hplusTC@@Ord (a) -> (@@hplusTC@@Ord (b) -> @@hplusTC@@Ord ((Either (a) (b)))))
"@@hplusTCInstance@@3Eq"                    <b> . <a> . (@@hplusTC@@Eq (a) -> (@@hplusTC@@Eq (b) -> @@hplusTC@@Eq ((Either (a) (b)))))
"@@hplusTCInstance@@5Semigroup"             <b> . <a> . @@hplusTC@@Semigroup ((Either (a) (b)))
"@@hplusTCInstance@@8Eq"                    <a> . (@@hplusTC@@Eq (a) -> @@hplusTC@@Eq (([a])))
"Cons"                                      <a> . (a -> ([a] -> {[a]|_v == (Cons x xs)}))
"Data.Bool.False"                            Bool
"Data.Bool.True"                             Bool
"Data.Bool.bool"                            <a> . (a -> (a -> (Bool -> a)))
"Data.Bool.not"                             (Bool -> Bool)
"Data.Bool.otherwise"                        Bool
"Data.Either.Left"                          <b> . <a> . (a -> Either (a) (b))
"Data.Either.Right"                         <b> . <a> . (b -> Either (a) (b))
"Data.Either.either"                        <c> . <b> . <a> . (((a -> c)) -> (((b -> c)) -> (Either (a) (b) -> c)))
"Data.Either.fromLeft"                      <b> . <a> . (a -> (Either (a) (b) -> a))
"Data.Either.fromRight"                     <b> . <a> . (b -> (Either (a) (b) -> b))
"Data.Either.isLeft"                        <b> . <a> . (Either (a) (b) -> Bool)
"Data.Either.isRight"                       <b> . <a> . (Either (a) (b) -> Bool)
"Data.Either.lefts"                         <b> . <a> . ([Either (a) (b)] -> [a])
"Data.Either.partitionEithers"              <b> . <a> . ([Either (a) (b)] -> ([a] , [b]))
"Data.Either.rights"                        <b> . <a> . ([Either (a) (b)] -> [b])
"Data.Function.const"                       <b> . <a> . (a -> (b -> a))
"Data.Function.fix"                         <a> . (((a -> a)) -> a)
"Data.Function.flip"                        <c> . <b> . <a> . (((a -> (b -> c))) -> (b -> (a -> c)))
"Data.Function.id"                          <a> . (a -> a)
"Data.Function.on"                          <c> . <b> . <a> . (((b -> (b -> c))) -> (((a -> b)) -> (a -> (a -> c))))
"Data.List.group"                           <a> . (@@hplusTC@@Eq (a) -> ([a] -> [[a]]))
"Data.Maybe.Just"                           <a> . (a -> Maybe (a))
"Data.Maybe.Nothing"                        <a> . Maybe (a)
"Data.Maybe.catMaybes"                      <a> . ([Maybe (a)] -> [a])
"Data.Maybe.fromJust"                       <a> . (Maybe (a) -> a)
"Data.Maybe.fromMaybe"                      <a> . (a -> (Maybe (a) -> a))
"Data.Maybe.isJust"                         <a> . (Maybe (a) -> Bool)
"Data.Maybe.isNothing"                      <a> . (Maybe (a) -> Bool)
"Data.Maybe.listToMaybe"                    <a> . ([a] -> Maybe (a))
"Data.Maybe.mapMaybe"                       <b> . <a> . (((a -> Maybe (b))) -> ([a] -> [b]))
"Data.Maybe.maybe"                          <b> . <a> . (b -> (((a -> b)) -> (Maybe (a) -> b)))
"Data.Maybe.maybeToList"                    <a> . (Maybe (a) -> [a])
"Data.Tuple.curry"                          <c> . <b> . <a> . ((((a , b) -> c)) -> (a -> (b -> c)))
"Data.Tuple.fst"                            <b> . <a> . ((a , b) -> a)
"Data.Tuple.snd"                            <b> . <a> . ((a , b) -> b)
"Data.Tuple.swap"                           <b> . <a> . ((a , b) -> (b , a))
"Data.Tuple.uncurry"                        <c> . <b> . <a> . (((a -> (b -> c))) -> ((a , b) -> c))
"GHC.Char.chr"                              (Int -> Char)
"GHC.Char.eqChar"                           (Char -> (Char -> Bool))
"GHC.Char.neChar"                           (Char -> (Char -> Bool))
"GHC.List.all"                              <a> . (((a -> Bool)) -> ([a] -> Bool))
"GHC.List.and"                             ([Bool] -> Bool)
"GHC.List.any"                              <a> . (((a -> Bool)) -> ([a] -> Bool))
"GHC.List.break"                            <a> . (((a -> Bool)) -> ([a] -> ([a] , [a])))
"GHC.List.concat"                           <a> . ([[a]] -> [a])
"GHC.List.concatMap"                        <b> . <a> . (((a -> [b])) -> ([a] -> [b]))
"GHC.List.cycle"                            <a> . ([a] -> [a])
"GHC.List.drop"                             <a> . (Int -> ([a] -> [a]))
"GHC.List.dropWhile"                        <a> . (((a -> Bool)) -> ([a] -> [a]))
"GHC.List.elem"                             <a> . (@@hplusTC@@Eq (a) -> (a -> ([a] -> Bool)))
"GHC.List.filter"                           <a> . (((a -> Bool)) -> ([a] -> [a]))
"GHC.List.foldl"                            <b> . <a> . (((b -> (a -> b))) -> (b -> ([a] -> b)))
"GHC.List.foldl'"                           <b> . <a> . (((b -> (a -> b))) -> (b -> ([a] -> b)))
"GHC.List.foldl1"                           <a> . (((a -> (a -> a))) -> ([a] -> a))
"GHC.List.foldl1'"                          <a> . (((a -> (a -> a))) -> ([a] -> a))
"GHC.List.foldr"                            <b> . <a> . (((a -> (b -> b))) -> (b -> ([a] -> b)))
"GHC.List.foldr1"                           <a> . (((a -> (a -> a))) -> ([a] -> a))
"GHC.List.head"                             <a> . ([a] -> a)
"GHC.List.init"                             <a> . ([a] -> [a])
"GHC.List.iterate"                          <a> . (((a -> a)) -> (a -> [a]))
"GHC.List.iterate'"                         <a> . (((a -> a)) -> (a -> [a]))
"GHC.List.last"                             <a> . ([a] -> a)
"GHC.List.length"                           <a> . ([a] -> Int)
"GHC.List.lookup"                           <b> . <a> . (@@hplusTC@@Eq (a) -> (a -> ([(a , b)] -> Maybe (b))))
"GHC.List.map"                              <b> . <a> . (((a -> b)) -> ([a] -> [b]))
"GHC.List.maximum"                          <a> . (@@hplusTC@@Ord (a) -> ([a] -> a))
"GHC.List.minimum"                          <a> . (@@hplusTC@@Ord (a) -> ([a] -> a))
"GHC.List.notElem"                          <a> . (@@hplusTC@@Eq (a) -> (a -> ([a] -> Bool)))
"GHC.List.null"                             <a> . ([a] -> Bool)
"GHC.List.or"                              ([Bool] -> Bool)
"GHC.List.product"                          <a> . (@@hplusTC@@Num (a) -> ([a] -> a))
"GHC.List.repeat"                           <a> . (a -> [a])
"GHC.List.replicate"                        <a> . (Int -> (a -> [a]))
"GHC.List.reverse"                          <a> . ([a] -> [a])
"GHC.List.scanl"                            <b> . <a> . (((b -> (a -> b))) -> (b -> ([a] -> [b])))
"GHC.List.scanl'"                           <b> . <a> . (((b -> (a -> b))) -> (b -> ([a] -> [b])))
"GHC.List.scanl1"                           <a> . (((a -> (a -> a))) -> ([a] -> [a]))
"GHC.List.scanr"                            <b> . <a> . (((a -> (b -> b))) -> (b -> ([a] -> [b])))
"GHC.List.scanr1"                           <a> . (((a -> (a -> a))) -> ([a] -> [a]))
"GHC.List.span"                             <a> . (((a -> Bool)) -> ([a] -> ([a] , [a])))
"GHC.List.splitAt"                          <a> . (Int -> ([a] -> ([a] , [a])))
"GHC.List.sum"                              <a> . (@@hplusTC@@Num (a) -> ([a] -> a))
"GHC.List.tail"                             <a> . ([a] -> [a])
"GHC.List.take"                             <a> . (Int -> ([a] -> [a]))
"GHC.List.takeWhile"                        <a> . (((a -> Bool)) -> ([a] -> [a]))
"GHC.List.uncons"                           <a> . ([a] -> Maybe (((a , [a]))))
"GHC.List.unzip"                            <b> . <a> . ([(a , b)] -> ([a] , [b]))
"GHC.List.unzip3"                           <c> . <b> . <a> . ([((a , b) , c)] -> (([a] , [b]) , [c]))
"GHC.List.zip"                              <b> . <a> . ([a] -> ([b] -> [(a , b)]))
"GHC.List.zip3"                             <c> . <b> . <a> . ([a] -> ([b] -> ([c] -> [((a , b) , c)])))
"GHC.List.zipWith"                          <c> . <b> . <a> . (((a -> (b -> c))) -> ([a] -> ([b] -> [c])))
"GHC.List.zipWith3"                         <d> . <c> . <b> . <a> . (((a -> (b -> (c -> d)))) -> ([a] -> ([b] -> ([c] -> [d]))))
"Nil"                                       <a> . {[a]|_v == (Nil)}
"Pair"                                      <b> . <a> . (a -> (b -> {(a , b)|_v == (Pair x y)}))
"Text.Show.show"                            <a> . (@@hplusTC@@Show (a) -> (a -> [Char]))
"Text.Show.showChar"                        (Char -> ([Char] -> [Char]))
"Text.Show.showList"                        <a> . (@@hplusTC@@Show (a) -> ([a] -> ([Char] -> [Char])))
"Text.Show.showListWith"                    <a> . (((a -> ([Char] -> [Char]))) -> ([a] -> ([Char] -> [Char])))
"Text.Show.showParen"                       (Bool -> ((([Char] -> [Char])) -> ([Char] -> [Char])))
"Text.Show.showString"                     ([Char] -> ([Char] -> [Char]))
"Text.Show.shows"                           <a> . (@@hplusTC@@Show (a) -> (a -> ([Char] -> [Char])))
"Text.Show.showsPrec"                       <a> . (@@hplusTC@@Show (a) -> (Int -> (a -> ([Char] -> [Char]))))
"arg0"                                      [a]
"arg1"                                      (a -> Bool)
"fst"                                       <b> . <a> . ((a , b) -> a)
"snd"                                       <b> . <a> . ((a , b) -> b)






------
We are working on spliting up the goal into `alpha -> T` and `alpha`. Here is how we're doing it:

```
goalType :: RType 

let alpha' = ScalarT (TypeVarT Map.empty "alpha") ftrue :: RType
let schema' = ForallT "alpha" $ Monotype $ FunctionT "myArg" alpha' goalType :: RSchema

-- reset the name counter so it generates the same tau
indices <- getNameCounter :: TopDownSolver IO (Map Id Int)
alpha <- freshType (env ^. boundTypeVars) (ForallT "alpha" $ Monotype $ alpha') :: TopDownSolver IO RType
setNameCounter indices

schema <- freshType (env ^. boundTypeVars) schema' :: TopDownSolver IO RType

schemaProgram <- dfsEMode env messageChan (quota - 1) schema :: TopDownSolver IO RProgram
let quota' = quota - sizeOf schemaProgram

st' <- get
let sub = st' ^. typeAssignment
let alphaSub' = stypeSubstitute sub (shape alpha) :: SType 
let alphaSub = (refineBot env alphaSub')

alphaProgram <- dfsIMode env messageChan (quota') (refineBot env alphaSub) :: TopDownSolver IO RProgram
```

We have a few questions: 

* is this a good way of splitting up the goals into `alpha -> T` and `alpha`? Is there a better way to do it? 
* if this way is correct (or at least not totally wrong), we are running into an issue where `alphaSub` is `(b -> (a -> {c|False}))`, but that isn't what we want. We are pretty sure this comes from the fact we're using `stypeSubstitute` (which creates `SType` and then we have to convirt back into `RType`). We say that there's a function `typeSubstitute`, but it assumes the substitution is `RType`, whereas `st' ^. typeAssignment` is based on `SType`s. Is there a way to get an `RType` version? Or should we be using a different `refine` function? 






Starting!
Arguments: fromList [("arg0",(((b -> (a -> c))) -> d)),("arg1",(a -> (b -> c)))]
Goal: d
==================

running dfs on <d> . <c> . <b> . <a> . (((((b -> (a -> c))) -> d)) -> (((a -> (b -> c))) -> d)) at size 6
splitting (d) up into: ((tau0 -> d)) and (tau0)
found    ((tau0 -> d)) --- arg0 
refined alpha into alphaSub = b -> a -> c

------
we are actually here scalart!!!!! 
splitting (tau0) up into: ((tau1 -> tau0)) and (tau1)
splitting ((tau1 -> tau0)) up into: ((tau2 -> (tau1 -> tau0))) and (tau2)
splitting ((tau2 -> (tau1 -> tau0))) up into: ((tau3 -> (tau2 -> (tau1 -> tau0)))) and (tau3)
splitting ((tau3 -> (tau2 -> (tau1 -> tau0)))) up into: ((tau4 -> (tau3 -> (tau2 -> (tau1 -> tau0))))) and (tau4)
splitting ((tau4 -> (tau3 -> (tau2 -> (tau1 -> tau0))))) up into: ((tau5 -> (tau4 -> (tau3 -> (tau2 -> (tau1 -> tau0)))))) and (tau5)
splitting ((tau0 -> d)) up into: ((tau1 -> (tau0 -> d))) and (tau1)
splitting ((tau1 -> (tau0 -> d))) up into: ((tau2 -> (tau1 -> (tau0 -> d)))) and (tau2)
splitting ((tau2 -> (tau1 -> (tau0 -> d)))) up into: ((tau3 -> (tau2 -> (tau1 -> (tau0 -> d))))) and (tau3)
splitting ((tau3 -> (tau2 -> (tau1 -> (tau0 -> d))))) up into: ((tau4 -> (tau3 -> (tau2 -> (tau1 -> (tau0 -> d)))))) and (tau4)
splitting ((tau4 -> (tau3 -> (tau2 -> (tau1 -> (tau0 -> d)))))) up into: ((tau5 -> (tau4 -> (tau3 -> (tau2 -> (tau1 -> (tau0 -> d))))))) and (tau5)

running dfs on <d> . <c> . <b> . <a> . (((((b -> (a -> c))) -> d)) -> (((a -> (b -> c))) -> d)) at size 7
splitting (d) up into: ((tau0 -> d)) and (tau0)
found    ((tau0 -> d)) --- arg0 
refined alpha into alphaSub = b -> a -> c

------
we are actually here scalart!!!!! 
splitting (tau0) up into: ((tau1 -> tau0)) and (tau1)
splitting ((tau1 -> tau0)) up into: ((tau2 -> (tau1 -> tau0))) and (tau2)
splitting ((tau2 -> (tau1 -> tau0))) up into: ((tau3 -> (tau2 -> (tau1 -> tau0)))) and (tau3)
splitting ((tau3 -> (tau2 -> (tau1 -> tau0)))) up into: ((tau4 -> (tau3 -> (tau2 -> (tau1 -> tau0))))) and (tau4)
splitting ((tau4 -> (tau3 -> (tau2 -> (tau1 -> tau0))))) up into: ((tau5 -> (tau4 -> (tau3 -> (tau2 -> (tau1 -> tau0)))))) and (tau5)
splitting ((tau5 -> (tau4 -> (tau3 -> (tau2 -> (tau1 -> tau0)))))) up into: ((tau6 -> (tau5 -> (tau4 -> (tau3 -> (tau2 -> (tau1 -> tau0))))))) and (tau6)
splitting ((tau0 -> d)) up into: ((tau1 -> (tau0 -> d))) and (tau1)
splitting ((tau1 -> (tau0 -> d))) up into: ((tau2 -> (tau1 -> (tau0 -> d)))) and (tau2)
splitting ((tau2 -> (tau1 -> (tau0 -> d)))) up into: ((tau3 -> (tau2 -> (tau1 -> (tau0 -> d))))) and (tau3)
splitting ((tau3 -> (tau2 -> (tau1 -> (tau0 -> d))))) up into: ((tau4 -> (tau3 -> (tau2 -> (tau1 -> (tau0 -> d)))))) and (tau4)
splitting ((tau4 -> (tau3 -> (tau2 -> (tau1 -> (tau0 -> d)))))) up into: ((tau5 -> (tau4 -> (tau3 -> (tau2 -> (tau1 -> (tau0 -> d))))))) and (tau5)
splitting ((tau5 -> (tau4 -> (tau3 -> (tau2 -> (tau1 -> (tau0 -> d))))))) up into: ((tau6 -> (tau5 -> (tau4 -> (tau3 -> (tau2 -> (tau1 -> (tau0 -> d)))))))) and (tau6)

running dfs on <d> . <c> . <b> . <a> . (((((b -> (a -> c))) -> d)) -> (((a -> (b -> c))) -> d)) at size 8
splitting (d) up into: ((tau0 -> d)) and (tau0)
found    ((tau0 -> d)) --- arg0 
refined alpha into alphaSub = b -> a -> c

------
we are actually here scalart!!!!! 
splitting (tau0) up into: ((tau1 -> tau0)) and (tau1)
splitting ((tau1 -> tau0)) up into: ((tau2 -> (tau1 -> tau0))) and (tau2)
splitting ((tau2 -> (tau1 -> tau0))) up into: ((tau3 -> (tau2 -> (tau1 -> tau0)))) and (tau3)
splitting ((tau3 -> (tau2 -> (tau1 -> tau0)))) up into: ((tau4 -> (tau3 -> (tau2 -> (tau1 -> tau0))))) and (tau4)
splitting ((tau4 -> (tau3 -> (tau2 -> (tau1 -> tau0))))) up into: ((tau5 -> (tau4 -> (tau3 -> (tau2 -> (tau1 -> tau0)))))) and (tau5)
splitting ((tau5 -> (tau4 -> (tau3 -> (tau2 -> (tau1 -> tau0)))))) up into: ((tau6 -> (tau5 -> (tau4 -> (tau3 -> (tau2 -> (tau1 -> tau0))))))) and (tau6)
splitting ((tau6 -> (tau5 -> (tau4 -> (tau3 -> (tau2 -> (tau1 -> tau0))))))) up into: ((tau7 -> (tau6 -> (tau5 -> (tau4 -> (tau3 -> (tau2 -> (tau1 -> tau0)))))))) and (tau7)
splitting ((tau0 -> d)) up into: ((tau1 -> (tau0 -> d))) and (tau1)
splitting ((tau1 -> (tau0 -> d))) up into: ((tau2 -> (tau1 -> (tau0 -> d)))) and (tau2)
splitting ((tau2 -> (tau1 -> (tau0 -> d)))) up into: ((tau3 -> (tau2 -> (tau1 -> (tau0 -> d))))) and (tau3)
splitting ((tau3 -> (tau2 -> (tau1 -> (tau0 -> d))))) up into: ((tau4 -> (tau3 -> (tau2 -> (tau1 -> (tau0 -> d)))))) and (tau4)
splitting ((tau4 -> (tau3 -> (tau2 -> (tau1 -> (tau0 -> d)))))) up into: ((tau5 -> (tau4 -> (tau3 -> (tau2 -> (tau1 -> (tau0 -> d))))))) and (tau5)
splitting ((tau5 -> (tau4 -> (tau3 -> (tau2 -> (tau1 -> (tau0 -> d))))))) up into: ((tau6 -> (tau5 -> (tau4 -> (tau3 -> (tau2 -> (tau1 -> (tau0 -> d)))))))) and (tau6)
splitting ((tau6 -> (tau5 -> (tau4 -> (tau3 -> (tau2 -> (tau1 -> (tau0 -> d)))))))) up into: ((tau7 -> (tau6 -> (tau5 -> (tau4 -> (tau3 -> (tau2 -> (tau1 -> (tau0 -> d))))))))) and (tau7)

-}














-- goal: Maybe (Maybe (Maybe (b)))         program: arg2
-- goal: Maybe (Maybe (b))         program: Data.Maybe.fromJust arg2
-- goal: Maybe (b)         program: Data.Maybe.fromJust (Data.Maybe.fromJust arg2)
-- goal: b         program: Data.Maybe.fromJust (Data.Maybe.fromJust (Data.Maybe.fromJust arg2))
-- goal: b         program: 
-- (\arg2 -> Data.Maybe.fromJust (Data.Maybe.fromJust (Data.Maybe.fromJust arg2))) $ arg2

  {-
    syn "arg0:Maybe (a -> b) -> arg1:a -> b"
    (fromJust arg0) arg1

                                        ?? :: b
                                           \
                          $ ?? :: (tau1 -> b) -> (?? :: a) -> b
                              /                           \
          fromJust ?? :: Maybe (tau1 -> b) -> (a -> b)   arg1 :: a
                          /
                  arg0  :: Maybe (a -> b)


    solution:       (Data.Maybe.fromJust arg0) $ arg1
    we got:         (Data.Maybe.fromJust arg0 arg1) $ arg1
    we prob want:   (\arg2 -> Data.Maybe.fromJust arg0 arg2) $ arg1
    
    
    
    overall goal: b  component: tau4 (freshvars: (Maybe (tau4) -> tau4))    ====> Maybe (b) -> b
    overall goal: tau1 -> b  component: tau4 (freshvars: (Maybe (tau4) -> tau4))    ====> Maybe (tau1 -> b) -> tau1 -> b

    their solution: (Data.Maybe.fromJust arg0) $ arg1
    starting goal type: b
    component: arg0     :: Maybe (a -> b)
    component: fromJust :: Maybe tau1 -> tau1
    component: $        :: (((tau1 -> tau2)) -> (tau1 -> tau2))

    component: fromJust :: Maybe (tau1 -> tau2) -> (tau1 -> tau2)

    currently, we only check this:
      unified: fromJust :: Maybe b -> b
    but really we wanna check this too:
      unified: fromJust :: Maybe (tau1 -> b) -> (tau1 -> b)
    
  -}


{-

Hm, thinking about your example made me realize there are two ways to do
top-down enumeration that we might consider here:

1. If our goal type is T we literally search for a component/variable whose
type unifies with T. This won't work in your example with the query Maybe (a
-> b) -> a -> b and component fromJust unless you also add $.
  ======> (what we were doing before)
     
    T :: Maybe (a -> b) -> a -> b
    arg0 :: Maybe (a -> b)
    arg1 :: a
    T' :: b
    [b] $ :: (tau -> b) -> tau -> b      (originally :: (tau -> tau1) -> tau -> tau1 )
    args: [(tau -> b), tau]

    arg: tau -> b     is a functionType
    newArgs = [arg2: tau]
    add arg2 to the env
    recursive with b as goal
    
    arg2 :: tau
    T'' :: b
    [tau -> b] fromJust :: Maybe (tau -> b) -> (tau -> b)

    
    
    
    [tau -> b] fromJust :: Maybe b -> b
    fromJust :: Maybe tau1 -> tau1
    T''' :: tau
    

    


    fromJust :: <alpha> . Maybe alpha -> alpha
    arg0 :: Maybe (a -> b)
    
    $ :: (tau -> Maybe (a -> b) -> a -> b) -> tau -> Maybe (a -> b) -> a -> b

    

2. Another way to do top-down is what Synquid does. If the goal is T and we
are currently in the enumeration mode, Synquid will consider two
possibilities: 
  a) is there a variable (component) whose entire type unifies with T?  

  ======> comp :: <a,b,c> . a -> b -> c
  ======> goal :: Int -> Bool -> Int         ~ T
  
  b) otherwise we are looking for an application, so generate two
     subgoals alpha -> T and alpha, and repeat the process.

    f: alpha -> (Int -> Bool -> Int)
    x: alpha

  ======> just like using the component ($)
  ======> which recurses on its arguments (a function alpha -> b and 
          its argument alpha)

The latter approach works for this example (without $):

  ***** ONE *****
  
    First our goal is: 
  
  fromJust : Maybe gamma -> gamma
  x : Maybe(a -> b)
  y : a 
  |-
  ?? : b
    
    We don't have variables of type b so, it's going to be an application, let's generate
    two subgoals 

      alpha -> b   
      alpha 
    
    where alpha is a fresh type variable.


  * Now let's solve the subgoal (alpha -> b). fromJust unifies with it, with 
  
  gamma := b, 
  alpha := Maybe b, 
    
    but then for the seond subgoal (alpha) we need a (Maybe b) which will lead to a dead end.

  * So let's consider the other option for (alpha -> b), an application; this generates 
    two more subgoals: (this is still to solve (alpha -> b))

    beta -> (alpha -> b)   
    beta
    
    Let's solve the first one (beta -> (alpha -> b)).

  * Again fromJust unifies with this subgoal, this time with
    gamma := (alpha -> b),
    beta := Maybe (alpha -> b) .

  * Our second subgoal (beta) becomes (Maybe (alpha -> b)) which can be solved with the variable
    x producing the substitution 
    alpha := a

  environment:
    fromJust : Maybe gamma -> gamma
    x : Maybe(a -> b)
    y : a 

  * Popping up a level, we still have an open subgoal 
    alpha (aka a) 
    
    which is solved by the variable y.


1. given goal T, look for exactly T in the env
2. add a type var (alpha) and look for alpha -> T and also alpha (apply $)
`mplus` not `interleave`

Ideas for (1):
  
  * getExactUnified `mplus` getTypeVarUnified


  ======> comp :: <a,b,c> . a -> b -> c
  ======> goal :: Int -> Bool -> Int         ~ T

    getExactUnified would return
  => PSymbol id (sub freshVars)

QUESTIONS:
    1. would we be interleaving the two approaches? 
    answer: no. first try to (1) and then go to (2)

------------------------------
An interesting aside is that Synquid only had to look at components of a
given arity when trying to unify variables with a type because it didn't
allow unifying a type var with an arrow.

Whereas here you can't do that: you in fact need to unify
  a goal (beta -> alpha -> b) of arity 2
  with the component fromMaybe of arity 1.
But this doesn't break the algorithm, just removes an optimization.

Also Synquid always knew when to stop producing
goals of higher arities, because they were limited by the max arity of the
components, while you don't know when to stop. But the size ordering should
take of that.
  ===> not sure how size takes care of it, but we'll probably run into it
  ===> if there is a solution we'll return the smallest one anyway
   even if we search for huge a->b->c->d->e->f->g types
     alpha -> T     size k
     alpha          size quota - k
------------------------------

So my main question is: is approach 1 + the $ component
  ===> what we were doing before

actually *the same* as approach 2 without the $ component?
  ===> this new algorithm where if we don't find T, we look for alpha -> T and alpha

My hunch is that 1 + $ has a lot of redundancy (e.g. it's enumerating almost
every application both "natively" and with $) and so approach 2 should be more
efficient. But you should think about this, and maybe go through some simple
examples with pen and paper.

Oh also: here's a neat way to discourage the synthesizer from using big types.
You know how in many formalizations of languages with polymorphic types
(SystemF etc, even the liquid types paper has it I think?) you write type
application explicitly in the program. E.g. the above solution \x y ->
fromJust x y actually has to be written in proper SystemF syntax as \x y ->
([gamma := a -> b] fromJust) x y . So if you imagine that you are synthesizing
programs in this language with explicit type applications and calculate
program size accordingly for size-based enumeration, then you'll automatically
get the property we were talking about.




------------------
FROM ZHENG
------------------

[ b ]                          $ :: (tau1 -> b) -> tau1 -> b
[ tau1 -> b ]                  $ :: (tau2 -> (tau1 -> b)) -> tau2 -> (tau1 -> b) 
[ (tau2 -> (tau1 -> b)) ]      fromJust :: Maybe (tau1 -> b) -> (tau1 -> b)

[ tau1 -> b ]   Maybe tau1 -> tau1                   (Maybe b -> b) 
[ tau1 -> b ]   (tau2 -> tau3) -> tau2 -> tau3       $ :: (tau2 -> (tau1 -> b)) -> tau2 -> (tau1 -> b) 



it finds (??::tau1 -> b) $ (??::tau1)
it finds ((??::tau2 -> (tau1 -> b)) $ (??::tau2)) $ (??::tau1)
it finds (fromJust (??::Maybe (tau1 -> b))) $ (??::tau2)) $ (??::tau1)
it finds (fromJust (arg0::Maybe (tau1 -> b))) $ (arg0::tau2)) $ (??::tau1)

(\arg2 -> fromJust arg0 $ arg1) $ arg1

* First, I don’t think (tau1 -> b) unifies with fromJust and results in 
  
  Maybe (tau1 -> b) -> (tau1 -> b)

instead, it should be (Maybe b -> b) , and you cannot find a term of type (Maybe b) .
  
  =====> when unifying (tau1 -> b) w/ fromJust, you get (Maybe b -> b) 

* Next, you will try to use ($) that returns type (tau1 -> b) , so you find that 
  
  $ :: (tau2 -> (tau1 -> b)) -> tau2 -> (tau1 -> b) 
  
and this time you will find the first argument unifies with 
    fromJust :: Maybe (tau1 -> b) -> (tau1 -> b)



dfs goaltype: [tau1 -> b]

getUnifiedComp = fromJust :: Maybe (tau1 -> b) -> tau1 -> b

args = [Maybe (tau1 -> b), tau1]

1. find stuff for Maybe (tau1 -> b)

  find arg0

2. check if remaining type unifies with OG goalType tau1 -> b 
  
  => it does, so return here instead of going to next argument


------------------
FANGIRLING SECTION
------------------

darya: we are going to first try changing our current implementation
   but with moving the $ to the end of the components list that it checks against
   (it’s currently in the front… which is front-loading a bunch of unnecessary synthesizing)

nadia: Yeah, I think this makes sense. So just to summarize the plan is:

   1. Put $ at the end
   2. Put in explicit type applications to bias against complex types
   3. Look at the logs to see if the presence of $ causes enumeration of lot of redundant (equivalent) terms
   4. If 3 is true, then implement a more Synquid-like enumeration

daniel: hey, could you explain what is biasing against complex types?

nadia: ah, because type instantiations would be counted in the size of the program, 
   as you are doing by-size enumeration, programs with larger type instantiations
   will be enumerated later in the search and ones with smaller instantiations
   earlier in the search

daniel: so for example, we have fromJust : <a> . Maybe a -> a, if we instantiate this
    to like fromJust : Maybe b -> b  the "b" counts as +1 size?

nadia: yeah
   because your actual program is now not just fromJust but `[b] fromJust`

daniel: right now all the instantiations are in a map,
    like {"a" ===> Int -> Int, "b" ===> Int} or something like that.
    then instead of the length of this map like i said, we could add up like
    (Int -> Int) + (Int) = 3 + 1   and get 4.
    so a program that made this map in the environment gets a size of 4?
  
nadia: right, so then you must be able to figure it out from that map.
   basically, the size of a variable instead of always being 1 can now be 1 + size of its instantiation.
   And the size of instantiation you'd have to figure out using the variables's type and those maps.

nadia: so I guess you could just use the size of the variable's type for simplicity...
   (and not the size of the sub)
   it's not quite the same, but idk which one would work better

darya: ok. we’ll mess around with some things and see what happens


-}






{-

But either way, I feel like there are two somewhat orthogonal questions here: 

    1) should we search by unifying goal with component return types vs unifying the whole type
        ===> unifying (a -> b) vs (b)
        
    2) when should we search for only application terms vs decompose an arrow goal into a lambda
        ===> do we use $ or do it manually (approach 2)

Re 2: in Synquid there are these two modes, where it searchers for 
  I-terms (lambda terms)
  E-terms (application terms). 

When it's in the first mode, and the goal is a -> b it will synthesize a
lambda, adding the a to the context and making b the goal. 

When it's in the second mode, then goal (a -> b) means "find a variable of type (a->b) or
application of (c -> a -> b) to a (c)". 

And switching between these modes happens according to the rules of bidirectional type-checking.
    ====> what is bidirectional type-checking (apparently we read about this? lol )
                                                    - synquid paper? 

Whereas H+ works differently: essentially it's only in the I-mode once for the
very top goal, and then once it added all the query args to the context, it
stays in the E-mode forever. That's why it never synthesizes inner lambdas

(instead filling HO args with variables and partial applications. It's important 
to decide which of these approaches you want to take, because
both of them stick to a different kind of normal form (one is fully
eta-expanded, the other one is the opposite except at the top level)

Because if you don't stick with a normal form, your search will be very slow,
because it will be enumerating a ton of redundant terms

I would suggest that @Darya Verzhbinsky and @Daniel Wang work out all of these
options in a google doc: take your running example with just a minimal set of
components (maybe +1 or 2 if the search space ends up being too small), and
then work out how the search is progressing and which terms it's enumerating,
so that we can stare at it

This actually ends up being an interesting problem! I didn't realize




I-term 
  query: "(a->b) -> a -> b"

  arg0: a -> b
  arg1: a

  goal: b

  return (\arg0 arg1 -> ...program...)
  always return a lambda (or just a component, if there's no arguments)


E-term
  query: "(a->b) -> (a -> b)"
  either you find some component :: (a->b) -> (a -> b)
  and return it

  or split the goal up into
    prog1 :: gamma -> (a->b) -> (a -> b)
    prog2 :: gamma
  
  return \arg0 arg1 -> (prog1 prog2)
  always return a partial application (or just a component, if goal is found in the environment)


note: using $ in the I-term mode is the same as synthesizing E-term
  except you return (\arg0 arg1 -> prog1 $ prog2) instead of (prog1 prog2)

yeah, so I would compare: 
    1) synquid vs H+ approach to i-terms and e-terms enumeration
      synquid:  
      H+:       lambda for top layer, and after that only look for full types 
                (don't split up the arrow types of the arguments to components)

      Whereas H+ works differently: essentially it's only in the I-mode once for the
      very top goal, and then once it added all the query args to the context, it
      stays in the E-mode forever. That's why it never synthesizes inner lambdas

    2) synquid's approach to top-down vs your approach to top-down + $
      synquid:    
      ours + $: recurse on component arguments. when you see an arrow type as a goal, always split it up and add the arguments to the env (this creates lambdas)

they are not quite orthogonal I think but you'll figure it out

-----------
For your comment about comparison, we want to make sure we understand the difference between each thing you think we should be comparing. 

For the query "Maybe (a->b) -> a -> b", compare: 

    1) synquid vs H+ approach to i-terms and e-terms enumeration
      `H+`: lambda for top layer, and after that only look for full types (don't split up the arrow types of the arguments to components)

    2) synquid's approach to top-down vs your approach to top-down + $
      `ours + $`: recurse on component arguments. when you see an arrow type as a goal, always split it up and add the arguments to the env (this creates lambdas)

For Synquid, what particular part of it do you want us to compare to the other thing? 
I think we're still kind of confused about what exactly Synquid does. We've discussed what parts of it do in different contexts but we're
unsure of the complete alogirthm. 

Our best understanding of Synquid is that it first tries to top-down synthesize E-terms as far as it can go. 
When that doesn't work (maybe when the type gets too big? don't know) it switches
and tries to bottom-up build up an I-term for the remaining goal.

This came from paraphrasing the following excerpt from the synquid paper:

In a bidirectional system, analyzing a program starts with propagating its top-level type annotation top-down using checking rules, until the system encounters a term t to which no checking rule applies. At this point the system switches to bottom-up mode, infers the type T ′ of t, and checks if T ′ is a subtype of the goal type; if the check fails, t is rejected. Bidirectional type propagation is “all-or-nothing”: once a checking problem for a term cannot be decomposed perfectly into checking problems for its subterms, the system abandons all information about the goal type and switches to purely bottom-up inference. 

----------------------------------------------------------------------------------------

----------------------
---- I TERM STUFF ----
----------------------

* Query: "Maybe (a->b) -> a -> b"
* Env: 
  arg0     :: Maybe (a->b)
  arg1     :: a
  fromJust :: <tau0> . Maybe tau0 -> tau0
  $        :: (tau1 -> tau2) -> tau1 -> tau2

{b}             fromJust (?? :: Maybe b)
    args = [Maybe b]
{Maybe b}       fromJust (?? :: Maybe (Maybe b))  
    args = [Maybe (Maybe b)]
    => reject because of excess in quota

{b}             $ (?? :: tau1 -> b) (?? :: tau1)
{tau1 -> b}     
  arg2     :: tau1
    {b}         fromJust (?? :: Maybe (Maybe b))                  

=> this entire approach fails since we never get Maybe (tau1 -> b) as a goal

----------------------
---- E TERM STUFF ----
----------------------

* Query: "Maybe (a->b) -> a -> b"
* Env: 
  arg0     :: Maybe (a->b)
  arg1     :: a
  fromJust :: <tau0> . Maybe tau0 -> tau0
  $        :: (tau1 -> tau2) -> tau1 -> tau2

{Maybe (a->b) -> a -> b}
  i guess [a->b]fromJust unifies?
  so return the e-term: fromJust


------------------
---- H+ STUFF ----
------------------

* Query: "Maybe (a->b) -> a -> b"
* Env: 
  arg0     :: Maybe (a->b)
  arg1     :: a
  fromJust :: <tau0> . Maybe tau0 -> tau0
  $        :: (tau1 -> tau2) -> tau1 -> tau2






In a bidirectional system, analyzing a program starts with propagating its
top-level type annotation top-down using checking rules, until the system
encounters a term t to which no checking rule applies. At this point the
system switches to bottom-up mode, infers the type T ′ of t, and checks if T ′
is a subtype of the goal type; if the check fails, t is rejected.
Bidirectional type propagation is “all-or-nothing”: once a checking problem
for a term cannot be decomposed perfectly into checking problems for its
subterms, the system abandons all information about the goal type and switches
to purely bottom-up inference. 



Re 2: in Synquid there are these two modes, where it searchers for 
  I-terms (lambda terms)
  E-terms (application terms). 

When it's in the first mode, and the goal is a -> b it will synthesize a
lambda, adding the a to the context and making b the goal. 

When it's in the second mode, then goal (a -> b) means "find a variable of type (a->b) or
application of (c -> a -> b) to a (c)". 







synquid paper: https://arxiv.org/pdf/1510.08419











-------------------------

Yeah, I think what you said makes sense, except I think you switched the I-terms and
E-terms around above. More precisely, Synquid's search has two modes, the I-mode and the E-mode.

    * In I-mode:

    - if the goal is an arrow type T -> S , decompose it, i.e. synthesize a lambda 
      by adding x:T to the context and changing goal to S
    - if the goal is not an arrow, switch to E-mode

    * In E-mode, given the goal T we always synthesize a variable or an application:

    - if there is (x: T )in the context, then we can return that variable
    - or we can synthesize an application by synthesizing both 

            a) a term of type alpha -> T in E-mode 
            b) a term of type alpha in I-mode

(of course you want to first synthesize the LHS and figure out what alpha is, at least 
partially) and then proceed to synthesize the argument)

This way, all the LHSs of applications are always themselves variables or 
applications (not lambdas! cause that would be a redex), whereas all RHSs 
i.e. arguments are lambdas if they are HO and variables/applications if 
they are FO.

So, if we take this as the baseline, then by 

    1) synquid vs H+ I mean: the approach described above vs a modification where 
   you don't switch back to I-mode to synthesize arguments, but just keep using 
   E-mode forever once you switched to it the first time. If you do that, 
   then all your HO arguments will come out as application-terms rather than lambdas.

      * way 1: do the synquid thing (switching back and forth between e and i mode)
      * way 2: only do i mode 1 time, and then switch to emode forever

TODO work on this question
    2) is the difference in how the E-mode is implemented. Instead of doing the E-mode 
   like above (i.e. generate variable of goal type T or application of 
   alpha -> T to alpha) you can say "just give me all the variables of types 
   A1 -> ... -> An -> R" where `R` unifies with `T` for n >= 0, and then for 
   each such variable, I create n new subgoals A1, ..., An. Now, each one of 
   those could be solved using I-mode or E-mode, depending on what you choose 
   for dimension 1.

These two different approaches to 2 seem to be equivalent, even without $, when 
type vars cannot be instantiated with arrows. But when they can, then approach 
2 seems less powerful, essentially because a goal types like beta -> alpha -> b  
can unify with a component types like A -> gamma by substituting 
beta := A, gamma := alpha -> b and you just don't get this unification 
if you simply ask for a component whose return type unifies with b. You 
might need its return type to unify with ... -> b! $ seems to restore this power, 
but I'm afraid it's at the cost of a lot of redundancy (but that's why I want to 
see these worked examples, to see if that's true)

Hm, actually, I think I finally have a crisp description of why I don't like this 
second approach in the a higher-order setting. Essentially you are saying 
"give me all components whose return type unifies with the goal". But what does 
"return type" mean? In a monomorphic or first-order setting it's clear: it's the 
type after the last arrow. But in a fully higher-order polymorphic setting it's 
not at all clear, because in the component's signature the type after the last 
arrow can be a type variable, and this type variable can be substituted with an 
arrow type, so the notion of "return type" isn't really well-defined. And that's 
why I don't think this approach is a good fit for this setting. (But feel free to 
argue with me!)


-----------
Environment 1:
  fromJust :: <tau0> . Maybe tau0 -> tau0

query 1: "arg0:Maybe (a -> b) -> arg1:a -> b"
    solution: "fromJust arg0 $ arg1"


way 1: 

- start in i mode (split args)
- do e mode for the return type
- if the whole type (not return type) doesn't unify with anything in the environment, split into two goals
  + a term of type alpha -> T in E-mode (only synth)
  + a term of type alpha in I-mode (split args)

--------
* In I-mode:

    - if the goal is an arrow type T -> S , decompose it, i.e. synthesize a lambda 
      by adding x:T to the context and changing goal to S
    - if the goal is not an arrow, switch to E-mode

* In E-mode, given the goal T we always synthesize a variable or an application:

  - if there is (x: T )in the context, then we can return that variable
  - or we can synthesize an application by synthesizing both 

      a) a term of type alpha -> T in E-mode 
      b) a term of type alpha in I-mode
--------

query 1: "arg0:Maybe (a -> b) -> arg1:a -> b" (i-mode)

* add args to the environment. new env: 
  arg0     :: Maybe (a->b)
  arg1     :: a
  fromJust :: <tau0> . Maybe tau0 -> tau0

query 2: b (e-mode)

* nothing in env has type that unifies entirely with b
* split into two goals (3a and 3b)
  alpha -> b      3a
  alpha           3b

query 3a: alpha -> b (e-mode)

* fromJust unifies with this:  [b] fromJust :: Maybe b -> b
    return fromJust

query 3b: alpha (i-mode)

* now alpha maps to (Maybe b) (from query 3a)
* not an arrow type, so switch to e-mode
(e-mode)
* nothing directly unifies
* split into 2 goals (4a and 4b)

query 4a: (beta -> Maybe b) (e-mode)

* fromJust unifies with this:  [Maybe b] fromJust :: Maybe (Maybe b) -> (Maybe b)
    return fromJust

query 4b: beta (i-mode)

* now beta maps to (Maybe (Maybe b)) (from query 4a)
* not an arrow type, so switch to e-mode
(e-mode)

==> this will continue forever, so at some point it stops because of reaching size limit. 
==> go back to query 3a and restart with 2 new queries (5a and 5b)
    beta -> (alpha -> b)      5a
    beta                      5b

query 5a: beta -> (alpha -> b) (e-mode)

* fromJust unifies with this:  [alpha -> b] fromJust :: Maybe (alpha -> b) -> (alpha -> b)
    return fromJust

query 5b: beta (i-mode)

  arg0     :: Maybe (a->b)
  arg1     :: a
  fromJust :: <tau0> . Maybe tau0 -> tau0

* now beta maps to (Maybe (alpha -> b))
* not an arrow type, so switch to e-mode
(e-mode)
* arg0 unifies with goal: [a] arg0 :: Maybe (a -> b)
    return arg0

back to query 3b (alpha) (i-mode)

* because of query 5b, alpha => a
* arg1 unifies with this
    return arg1

==> now we get solution: fromJust arg0 arg1 (yay)

----------------------
way 2: 

--------
- start in i mode (split args)
- switch to e mode forever
--------

  ==> for query 1, it will be the same, since in way 1, whenever we started with i-mode,
  we never were searching for an arrow type and so it always just switched it directly
  to e-mode. so same thing as just always doing e-mode.

  would return: fromJust arg0 arg1
--------




----------------------
----------------------
Environment 2:
  map :: <tau0> . <tau1> . (tau0 -> tau1) -> [tau0] -> [tau1]
  ,   :: <tau0> . <tau1> . tau0 -> tau1 -> (tau0, tau1)
  

query 2: "[a] -> [b] -> [[(a,b)]]"
    solution: "\xs ys -> Data.List.map (\x -> Data.List.map ((,) x) ys) xs"
    solution: "\xs ys -> Data.List.map (\x -> Data.List.map (\y' -> (x,y')) ys) xs"


----------------------
way 1: 

--------
- start in i mode (split args)
- do e mode for the return type
- if the whole type (not return type) doesn't unify with anything in the environment, split into two goals
  + a term of type alpha -> T in E-mode (only synth)
  + a term of type alpha in I-mode (split args)

--------
* In I-mode:

    - if the goal is an arrow type T -> S , decompose it, i.e. synthesize a lambda 
      by adding x:T to the context and changing goal to S
    - if the goal is not an arrow, switch to E-mode

* In E-mode, given the goal T we always synthesize a variable or an application:

  - if there is (x: T )in the context, then we can return that variable
  - or we can synthesize an application by synthesizing both 

      a) a term of type alpha -> T in E-mode 
      b) a term of type alpha in I-mode
--------


query 1: "[a] -> [b] -> [[(a,b)]]" (i-mode)

* add args to the environment. new env: 
  arg0 :: [a]
  arg1 :: [b]
  map :: <tau0> . <tau1> . (tau0 -> tau1) -> [tau0] -> [tau1]
  ,   :: <tau0> . <tau1> . tau0 -> tau1 -> (tau0, tau1)

* search for [[(a,b)]] in e-mode

query 2: [[(a,b)]] (e-mode)

* nothing unifies with [[(a,b)]]
* creates new goals:
  alpha -> [[(a,b)]]  3a (e-mode)
  alpha               3b (i-mode)
  
query 3a: alpha -> [[(a,b)]] (e-mode)

* nothing unifies
* creates new goals: 

    beta -> (alpha -> [[(a,b)]])   4a (e-mode)
    beta                           4b (i-mode)

query 4a: beta -> (alpha -> [[(a,b)]]) (e-mode)

* map unifies with goal to 
    [tau1 ==> [(a,b)]] map  :: <tau0> . <tau1> . (tau0 -> tau1) -> [tau0] -> [tau1]
  with
    alpha ==> [tau0]
    beta  ==> (tau0 -> [(a,b)])
* return map


query 4b: beta (i-mode)

* in 4a, beta ==> (tau0 -> [(a,b)])
* split into arguments. new env: 
  arg0 :: [a]
  arg1 :: [b]
  arg2 :: tau0
  map :: <tau0> . <tau1> . (tau0 -> tau1) -> [tau0] -> [tau1]
  ,   :: <tau0> . <tau1> . tau0 -> tau1 -> (tau0, tau1)
* find new goal [(a,b)]

query 5: [(a,b)] (i-mode)

* not arrow type, switch to (e-mode)
(e-mode)
* arg2 unifies 
  ==> this would lead to (\arg2 -> arg2) in query 4,
      which makes an identity function, map (\arg2 -> arg2) :: [[(a,b)]] -> [[(a,b)]], in query 3
      and we get no progress with the goal
  ==> split it up into 2 goals instead since this failed
* create new goals:

    gamma -> [(a,b)]   6a (e-mode)
    gamma              6b (i-mode)



query 6a: gamma -> [(a,b)]   6a (e-mode)

* arg2 unifies
  ==> this would lead to (\arg2 -> arg2) :: (gamma -> [(a,b)]) -> (gamma -> [(a,b)]) in query 4,
      which makes an identity function, map (\arg2 -> arg2) :: [gamma -> [(a,b)]] -> [gamma -> [(a,b)]], in query 3
      and turning the original goal from [[(a,b)]] to [gamma -> [(a,b)]]
      isn't progressing towards what we want
      (it will end up going down a rabbit hole that will eventually fail because of reaching too big a size)
  ==> split it up into 2 goals instead since this failed
* create new goals: 

    delta -> gamma -> [(a,b)]   7a (e-mode)
    delta                       7b (i-mode)
    
query 7a: delta -> gamma -> [(a,b)]   7a (e-mode)

* map unifies with goal to 
    [tau3 ==> (a,b)] map  :: <tau2> . <tau3> . (tau2 -> tau3) -> [tau2] -> [tau3]
  with
    delta ==> tau2 -> (a,b)
    gamma ==> [tau2]
* return map

query 7b: delta                       7a (i-mode)

* from 7a, delta ==> tau2 -> (a,b)
* split up args and add to env. new env: 
  arg0 :: [a]
  arg1 :: [b]
  arg2 :: tau0
  arg3 :: tau2
  map :: <tau0> . <tau1> . (tau0 -> tau1) -> [tau0] -> [tau1]
  ,   :: <tau0> . <tau1> . tau0 -> tau1 -> (tau0, tau1)
* new goal: (a,b) (i-mode)

query 8: (a,b) (i-mode)

* not arrow type, switch to (e-mode)
(e-mode)
* arg2 unifies
  ==> this would lead to (\arg3 -> arg2) :: tau2 -> (a,b) in query 7b,
      which makes an identity function, map (\arg3 -> arg2) :: [tau2] -> [(a,b)], in query 7a.
      It would lead to something that looks like this: 

        6a = map (\arg3 -> arg2) :: [tau2] -> [(a,b)]
        6b = ?? :: gamma ~ [tau2]
        6b = arg0 :: [a]
        5 = map (\arg3 -> arg2) arg0 :: [(a,b)]

        4a = map
        4b = \arg2 -> map (\arg3 -> arg2) arg0 :: (a,b) -> [(a,b)]

        3a = map (\arg2 -> map (\arg3 -> arg2) arg0)
        3b = (?? :: beta ~ (a,b) -> [(a,b)])
              --> requries finding a program for [(a,b)]
              --> which we will do anyway in the happy path
              --> with far less size

        2 = map (\arg2 -> map (\arg3 -> arg2) arg0) (?? :: (a,b) -> [(a,b)])
        1 = \arg0 arg1 -> map (\arg2 -> map (\arg3 -> arg2) arg0) (?? :: (a,b) -> [(a,b)])

      This will eventually either never find something, or get cut off because of size limitations.

  ==> try next component in env
* arg3 unifies
  ==> this would lead to (\arg3 -> arg3), which will again not advance our goal 
  ==> split it up into 2 goals instead since this failed
* create new goals:

    epsilon -> (a,b)     9a (e-mode)
    epsilon              9b (i-mode)

query 9a: epsilon -> (a,b)    9a (e-mode)

* arg2 and arg3 unify, but for similar reasons to above, will not progress the goal
  so we can ignore these and keep going

* create new goals: 

  zeta -> epsilon -> (a,b)     10a (e-mode)
  zeta                         10b (i-mode)

query 10a: zeta -> epsilon -> (a,b)    10a (e-mode)

* unifies with (,) into
    [tau4 ==> a, tau5 ==> b](,) <tau4> . <tau5> . tau4 -> tau5 -> (tau4, tau5)
    where
  zeta ==> a
  epsilon ==> b

* Return (,)

query 10b: zeta                        10b (i-mode)

* from 10a, zeta ==> a
* no arrow, turn to (e-mode)
(e-mode)
* unifies with arg2 :: tau0
return arg2


query 9b: epsilon             9b (i-mode)

* from 10a, epsilon ==> b
* no arrow, turn to (e-mode)
(e-mode)
* unifies with arg3 :: tau2
return arg3



query 6b: gamma              6b (i-mode)

* gamma ~ [b]
* not arrow type, go to e-mode
(e-mode)
* unifies with arg1
return arg1




query 3b: alpha (i-mode)

* alpha ~ [a]
* not arrow type, go to e-mode
(e-mode)
* unifies with arg0
return arg0

------------
PUTTING IT ALL TOGETHER:
------------

10a = (,)
10b = arg2
9a = 10a 10b
9a = (,) arg2
9b = arg3
8 = 9a 9b = ((,) arg2) arg3
7b = \arg3 -> ((,) arg2) arg3
7a = map
6a = 7a 7b = map (\arg3 -> ((,) arg2) arg3)
6b = gamma ~ [b] = arg1
5 = 6a 6b = (map (\arg3 -> ((,) arg2) arg3)) arg1
4b = \arg2 -> (map (\arg3 -> ((,) arg2) arg3)) arg1
4a = map
3a = 4a 4b = map (\arg2 -> (map (\arg3 -> ((,) arg2) arg3)) arg1)
3b = alpha ~ [tau0] ~ [a] = arg0
2 = 3a 3b = (map (\arg2 -> (map (\arg3 -> ((,) arg2) arg3)) arg1)) arg0
1 = \arg0 arg1 -> (map (\arg2 -> (map (\arg3 -> ((,) arg2) arg3)) arg1)) arg0
 
1 = \arg0 arg1 -> map (\arg2 -> map (\arg3 -> (arg2,arg3)) arg1) arg0

GOT OUR SOLUTION yay go team (!!!!!!) (!!!!!!!!!!!)


map (\x -> map ((,) x) ys) xs
map (flip map ys . (,)) xs

\xs ys -> map (\x -> map ((,) x) ys) xs

flip (map . flip (map . (,)))

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

(\x y -> f y x)

----------------------
way 2: 

--------
- start in i mode (split args)
- switch to e mode forever
--------


query 1: "[a] -> [b] -> [[(a,b)]]" (i-mode)

* add args to the environment. new env: 
  arg0 :: [a]
  arg1 :: [b]
  map :: <tau0> . <tau1> . (tau0 -> tau1) -> [tau0] -> [tau1]
  ,   :: <tau0> . <tau1> . tau0 -> tau1 -> (tau0, tau1)

* search for [[(a,b)]] in e-mode

query 2: [[(a,b)]] (e-mode)

* nothing unifies with [[(a,b)]]
* creates new goals:
  alpha -> [[(a,b)]]  3a
  alpha               3b

query 3a: alpha -> [[(a,b)]] (e-mode)

* nothing unifies
* creates new goals: 

    beta -> (alpha -> [[(a,b)]])   4a
    beta                           4b


query 4a: beta -> (alpha -> [[(a,b)]]) (e-mode)

* map unifies with goal to 
    [tau1 ==> [(a,b)]] map  :: <tau0> . <tau1> . (tau0 -> tau1) -> [tau0] -> [tau1]
  with
    alpha ==> [tau0]
    beta  ==> (tau0 -> [(a,b)])
* return map

query 4b: beta (e-mode)

* in 4a, beta ==> (tau0 -> [(a,b)])
* nothing unifies
* create new goals: 
    gamma -> (tau0 -> [(a,b)])   5a
    gamma                        5b
    

query 5a: gamma -> tau0 -> [(a,b)] (e-mode)

* map unifies with goal to 
    [tau3 ==> (a,b)] map  :: <tau2> . <tau3> . (tau2 -> tau3) -> [tau2] -> [tau3]
    tau0 ==> [tau2]
  with
    gamma ==> (tau2 -> (a,b))
* return map

query 5b: gamma (e-mode)

* from 5a, gamma ==> (tau2 -> (a,b))
* nothing unifies, create 2 new goals
    delta -> (tau2 -> (a,b))  6a
    delta                     6b

query 6a: delta -> (tau2 -> (a,b)) (e-mode)

* (,) unifies with goal to 
  [tau4 ==> a, tau5 ==> b] <tau4> . <tau5> . tau4 -> tau5 -> (tau4, tau5)
    where
  delta ==> a
  tau2 ==> b
* return (,)

query 6b: delta (e-mode)

* from 6a, delta ==> a
* nothing unifies with a
* split into new goals: 

  epsilon -> a   7a
  epsilon        7b

query 7a: epsilon -> a (e-mode)

* nothing unfies
* split into new goals
  zeta -> (epsilon -> a)   
  zeta

===> here, we are never going to get something that unifies with anything looking
     like (T1 -> T2 -> ... -> Tn -> b)

     => would need a lambda to split this up! otherwise, it won't ever be able to synthesize


query 7b: epsilon (e-mode)

* WE FAILED A PREVIOUS STEP - WON'T GET HERE

query 3b: alpha (e-mode)

* WE FAILED A PREVIOUS STEP - WON'T GET HERE

----------------
----------------
----------------

CONCENSUS for question 1: we like Synquid way better because it solves both types of problems 
  => we were able to solve both types of queries
  => for query 1, the second way was the same as first
  => for query 2. the second way couldn't even solve it 



--------------------------------------------------------------------------------
QUESTION 2: 

TODO work on this question
    2) is the difference in how the E-mode is implemented. Instead of doing the E-mode 
   like above (i.e. generate variable of goal type T or application of 
   alpha -> T to alpha) you can say "just give me all the variables of types 
   A1 -> ... -> An -> R" where `R` unifies with `T` for n >= 0, and then for 
   each such variable, I create n new subgoals A1, ..., An. Now, each one of 
   those could be solved using I-mode or E-mode, depending on what you choose 
   for dimension 1.

    e-mode above: 
    - either find something that unifies with T
    - split into 
          alpha -> T
          alpha

    e-mode now:
    - just check for components whose return type unifies with T
    - make new subgoals for each argument of the components (or 
      the part of the type that didn't unify with T)
        => make (i-mode)
    just give me all the variables of types A1 -> ... -> An -> R
    - 









  T :: (Int -> Bool)
  f :: Bool -> Char -> R:(Int -> Bool)

PREVIOUS:
i-mode
  * if function type
    split up the args
    add them to env
    get new goal (i-mode)
  * if not function type, go to e-mode
e-mode
  * if T in env, return that
  * if T not in env
  alpha -> T   (e-mode)
  alpha        (i-mode)

ALTERNATIVE:
i-mode
  * if function type
    split up the args
    add them to env
    get new goal (i-mode)
  * if not function type, go to e-mode
e-mode
  * if anything's return type unifies with T,
    * create new goals based on the args
    * return that comp + args
  * else
    * do $ stuff (have it unify with $ basically)

----

----

These two different approaches to 2 seem to be equivalent, even without $, when 
type vars cannot be instantiated with arrows. But when they can, then approach 
2 seems less powerful, essentially because a goal types like beta -> alpha -> b  
can unify with a component types like A -> gamma by substituting 
beta := A, gamma := alpha -> b and you just don't get this unification 
if you simply ask for a component whose return type unifies with b. You 
might need its return type to unify with ... -> b! $ seems to restore this power, 
but I'm afraid it's at the cost of a lot of redundancy (but that's why I want to 
see these worked examples, to see if that's true)

Hm, actually, I think I finally have a crisp description of why I don't like this 
second approach in the a higher-order setting. Essentially you are saying 
"give me all components whose return type unifies with the goal". But what does 
"return type" mean? In a monomorphic or first-order setting it's clear: it's the 
type after the last arrow. But in a fully higher-order polymorphic setting it's 
not at all clear, because in the component's signature the type after the last 
arrow can be a type variable, and this type variable can be substituted with an 
arrow type, so the notion of "return type" isn't really well-defined. And that's 
why I don't think this approach is a good fit for this setting. (But feel free to 
argue with me!)


------------------------
new (e-mode) way:
------------------------

  T :: Maybe (a -> b) -> a -> b


query 1: "arg0:Maybe (a -> b) -> arg1:a -> b" (i-mode)

* add args to the environment. new env: 
  arg0     :: Maybe (a->b)
  arg1     :: a
  fromJust :: <tau0> . Maybe tau0 -> tau0
  $        :: <tau0> . <tau1> . (tau0 -> tau1) -> tau0 -> tau1

query 2: b (e-mode) (part 1)

* return type of fromJust (tau0) unifies with b
  [b] fromJust :: Maybe b -> b
* make the following new goals: 
  Maybe b             3a (i-mode)

query 3a: Maybe b (i-mode)

* not a function type, switch to e-mode
(e-mode)
* return type of fromJust (tau0) unifies with (Maybe b)
  [Maybe b] fromJust :: Maybe (Maybe b) -> (Maybe b)
* make the following new goals: 
  Maybe (Maybe b)             4a (i-mode)

query 4a: Maybe (Maybe b) (i-mode)

* not a function type, switch to e-mode
(e-mode)
* return type of fromJust (tau0) unifies with (Maybe b)
  [Maybe b] fromJust :: Maybe (Maybe b) -> (Maybe b)
* make the following new goals: 
  Maybe (Maybe b)             4a (i-mode)

===> this will continue on forever! or it will size out at some point
===> go back to query 2 and look at the next component

query 2: b (e-mode) (part 2)

* return type of $ (tau3) unifies with b
    [b] $        :: <tau2> . (tau2 -> b) -> tau2 -> b
* maybe the following new goals: 
    tau2 -> b       5a (i-mode)
    tau2            5b (i-mode)

query 5a: tau2 -> b (i-mode)

* is a function type
    * split up the args [tau2]
    * add them to the env

  arg0     :: Maybe (a->b)
  arg1     :: a
  arg2     :: tau2
  fromJust :: <tau0> . Maybe tau0 -> tau0
  $        :: <tau0> . <tau1> . (tau0 -> tau1) -> tau0 -> tau1
    
    * create new goal b (6)

query 6: b    (i-mode)

* not a function type
* switch to (e-mode)
(e-mode)
* unifies with arg2
    ==> this would lead to (\arg2 -> arg2) in 5a, which doesn't help anything
* unifies with [b] fromJust :: Maybe b -> b
    ==> this would lead to (\arg2 -> fromJust (?? :: Maybe b))
* unifies with [b] $ :: <tau4> . (tau4 -> b) -> tau4 -> b
    ==> this would lead to (\arg2 -> (?? :: tau4 -> b) $ (?? :: tau4))
* create new goals:

    tau4 -> b     (7a)
    tau4          (7b)

query 7a: tau4 -> b (i-mode)




this will never be able to synthesize ([a -> b]fromJust). whenever we
get the $ equivalent of splitting into goals alpha -> T and alpha, we
are in i-mode (since alpha -> T is an argument to $, and the synquid way
synthesizes arguments in i-mode). so the goal alpha -> T gets split
immediately, putting alpha as a component in the environment and we are back
to searching for T. meaning our goal is never alpha -> T, so we never get our
([a -> T]fromJust).



query 7b: tau4      (i-mode)



query 5b: tau2 (i-mode)

------------
idea (might be wrong): - this way is bad because almost every query checks against the $
  which leads to a lot of dumb enumeration
  - whereas the alpha -> T and alpha way leads to controlling
  when you use the $ functionality

fromJust ??, fromJust (fromJust ??), fromJust (frommJust ?? $ ??), .....
are all checked before
fromJust ?? $ ??
------------


-}































{-



### Example 2 - `arg0:[a] -> arg1:[b] -> [[(a,b)]]`

Environment:
```
map :: <tau0> . <tau1> . (tau0 -> tau1) -> [tau0] -> [tau1]
,   :: <tau0> . <tau1> . tau0 -> tau1 -> (tau0, tau1)
```


#### Way 1:

**Query 1:** `[a] -> [b] -> [[(a,b)]]` (i-mode)

* add args to the environment
* new env: 
```
arg0 :: [a]
arg1 :: [b]
map  :: <tau0> . <tau1> . (tau0 -> tau1) -> [tau0] -> [tau1]
(,)  :: <tau0> . <tau1> . tau0 -> tau1 -> (tau0, tau1)
```

* search for `[[(a,b)]]` in e-mode

**Query 2:** `[[(a,b)]]` (e-mode)

* nothing unifies with `[[(a,b)]]`
* creates new goals:
    * `alpha -> [[(a,b)]]`  (3a) (e-mode)
    * `alpha `              (3b) (i-mode)
  
**Query 3a:** `alpha -> [[(a,b)]]` (e-mode)

* nothing unifies
* creates new goals: 

    `beta -> (alpha -> [[(a,b)]])`   (4a) (e-mode)
    `beta`                           (4b) (i-mode)

**Query 4a:** `beta -> (alpha -> [[(a,b)]])` (e-mode)

* `map` unifies with goal to 
```
[tau1 ==> [(a,b)]]
map  :: <tau0> . <tau1> . (tau0 -> tau1) -> [tau0] -> [tau1]
```
  with
```
alpha ==> [tau0]
beta  ==> (tau0 -> [(a,b)])
```
* return `map`


**Query 4b:** beta (i-mode)

* in 4a, `beta ==> (tau0 -> [(a,b)])`
* split into arguments. new env: 
```
arg0 :: [a]
arg1 :: [b]
arg2 :: tau0
map  :: <tau0> . <tau1> . (tau0 -> tau1) -> [tau0] -> [tau1]
(,)  :: <tau0> . <tau1> . tau0 -> tau1 -> (tau0, tau1)
```
* find new goal `[(a,b)]`

**Query 5:** `[(a,b)]` (i-mode)

* not arrow type, switch to (e-mode)
(e-mode)
* `arg2` unifies 

==> this would lead to `\arg2 -> arg2` in **Query 4**, which makes an identity function, `map (\arg2 -> arg2) :: [[(a,b)]] -> [[(a,b)]]`, in **Query 3** and we get no progress with the goal

==> split it up into 2 goals instead since this failed
* create new goals:

    * `gamma -> [(a,b)]`   (6a) (e-mode)
    * `gamma`              (6b) (i-mode)



**Query 6a:** `gamma -> [(a,b)]` (e-mode)

* `arg2` unifies

==> this would lead to `\arg2 -> arg2` in Query 4, which makes an identity function, `map (\arg2 -> arg2) :: [gamma -> [(a,b)]] -> [gamma -> [(a,b)]]`, in Query 3 and turning the original goal from `[[(a,b)]]` to `[gamma -> [(a,b)]]` isn't progressing towards what we want (it will end up going down a rabbit hole that will eventually fail because of reaching too big a size)

==> split it up into 2 goals instead since this failed
* create new goals: 

    * `delta -> gamma -> [(a,b)]`   (7a) (e-mode)
    * `delta`                       (7b) (i-mode)
    
**Query 7a:** `delta -> gamma -> [(a,b)]`

* `map` unifies with goal to 

```
[tau3 ==> (a,b)] 
map  :: <tau2> . <tau3> . (tau2 -> tau3) -> [tau2] -> [tau3]
```
with
```
delta ==> tau2 -> (a,b)
gamma ==> [tau2]
```
* return `map`

**Query 7b:** `delta`

* from 7a, `delta ==> tau2 -> (a,b)`
* split up args and add to env. new env: 
```
arg0 :: [a]
arg1 :: [b]
arg2 :: tau0
arg3 :: tau2
map  :: <tau0> . <tau1> . (tau0 -> tau1) -> [tau0] -> [tau1]
(,)  :: <tau0> . <tau1> . tau0 -> tau1 -> (tau0, tau1)
```
* new goal: `(a,b)` (i-mode)

**Query 8:** `(a,b)` (i-mode)
HERE darya
* not arrow type, switch to (e-mode)
(e-mode)
* arg2 unifies
  ==> this would lead to (\arg3 -> arg2) :: tau2 -> (a,b) in Query 7b,
      which makes an identity function, map (\arg3 -> arg2) :: [tau2] -> [(a,b)], in Query 7a.
      It would lead to something that looks like this: 

        6a = map (\arg3 -> arg2) :: [tau2] -> [(a,b)]
        6b = ?? :: gamma ~ [tau2]
        6b = arg0 :: [a]
        5 = map (\arg3 -> arg2) arg0 :: [(a,b)]

        4a = map
        4b = \arg2 -> map (\arg3 -> arg2) arg0 :: (a,b) -> [(a,b)]

        3a = map (\arg2 -> map (\arg3 -> arg2) arg0)
        3b = (?? :: beta ~ (a,b) -> [(a,b)])
              --> requries finding a program for [(a,b)]
              --> which we will do anyway in the happy path
              --> with far less size

        2 = map (\arg2 -> map (\arg3 -> arg2) arg0) (?? :: (a,b) -> [(a,b)])
        1 = \arg0 arg1 -> map (\arg2 -> map (\arg3 -> arg2) arg0) (?? :: (a,b) -> [(a,b)])

      This will eventually either never find something, or get cut off because of size limitations.

  ==> try next component in env
* arg3 unifies
  ==> this would lead to (\arg3 -> arg3), which will again not advance our goal 
  ==> split it up into 2 goals instead since this failed
* create new goals:

    epsilon -> (a,b)     9a (e-mode)
    epsilon              9b (i-mode)

Query 9a: epsilon -> (a,b)    9a (e-mode)

* arg2 and arg3 unify, but for similar reasons to above, will not progress the goal
  so we can ignore these and keep going

* create new goals: 

  zeta -> epsilon -> (a,b)     10a (e-mode)
  zeta                         10b (i-mode)

Query 10a: zeta -> epsilon -> (a,b)    10a (e-mode)

* unifies with (,) into
    [tau4 ==> a, tau5 ==> b](,) <tau4> . <tau5> . tau4 -> tau5 -> (tau4, tau5)
    where
  zeta ==> a
  epsilon ==> b

* Return (,)

Query 10b: zeta                        10b (i-mode)

* from 10a, zeta ==> a
* no arrow, turn to (e-mode)
(e-mode)
* unifies with arg2 :: tau0
return arg2


Query 9b: epsilon             9b (i-mode)

* from 10a, epsilon ==> b
* no arrow, turn to (e-mode)
(e-mode)
* unifies with arg3 :: tau2
return arg3



Query 6b: gamma              6b (i-mode)

* gamma ~ [b]
* not arrow type, go to e-mode
(e-mode)
* unifies with arg1
return arg1




Query 3b: alpha (i-mode)

* alpha ~ [a]
* not arrow type, go to e-mode
(e-mode)
* unifies with arg0
return arg0

------------
PUTTING IT ALL TOGETHER:
------------

10a = (,)
10b = arg2
9a = 10a 10b
9a = (,) arg2
9b = arg3
8 = 9a 9b = ((,) arg2) arg3
7b = \arg3 -> ((,) arg2) arg3
7a = map
6a = 7a 7b = map (\arg3 -> ((,) arg2) arg3)
6b = gamma ~ [b] = arg1
5 = 6a 6b = (map (\arg3 -> ((,) arg2) arg3)) arg1
4b = \arg2 -> (map (\arg3 -> ((,) arg2) arg3)) arg1
4a = map
3a = 4a 4b = map (\arg2 -> (map (\arg3 -> ((,) arg2) arg3)) arg1)
3b = alpha ~ [tau0] ~ [a] = arg0
2 = 3a 3b = (map (\arg2 -> (map (\arg3 -> ((,) arg2) arg3)) arg1)) arg0
1 = \arg0 arg1 -> (map (\arg2 -> (map (\arg3 -> ((,) arg2) arg3)) arg1)) arg0
 
1 = \arg0 arg1 -> map (\arg2 -> map (\arg3 -> (arg2,arg3)) arg1) arg0

GOT OUR SOLUTION yay go team (!!!!!!) (!!!!!!!!!!!)







----------------------
way 2: 

--------
- start in i mode (split args)
- switch to e mode forever
--------


Query 1: "[a] -> [b] -> [[(a,b)]]" (i-mode)

* add args to the environment. new env: 
  arg0 :: [a]
  arg1 :: [b]
  map :: <tau0> . <tau1> . (tau0 -> tau1) -> [tau0] -> [tau1]
  ,   :: <tau0> . <tau1> . tau0 -> tau1 -> (tau0, tau1)

* search for [[(a,b)]] in e-mode

Query 2: [[(a,b)]] (e-mode)

* nothing unifies with [[(a,b)]]
* creates new goals:
  alpha -> [[(a,b)]]  3a
  alpha               3b

Query 3a: alpha -> [[(a,b)]] (e-mode)

* nothing unifies
* creates new goals: 

    beta -> (alpha -> [[(a,b)]])   4a
    beta                           4b


Query 4a: beta -> (alpha -> [[(a,b)]]) (e-mode)

* map unifies with goal to 
    [tau1 ==> [(a,b)]] map  :: <tau0> . <tau1> . (tau0 -> tau1) -> [tau0] -> [tau1]
  with
    alpha ==> [tau0]
    beta  ==> (tau0 -> [(a,b)])
* return map

Query 4b: beta (e-mode)

* in 4a, beta ==> (tau0 -> [(a,b)])
* nothing unifies
* create new goals: 
    gamma -> (tau0 -> [(a,b)])   5a
    gamma                        5b
    

Query 5a: gamma -> tau0 -> [(a,b)] (e-mode)

* map unifies with goal to 
    [tau3 ==> (a,b)] map  :: <tau2> . <tau3> . (tau2 -> tau3) -> [tau2] -> [tau3]
    tau0 ==> [tau2]
  with
    gamma ==> (tau2 -> (a,b))
* return map

Query 5b: gamma (e-mode)

* from 5a, gamma ==> (tau2 -> (a,b))
* nothing unifies, create 2 new goals
    delta -> (tau2 -> (a,b))  6a
    delta                     6b

Query 6a: delta -> (tau2 -> (a,b)) (e-mode)

* (,) unifies with goal to 
  [tau4 ==> a, tau5 ==> b] <tau4> . <tau5> . tau4 -> tau5 -> (tau4, tau5)
    where
  delta ==> a
  tau2 ==> b
* return (,)

Query 6b: delta (e-mode)

* from 6a, delta ==> a
* nothing unifies with a
* split into new goals: 

  epsilon -> a   7a
  epsilon        7b

Query 7a: epsilon -> a (e-mode)

* nothing unfies
* split into new goals
  zeta -> (epsilon -> a)   
  zeta

===> here, we are never going to get something that unifies with anything looking
     like (T1 -> T2 -> ... -> Tn -> b)

     => would need a lambda to split this up! otherwise, it won't ever be able to synthesize

Query 7b: epsilon (e-mode)

* WE FAILED A PREVIOUS STEP - WON'T GET HERE

Query 3b: alpha (e-mode)

* WE FAILED A PREVIOUS STEP - WON'T GET HERE


-}