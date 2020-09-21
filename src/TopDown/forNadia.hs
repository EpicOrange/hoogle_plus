{-

Hello! We have run a test and have put some logs into 2 files.

* query: `firstJust ("a -> [Maybe a] -> a")`
* components: `Data.Maybe.fromMaybe`, `Data.Maybe.listToMaybe`, `Data.Maybe.catMaybes`, `GHC.List.foldl`
* `new_size_way.txt`: this contains results of the new suggested size mechanism (includes size of instantiated type variables)
* `old_size_way.txt`: this contains results of the original size mechanism (ignores types)

The way the print statements are formatted is the following:

```
[entire goal at the point in time]              (size of program we're synthesizing)    actual goal type we're synthesizing
```

For example:

```
((GHC.List.foldl1' (?? :: alpha1)) (?? :: alpha0))     (size 1) goal: (a -> (a -> a))
```

means it's currently solving the leftmost hole `(?? :: alpha1)` which, after type substitutions, is the type `(a -> (a -> a))`. The size quota for this hole is 1. Let us know if you have any questions!

Here is what we have analyzed from these outputs: 

* you can clearly see the repetition of work that happens from size x to x+1, in both size ways, by looking at how the goals at the beginning of each new size are very similar to those of the previous size.

* in addition to the repetition of work, another big time sink is that because the new size way needs to get to larger sizes, it will try synthesizing much larger goal types to accomodate for the increased sub size. For example, if we are at size 7, and we're synthesizing a goal that requires being split up, it will continue to split that goal 6 times (into eventually `a6->a5->a4->a3->a2 ...`), because it has the quota to do so. In the older size way, this wouldn't happen because it was never given enough quota in the first place. So this is another reason we feel that  the new size way ends up being much slower. 






-}