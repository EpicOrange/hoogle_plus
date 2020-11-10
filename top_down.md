LOG
===

goal to synthesize:
  <b> . <a> . (Maybe (((a -> b))) -> (a -> b))
with components:
  (Data.Bool.&&) :: (Bool -> (Bool -> Bool))
  (Data.Bool.||) :: (Bool -> (Bool -> Bool))
  (Data.Eq./=) :: <a> . (@@hplusTC@@Eq (a) -> (a -> (a -> Bool)))
  (Data.Eq.==) :: <a> . (@@hplusTC@@Eq (a) -> (a -> (a -> Bool)))
  (Data.Function.$) :: <b> . <a> . (((a -> b)) -> (a -> b))
  (Data.Function.&) :: <b> . <a> . (a -> (((a -> b)) -> b))
  (Data.Function..) :: <c> . <b> . <a> . (((b -> c)) -> (((a -> b)) -> (a -> c)))
  (GHC.List.!!) :: <a> . ([a] -> (Int -> a))
  (GHC.List.++) :: <a> . ([a] -> ([a] -> [a]))
  @@hplusTCInstance@@0EqBool :: @@hplusTC@@Eq (Bool)
  @@hplusTCInstance@@0EqChar :: @@hplusTC@@Eq (Char)
  @@hplusTCInstance@@0EqDouble :: @@hplusTC@@Eq (Double)
  @@hplusTCInstance@@0EqFloat :: @@hplusTC@@Eq (Float)
  @@hplusTCInstance@@0EqInt :: @@hplusTC@@Eq (Int)
  @@hplusTCInstance@@0EqUnit :: @@hplusTC@@Eq (Unit)
  @@hplusTCInstance@@0NumDouble :: @@hplusTC@@Num (Double)
  @@hplusTCInstance@@0NumFloat :: @@hplusTC@@Num (Float)
  @@hplusTCInstance@@0NumInt :: @@hplusTC@@Num (Int)
  @@hplusTCInstance@@0OrdBool :: @@hplusTC@@Ord (Bool)
  @@hplusTCInstance@@0OrdChar :: @@hplusTC@@Ord (Char)
  @@hplusTCInstance@@0OrdDouble :: @@hplusTC@@Ord (Double)
  @@hplusTCInstance@@0OrdFloat :: @@hplusTC@@Ord (Float)
  @@hplusTCInstance@@0OrdInt :: @@hplusTC@@Ord (Int)
  @@hplusTCInstance@@0Show :: <b> . <a> . (@@hplusTC@@Show (a) -> (@@hplusTC@@Show (b) -> @@hplusTC@@Show ((Either (a) (b)))))
  @@hplusTCInstance@@0ShowBool :: @@hplusTC@@Show (Bool)
  @@hplusTCInstance@@0ShowChar :: @@hplusTC@@Show (Char)
  @@hplusTCInstance@@0ShowDouble :: @@hplusTC@@Show (Double)
  @@hplusTCInstance@@0ShowFloat :: @@hplusTC@@Show (Float)
  @@hplusTCInstance@@0ShowInt :: @@hplusTC@@Show (Int)
  @@hplusTCInstance@@0ShowUnit :: @@hplusTC@@Show (Unit)
  @@hplusTCInstance@@1Read :: <b> . <a> . (@@hplusTC@@Read (a) -> (@@hplusTC@@Read (b) -> @@hplusTC@@Read ((Either (a) (b)))))
  @@hplusTCInstance@@2Ord :: <b> . <a> . (@@hplusTC@@Ord (a) -> (@@hplusTC@@Ord (b) -> @@hplusTC@@Ord ((Either (a) (b)))))
  @@hplusTCInstance@@3Eq :: <b> . <a> . (@@hplusTC@@Eq (a) -> (@@hplusTC@@Eq (b) -> @@hplusTC@@Eq ((Either (a) (b)))))
  @@hplusTCInstance@@5Semigroup :: <b> . <a> . @@hplusTC@@Semigroup ((Either (a) (b)))
  @@hplusTCInstance@@8Eq :: <a> . (@@hplusTC@@Eq (a) -> @@hplusTC@@Eq (([a])))
  Cons :: <a> . (a -> ([a] -> {[a]|_v == (Cons x xs)}))
  Data.Bool.False :: Bool
  Data.Bool.True :: Bool
  Data.Bool.bool :: <a> . (a -> (a -> (Bool -> a)))
  Data.Bool.not :: (Bool -> Bool)
  Data.Bool.otherwise :: Bool
  Data.Either.Left :: <b> . <a> . (a -> Either (a) (b))
  Data.Either.Right :: <b> . <a> . (b -> Either (a) (b))
  Data.Either.either :: <c> . <b> . <a> . (((a -> c)) -> (((b -> c)) -> (Either (a) (b) -> c)))
  Data.Either.fromLeft :: <b> . <a> . (a -> (Either (a) (b) -> a))
  Data.Either.fromRight :: <b> . <a> . (b -> (Either (a) (b) -> b))
  Data.Either.isLeft :: <b> . <a> . (Either (a) (b) -> Bool)
  Data.Either.isRight :: <b> . <a> . (Either (a) (b) -> Bool)
  Data.Either.lefts :: <b> . <a> . ([Either (a) (b)] -> [a])
  Data.Either.partitionEithers :: <b> . <a> . ([Either (a) (b)] -> ([a] , [b]))
  Data.Either.rights :: <b> . <a> . ([Either (a) (b)] -> [b])
  Data.Function.const :: <b> . <a> . (a -> (b -> a))
  Data.Function.fix :: <a> . (((a -> a)) -> a)
  Data.Function.flip :: <c> . <b> . <a> . (((a -> (b -> c))) -> (b -> (a -> c)))
  Data.Function.id :: <a> . (a -> a)
  Data.Function.on :: <c> . <b> . <a> . (((b -> (b -> c))) -> (((a -> b)) -> (a -> (a -> c))))
  Data.List.group :: <a> . (@@hplusTC@@Eq (a) -> ([a] -> [[a]]))
  Data.Maybe.Just :: <a> . (a -> Maybe (a))
  Data.Maybe.Nothing :: <a> . Maybe (a)
  Data.Maybe.catMaybes :: <a> . ([Maybe (a)] -> [a])
  Data.Maybe.fromJust :: <a> . (Maybe (a) -> a)
  Data.Maybe.fromMaybe :: <a> . (a -> (Maybe (a) -> a))
  Data.Maybe.isJust :: <a> . (Maybe (a) -> Bool)
  Data.Maybe.isNothing :: <a> . (Maybe (a) -> Bool)
  Data.Maybe.listToMaybe :: <a> . ([a] -> Maybe (a))
  Data.Maybe.mapMaybe :: <b> . <a> . (((a -> Maybe (b))) -> ([a] -> [b]))
  Data.Maybe.maybe :: <b> . <a> . (b -> (((a -> b)) -> (Maybe (a) -> b)))
  Data.Maybe.maybeToList :: <a> . (Maybe (a) -> [a])
  Data.Tuple.curry :: <c> . <b> . <a> . ((((a , b) -> c)) -> (a -> (b -> c)))
  Data.Tuple.fst :: <b> . <a> . ((a , b) -> a)
  Data.Tuple.snd :: <b> . <a> . ((a , b) -> b)
  Data.Tuple.swap :: <b> . <a> . ((a , b) -> (b , a))
  Data.Tuple.uncurry :: <c> . <b> . <a> . (((a -> (b -> c))) -> ((a , b) -> c))
  GHC.Char.chr :: (Int -> Char)
  GHC.Char.eqChar :: (Char -> (Char -> Bool))
  GHC.Char.neChar :: (Char -> (Char -> Bool))
  GHC.List.all :: <a> . (((a -> Bool)) -> ([a] -> Bool))
  GHC.List.and :: ([Bool] -> Bool)
  GHC.List.any :: <a> . (((a -> Bool)) -> ([a] -> Bool))
  GHC.List.break :: <a> . (((a -> Bool)) -> ([a] -> ([a] , [a])))
  GHC.List.concat :: <a> . ([[a]] -> [a])
  GHC.List.concatMap :: <b> . <a> . (((a -> [b])) -> ([a] -> [b]))
  GHC.List.cycle :: <a> . ([a] -> [a])
  GHC.List.drop :: <a> . (Int -> ([a] -> [a]))
  GHC.List.dropWhile :: <a> . (((a -> Bool)) -> ([a] -> [a]))
  GHC.List.elem :: <a> . (@@hplusTC@@Eq (a) -> (a -> ([a] -> Bool)))
  GHC.List.filter :: <a> . (((a -> Bool)) -> ([a] -> [a]))
  GHC.List.foldl :: <b> . <a> . (((b -> (a -> b))) -> (b -> ([a] -> b)))
  GHC.List.foldl' :: <b> . <a> . (((b -> (a -> b))) -> (b -> ([a] -> b)))
  GHC.List.foldl1 :: <a> . (((a -> (a -> a))) -> ([a] -> a))
  GHC.List.foldl1' :: <a> . (((a -> (a -> a))) -> ([a] -> a))
  GHC.List.foldr :: <b> . <a> . (((a -> (b -> b))) -> (b -> ([a] -> b)))
  GHC.List.foldr1 :: <a> . (((a -> (a -> a))) -> ([a] -> a))
  GHC.List.head :: <a> . ([a] -> a)
  GHC.List.init :: <a> . ([a] -> [a])
  GHC.List.iterate :: <a> . (((a -> a)) -> (a -> [a]))
  GHC.List.iterate' :: <a> . (((a -> a)) -> (a -> [a]))
  GHC.List.last :: <a> . ([a] -> a)
  GHC.List.length :: <a> . ([a] -> Int)
  GHC.List.lookup :: <b> . <a> . (@@hplusTC@@Eq (a) -> (a -> ([(a , b)] -> Maybe (b))))
  GHC.List.map :: <b> . <a> . (((a -> b)) -> ([a] -> [b]))
  GHC.List.maximum :: <a> . (@@hplusTC@@Ord (a) -> ([a] -> a))
  GHC.List.minimum :: <a> . (@@hplusTC@@Ord (a) -> ([a] -> a))
  GHC.List.notElem :: <a> . (@@hplusTC@@Eq (a) -> (a -> ([a] -> Bool)))
  GHC.List.null :: <a> . ([a] -> Bool)
  GHC.List.or :: ([Bool] -> Bool)
  GHC.List.product :: <a> . (@@hplusTC@@Num (a) -> ([a] -> a))
  GHC.List.repeat :: <a> . (a -> [a])
  GHC.List.replicate :: <a> . (Int -> (a -> [a]))
  GHC.List.reverse :: <a> . ([a] -> [a])
  GHC.List.scanl :: <b> . <a> . (((b -> (a -> b))) -> (b -> ([a] -> [b])))
  GHC.List.scanl' :: <b> . <a> . (((b -> (a -> b))) -> (b -> ([a] -> [b])))
  GHC.List.scanl1 :: <a> . (((a -> (a -> a))) -> ([a] -> [a]))
  GHC.List.scanr :: <b> . <a> . (((a -> (b -> b))) -> (b -> ([a] -> [b])))
  GHC.List.scanr1 :: <a> . (((a -> (a -> a))) -> ([a] -> [a]))
  GHC.List.span :: <a> . (((a -> Bool)) -> ([a] -> ([a] , [a])))
  GHC.List.splitAt :: <a> . (Int -> ([a] -> ([a] , [a])))
  GHC.List.sum :: <a> . (@@hplusTC@@Num (a) -> ([a] -> a))
  GHC.List.tail :: <a> . ([a] -> [a])
  GHC.List.take :: <a> . (Int -> ([a] -> [a]))
  GHC.List.takeWhile :: <a> . (((a -> Bool)) -> ([a] -> [a]))
  GHC.List.uncons :: <a> . ([a] -> Maybe (((a , [a]))))
  GHC.List.unzip :: <b> . <a> . ([(a , b)] -> ([a] , [b]))
  GHC.List.unzip3 :: <c> . <b> . <a> . ([((a , b) , c)] -> (([a] , [b]) , [c]))
  GHC.List.zip :: <b> . <a> . ([a] -> ([b] -> [(a , b)]))
  GHC.List.zip3 :: <c> . <b> . <a> . ([a] -> ([b] -> ([c] -> [((a , b) , c)])))
  GHC.List.zipWith :: <c> . <b> . <a> . (((a -> (b -> c))) -> ([a] -> ([b] -> [c])))
  GHC.List.zipWith3 :: <d> . <c> . <b> . <a> . (((a -> (b -> (c -> d)))) -> ([a] -> ([b] -> ([c] -> [d]))))
  Nil :: <a> . {[a]|_v == (Nil)}
  Pair :: <b> . <a> . (a -> (b -> {(a , b)|_v == (Pair x y)}))
  Text.Show.show :: <a> . (@@hplusTC@@Show (a) -> (a -> [Char]))
  Text.Show.showChar :: (Char -> ([Char] -> [Char]))
  Text.Show.showList :: <a> . (@@hplusTC@@Show (a) -> ([a] -> ([Char] -> [Char])))
  Text.Show.showListWith :: <a> . (((a -> ([Char] -> [Char]))) -> ([a] -> ([Char] -> [Char])))
  Text.Show.showParen :: (Bool -> ((([Char] -> [Char])) -> ([Char] -> [Char])))
  Text.Show.showString :: ([Char] -> ([Char] -> [Char]))
  Text.Show.shows :: <a> . (@@hplusTC@@Show (a) -> (a -> ([Char] -> [Char])))
  Text.Show.showsPrec :: <a> . (@@hplusTC@@Show (a) -> (Int -> (a -> ([Char] -> [Char]))))
  arg0 :: Maybe (((a -> b)))
  arg1 :: a
  fst :: <b> . <a> . ((a , b) -> a)
  snd :: <b> . <a> . ((a , b) -> b)

Quota 1
=========

dfs has been entered 0 times
memo map looks like:
{
}

GOAL: (EMode | quota 1 | ?? :: b ~ b) is being seen for the first time

Quota 2
=========

dfs has been entered 1 time
memo map looks like:
{
}

GOAL: (EMode | quota 2 | ?? :: b ~ b) is being seen for the first time
  done with inEnv, split since quota (2) > 1
    GOAL: (EMode | quota 1 | ?? :: (alpha0 -> b) ~ (beta0 -> b)) is being seen for the first time
      unified (size 1): Data.Maybe.fromJust :: <a> . (Maybe (a) -> a) ~ (alpha0 -> b) via {"alpha0" ==> Maybe (b), "tau0" ==> b}
        GOAL: (IMode | quota 1 | ?? :: Maybe (b) ~ Maybe (b)) is being seen for the first time
          unified (size 1): Data.Maybe.Nothing :: <a> . Maybe (a) ~ Maybe (b) via {"alpha0" ==> Maybe (b), "tau0" ==> b, "tau1" ==> b}
      unified (size 1): Data.Tuple.fst :: <b> . <a> . ((a , b) -> a) ~ (alpha0 -> b) via {"alpha0" ==> (b , tau0), "tau1" ==> b}
        GOAL: (IMode | quota 1 | ?? :: (b , tau0) ~ (b , beta0)) is being seen for the first time
      unified (size 1): Data.Tuple.snd :: <b> . <a> . ((a , b) -> b) ~ (alpha0 -> b) via {"alpha0" ==> (tau1 , b), "tau0" ==> b}
        GOAL: (IMode | quota 1 | ?? :: (tau1 , b) ~ (beta0 , b)) is being seen for the first time
      unified (size 1): GHC.List.head :: <a> . ([a] -> a) ~ (alpha0 -> b) via {"alpha0" ==> [b], "tau0" ==> b}
        GOAL: (IMode | quota 1 | ?? :: [b] ~ [b]) is being seen for the first time
          unified (size 1): Nil :: <a> . {[a]|_v == (Nil)} ~ [b] via {"alpha0" ==> [b], "tau0" ==> b, "tau1" ==> b}
      unified (size 1): GHC.List.last :: <a> . ([a] -> a) ~ (alpha0 -> b) via {"alpha0" ==> [b], "tau0" ==> b}
        GOAL: (IMode | quota 1 | ?? :: [b] ~ [b]) has 1 solution in memo map
          retrieved (size 1): [] :: {[tau1]|_v == (Nil)}
      unified (size 1): fst :: <b> . <a> . ((a , b) -> a) ~ (alpha0 -> b) via {"alpha0" ==> (b , tau0), "tau1" ==> b}
        GOAL: (IMode | quota 1 | ?? :: (b , tau0) ~ (b , beta0)) is being seen for the first time
      unified (size 1): snd :: <b> . <a> . ((a , b) -> b) ~ (alpha0 -> b) via {"alpha0" ==> (tau1 , b), "tau0" ==> b}
        GOAL: (IMode | quota 1 | ?? :: (tau1 , b) ~ (beta0 , b)) is being seen for the first time

Quota 3
=========

dfs has been entered 9 times
memo map looks like:
{
  (IMode | quota 1 | ?? :: [b]) ==> [[] :: [b]]
  (IMode | quota 1 | ?? :: Maybe (b)) ==> [Data.Maybe.Nothing :: Maybe (b)]
  (EMode | quota 1 | ?? :: (beta0 -> b)) ==> [snd :: ((tau1 , b) -> b), fst :: ((b , tau0) -> b), GHC.List.last :: ([b] -> b), GHC.List.head :: ([b] -> b), Data.Tuple.snd :: ((tau1 , b) -> b), Data.Tuple.fst :: ((b , tau0) -> b), Data.Maybe.fromJust :: (Maybe (b) -> b)]
}

GOAL: (EMode | quota 3 | ?? :: b ~ b) is being seen for the first time
  done with inEnv, split since quota (3) > 1
    GOAL: (EMode | quota 1 | ?? :: (alpha0 -> b) ~ (beta0 -> b)) has 7 solutions in memo map
      retrieved (size 1): snd :: ((tau1 , tau0) -> tau0)
        GOAL: (IMode | quota 2 | ?? :: (tau1 , b) ~ (beta0 , b)) is being seen for the first time
          done with inEnv, split since quota (2) > 1
            GOAL: (EMode | quota 1 | ?? :: (alpha1 -> (tau1 , b)) ~ (beta0 -> (beta1 , b))) is being seen for the first time
              unified (size 1): Data.Maybe.fromJust :: <a> . (Maybe (a) -> a) ~ (alpha1 -> (tau1 , b)) via {"alpha0" ==> (tau1 , b), "alpha1" ==> Maybe ((tau1 , b)), "tau0" ==> b, "tau2" ==> (tau1 , b)}
                GOAL: (IMode | quota 1 | ?? :: Maybe (((tau1 , b))) ~ Maybe (((beta0 , b)))) is being seen for the first time
                  unified (size 1): Data.Maybe.Nothing :: <a> . Maybe (a) ~ Maybe (((tau1 , b))) via {"alpha0" ==> (tau1 , b), "alpha1" ==> Maybe ((tau1 , b)), "tau0" ==> b, "tau2" ==> (tau1 , b), "tau3" ==> (tau1 , b)}
              unified (size 1): Data.Tuple.fst :: <b> . <a> . ((a , b) -> a) ~ (alpha1 -> (tau1 , b)) via {"alpha0" ==> (tau1 , b), "alpha1" ==> ((tau1 , b) , tau2), "tau0" ==> b, "tau3" ==> (tau1 , b)}
                GOAL: (IMode | quota 1 | ?? :: ((tau1 , b) , tau2) ~ ((beta0 , b) , beta1)) is being seen for the first time
              unified (size 1): Data.Tuple.snd :: <b> . <a> . ((a , b) -> b) ~ (alpha1 -> (tau1 , b)) via {"alpha0" ==> (tau1 , b), "alpha1" ==> (tau3 , (tau1 , b)), "tau0" ==> b, "tau2" ==> (tau1 , b)}
                GOAL: (IMode | quota 1 | ?? :: (tau3 , (tau1 , b)) ~ (beta1 , (beta0 , b))) is being seen for the first time
              unified (size 1): Data.Tuple.swap :: <b> . <a> . ((a , b) -> (b , a)) ~ (alpha1 -> (tau1 , b)) via {"alpha0" ==> (tau1 , b), "alpha1" ==> (b , tau1), "tau0" ==> b, "tau2" ==> tau1, "tau3" ==> b}
                GOAL: (IMode | quota 1 | ?? :: (b , tau1) ~ (b , beta0)) is being seen for the first time
              unified (size 1): GHC.List.head :: <a> . ([a] -> a) ~ (alpha1 -> (tau1 , b)) via {"alpha0" ==> (tau1 , b), "alpha1" ==> [(tau1 , b)], "tau0" ==> b, "tau2" ==> (tau1 , b)}
                GOAL: (IMode | quota 1 | ?? :: [(tau1 , b)] ~ [(beta0 , b)]) is being seen for the first time
                  unified (size 1): Nil :: <a> . {[a]|_v == (Nil)} ~ [(tau1 , b)] via {"alpha0" ==> (tau1 , b), "alpha1" ==> [(tau1 , b)], "tau0" ==> b, "tau2" ==> (tau1 , b), "tau3" ==> (tau1 , b)}
              unified (size 1): GHC.List.last :: <a> . ([a] -> a) ~ (alpha1 -> (tau1 , b)) via {"alpha0" ==> (tau1 , b), "alpha1" ==> [(tau1 , b)], "tau0" ==> b, "tau2" ==> (tau1 , b)}
                GOAL: (IMode | quota 1 | ?? :: [(tau1 , b)] ~ [(beta0 , b)]) has 1 solution in memo map
                  retrieved (size 1): [] :: {[tau3]|_v == (Nil)}
              unified (size 1): fst :: <b> . <a> . ((a , b) -> a) ~ (alpha1 -> (tau1 , b)) via {"alpha0" ==> (tau1 , b), "alpha1" ==> ((tau1 , b) , tau2), "tau0" ==> b, "tau3" ==> (tau1 , b)}
                GOAL: (IMode | quota 1 | ?? :: ((tau1 , b) , tau2) ~ ((beta0 , b) , beta1)) is being seen for the first time
              unified (size 1): snd :: <b> . <a> . ((a , b) -> b) ~ (alpha1 -> (tau1 , b)) via {"alpha0" ==> (tau1 , b), "alpha1" ==> (tau3 , (tau1 , b)), "tau0" ==> b, "tau2" ==> (tau1 , b)}
                GOAL: (IMode | quota 1 | ?? :: (tau3 , (tau1 , b)) ~ (beta1 , (beta0 , b))) is being seen for the first time
      retrieved (size 1): fst :: ((tau1 , tau0) -> tau1)
        GOAL: (IMode | quota 2 | ?? :: (b , tau0) ~ (b , beta0)) is being seen for the first time
          done with inEnv, split since quota (2) > 1
            GOAL: (EMode | quota 1 | ?? :: (alpha1 -> (b , tau0)) ~ (beta0 -> (b , beta1))) is being seen for the first time
              unified (size 1): Data.Maybe.fromJust :: <a> . (Maybe (a) -> a) ~ (alpha1 -> (b , tau0)) via {"alpha0" ==> (b , tau0), "alpha1" ==> Maybe ((b , tau0)), "tau1" ==> b, "tau2" ==> (b , tau0)}
                GOAL: (IMode | quota 1 | ?? :: Maybe (((b , tau0))) ~ Maybe (((b , beta0)))) is being seen for the first time
                  unified (size 1): Data.Maybe.Nothing :: <a> . Maybe (a) ~ Maybe (((b , tau0))) via {"alpha0" ==> (b , tau0), "alpha1" ==> Maybe ((b , tau0)), "tau1" ==> b, "tau2" ==> (b , tau0), "tau3" ==> (b , tau0)}
              unified (size 1): Data.Tuple.fst :: <b> . <a> . ((a , b) -> a) ~ (alpha1 -> (b , tau0)) via {"alpha0" ==> (b , tau0), "alpha1" ==> ((b , tau0) , tau2), "tau1" ==> b, "tau3" ==> (b , tau0)}
                GOAL: (IMode | quota 1 | ?? :: ((b , tau0) , tau2) ~ ((b , beta0) , beta1)) is being seen for the first time
              unified (size 1): Data.Tuple.snd :: <b> . <a> . ((a , b) -> b) ~ (alpha1 -> (b , tau0)) via {"alpha0" ==> (b , tau0), "alpha1" ==> (tau3 , (b , tau0)), "tau1" ==> b, "tau2" ==> (b , tau0)}
                GOAL: (IMode | quota 1 | ?? :: (tau3 , (b , tau0)) ~ (beta1 , (b , beta0))) is being seen for the first time
              unified (size 1): Data.Tuple.swap :: <b> . <a> . ((a , b) -> (b , a)) ~ (alpha1 -> (b , tau0)) via {"alpha0" ==> (b , tau0), "alpha1" ==> (tau0 , b), "tau1" ==> b, "tau2" ==> b, "tau3" ==> tau0}
                GOAL: (IMode | quota 1 | ?? :: (tau0 , b) ~ (beta0 , b)) is being seen for the first time
              unified (size 1): GHC.List.head :: <a> . ([a] -> a) ~ (alpha1 -> (b , tau0)) via {"alpha0" ==> (b , tau0), "alpha1" ==> [(b , tau0)], "tau1" ==> b, "tau2" ==> (b , tau0)}
                GOAL: (IMode | quota 1 | ?? :: [(b , tau0)] ~ [(b , beta0)]) is being seen for the first time
                  unified (size 1): Nil :: <a> . {[a]|_v == (Nil)} ~ [(b , tau0)] via {"alpha0" ==> (b , tau0), "alpha1" ==> [(b , tau0)], "tau1" ==> b, "tau2" ==> (b , tau0), "tau3" ==> (b , tau0)}
              unified (size 1): GHC.List.last :: <a> . ([a] -> a) ~ (alpha1 -> (b , tau0)) via {"alpha0" ==> (b , tau0), "alpha1" ==> [(b , tau0)], "tau1" ==> b, "tau2" ==> (b , tau0)}
                GOAL: (IMode | quota 1 | ?? :: [(b , tau0)] ~ [(b , beta0)]) has 1 solution in memo map
                  retrieved (size 1): [] :: {[tau3]|_v == (Nil)}
              unified (size 1): fst :: <b> . <a> . ((a , b) -> a) ~ (alpha1 -> (b , tau0)) via {"alpha0" ==> (b , tau0), "alpha1" ==> ((b , tau0) , tau2), "tau1" ==> b, "tau3" ==> (b , tau0)}
                GOAL: (IMode | quota 1 | ?? :: ((b , tau0) , tau2) ~ ((b , beta0) , beta1)) is being seen for the first time
              unified (size 1): snd :: <b> . <a> . ((a , b) -> b) ~ (alpha1 -> (b , tau0)) via {"alpha0" ==> (b , tau0), "alpha1" ==> (tau3 , (b , tau0)), "tau1" ==> b, "tau2" ==> (b , tau0)}
                GOAL: (IMode | quota 1 | ?? :: (tau3 , (b , tau0)) ~ (beta1 , (b , beta0))) is being seen for the first time
      retrieved (size 1): GHC.List.last :: ([tau0] -> tau0)
        GOAL: (IMode | quota 2 | ?? :: [b] ~ [b]) is being seen for the first time
          done with inEnv, split since quota (2) > 1
            GOAL: (EMode | quota 1 | ?? :: (alpha1 -> [b]) ~ (beta0 -> [b])) is being seen for the first time
              unified (size 1): Data.Either.lefts :: <b> . <a> . ([Either (a) (b)] -> [a]) ~ (alpha1 -> [b]) via {"alpha0" ==> [b], "alpha1" ==> [Either (b) (tau1)], "tau0" ==> b, "tau2" ==> b}
                GOAL: (IMode | quota 1 | ?? :: [Either (b) (tau1)] ~ [Either (b) (beta0)]) is being seen for the first time
                  unified (size 1): Nil :: <a> . {[a]|_v == (Nil)} ~ [Either (b) (tau1)] via {"alpha0" ==> [b], "alpha1" ==> [Either (b) (tau1)], "tau0" ==> b, "tau2" ==> b, "tau3" ==> Either (b) (tau1)}
              unified (size 1): Data.Either.rights :: <b> . <a> . ([Either (a) (b)] -> [b]) ~ (alpha1 -> [b]) via {"alpha0" ==> [b], "alpha1" ==> [Either (tau2) (b)], "tau0" ==> b, "tau1" ==> b}
                GOAL: (IMode | quota 1 | ?? :: [Either (tau2) (b)] ~ [Either (beta0) (b)]) is being seen for the first time
                  unified (size 1): Nil :: <a> . {[a]|_v == (Nil)} ~ [Either (tau2) (b)] via {"alpha0" ==> [b], "alpha1" ==> [Either (tau2) (b)], "tau0" ==> b, "tau1" ==> b, "tau3" ==> Either (tau2) (b)}
              unified (size 1): Data.Maybe.catMaybes :: <a> . ([Maybe (a)] -> [a]) ~ (alpha1 -> [b]) via {"alpha0" ==> [b], "alpha1" ==> [Maybe (b)], "tau0" ==> b, "tau1" ==> b}
                GOAL: (IMode | quota 1 | ?? :: [Maybe (b)] ~ [Maybe (b)]) is being seen for the first time
                  unified (size 1): Nil :: <a> . {[a]|_v == (Nil)} ~ [Maybe (b)] via {"alpha0" ==> [b], "alpha1" ==> [Maybe (b)], "tau0" ==> b, "tau1" ==> b, "tau2" ==> Maybe (b)}
              unified (size 1): Data.Maybe.fromJust :: <a> . (Maybe (a) -> a) ~ (alpha1 -> [b]) via {"alpha0" ==> [b], "alpha1" ==> Maybe ([b]), "tau0" ==> b, "tau1" ==> [b]}
                GOAL: (IMode | quota 1 | ?? :: Maybe (([b])) ~ Maybe (([b]))) is being seen for the first time
                  unified (size 1): Data.Maybe.Nothing :: <a> . Maybe (a) ~ Maybe (([b])) via {"alpha0" ==> [b], "alpha1" ==> Maybe ([b]), "tau0" ==> b, "tau1" ==> [b], "tau2" ==> [b]}
              unified (size 1): Data.Maybe.maybeToList :: <a> . (Maybe (a) -> [a]) ~ (alpha1 -> [b]) via {"alpha0" ==> [b], "alpha1" ==> Maybe (b), "tau0" ==> b, "tau1" ==> b}
                GOAL: (IMode | quota 1 | ?? :: Maybe (b) ~ Maybe (b)) has 1 solution in memo map
                  retrieved (size 1): Data.Maybe.Nothing :: Maybe (tau2)
              unified (size 1): Data.Tuple.fst :: <b> . <a> . ((a , b) -> a) ~ (alpha1 -> [b]) via {"alpha0" ==> [b], "alpha1" ==> ([b] , tau1), "tau0" ==> b, "tau2" ==> [b]}
                GOAL: (IMode | quota 1 | ?? :: ([b] , tau1) ~ ([b] , beta0)) is being seen for the first time
              unified (size 1): Data.Tuple.snd :: <b> . <a> . ((a , b) -> b) ~ (alpha1 -> [b]) via {"alpha0" ==> [b], "alpha1" ==> (tau2 , [b]), "tau0" ==> b, "tau1" ==> [b]}
                GOAL: (IMode | quota 1 | ?? :: (tau2 , [b]) ~ (beta0 , [b])) is being seen for the first time
              unified (size 1): GHC.List.concat :: <a> . ([[a]] -> [a]) ~ (alpha1 -> [b]) via {"alpha0" ==> [b], "alpha1" ==> [[b]], "tau0" ==> b, "tau1" ==> b}
                GOAL: (IMode | quota 1 | ?? :: [[b]] ~ [[b]]) is being seen for the first time
                  unified (size 1): Nil :: <a> . {[a]|_v == (Nil)} ~ [[b]] via {"alpha0" ==> [b], "alpha1" ==> [[b]], "tau0" ==> b, "tau1" ==> b, "tau2" ==> [b]}
              unified (size 1): GHC.List.cycle :: <a> . ([a] -> [a]) ~ (alpha1 -> [b]) via {"alpha0" ==> [b], "alpha1" ==> [b], "tau0" ==> b, "tau1" ==> b}
                GOAL: (IMode | quota 1 | ?? :: [b] ~ [b]) has 1 solution in memo map
                  retrieved (size 1): [] :: {[tau2]|_v == (Nil)}
              unified (size 1): GHC.List.head :: <a> . ([a] -> a) ~ (alpha1 -> [b]) via {"alpha0" ==> [b], "alpha1" ==> [[b]], "tau0" ==> b, "tau1" ==> [b]}
                GOAL: (IMode | quota 1 | ?? :: [[b]] ~ [[b]]) has 1 solution in memo map
                  retrieved (size 1): [] :: {[tau2]|_v == (Nil)}
              unified (size 1): GHC.List.init :: <a> . ([a] -> [a]) ~ (alpha1 -> [b]) via {"alpha0" ==> [b], "alpha1" ==> [b], "tau0" ==> b, "tau1" ==> b}
                GOAL: (IMode | quota 1 | ?? :: [b] ~ [b]) has 1 solution in memo map
                  retrieved (size 1): [] :: {[tau2]|_v == (Nil)}
              unified (size 1): GHC.List.last :: <a> . ([a] -> a) ~ (alpha1 -> [b]) via {"alpha0" ==> [b], "alpha1" ==> [[b]], "tau0" ==> b, "tau1" ==> [b]}
                GOAL: (IMode | quota 1 | ?? :: [[b]] ~ [[b]]) has 1 solution in memo map
                  retrieved (size 1): [] :: {[tau2]|_v == (Nil)}
              unified (size 1): GHC.List.repeat :: <a> . (a -> [a]) ~ (alpha1 -> [b]) via {"alpha0" ==> [b], "alpha1" ==> b, "tau0" ==> b, "tau1" ==> b}
                GOAL: (IMode | quota 1 | ?? :: b ~ b) is being seen for the first time
              unified (size 1): GHC.List.reverse :: <a> . ([a] -> [a]) ~ (alpha1 -> [b]) via {"alpha0" ==> [b], "alpha1" ==> [b], "tau0" ==> b, "tau1" ==> b}
                GOAL: (IMode | quota 1 | ?? :: [b] ~ [b]) has 1 solution in memo map
                  retrieved (size 1): [] :: {[tau2]|_v == (Nil)}
              unified (size 1): GHC.List.tail :: <a> . ([a] -> [a]) ~ (alpha1 -> [b]) via {"alpha0" ==> [b], "alpha1" ==> [b], "tau0" ==> b, "tau1" ==> b}
                GOAL: (IMode | quota 1 | ?? :: [b] ~ [b]) has 1 solution in memo map
                  retrieved (size 1): [] :: {[tau2]|_v == (Nil)}
              unified (size 1): fst :: <b> . <a> . ((a , b) -> a) ~ (alpha1 -> [b]) via {"alpha0" ==> [b], "alpha1" ==> ([b] , tau1), "tau0" ==> b, "tau2" ==> [b]}
                GOAL: (IMode | quota 1 | ?? :: ([b] , tau1) ~ ([b] , beta0)) is being seen for the first time
              unified (size 1): snd :: <b> . <a> . ((a , b) -> b) ~ (alpha1 -> [b]) via {"alpha0" ==> [b], "alpha1" ==> (tau2 , [b]), "tau0" ==> b, "tau1" ==> [b]}
                GOAL: (IMode | quota 1 | ?? :: (tau2 , [b]) ~ (beta0 , [b])) is being seen for the first time
      retrieved (size 1): GHC.List.head :: ([tau0] -> tau0)
        GOAL: (IMode | quota 2 | ?? :: [b] ~ [b]) is being seen for the first time
          done with inEnv, split since quota (2) > 1
            GOAL: (EMode | quota 1 | ?? :: (alpha1 -> [b]) ~ (beta0 -> [b])) has 17 solutions in memo map
              retrieved (size 1): snd :: ((tau2 , tau1) -> tau1)
                GOAL: (IMode | quota 1 | ?? :: (tau2 , [b]) ~ (beta0 , [b])) is being seen for the first time
              retrieved (size 1): fst :: ((tau2 , tau1) -> tau2)
                GOAL: (IMode | quota 1 | ?? :: ([b] , tau1) ~ ([b] , beta0)) is being seen for the first time
              retrieved (size 1): GHC.List.tail :: ([tau1] -> [tau1])
                GOAL: (IMode | quota 1 | ?? :: [b] ~ [b]) has 1 solution in memo map
                  retrieved (size 1): [] :: {[tau2]|_v == (Nil)}
              retrieved (size 1): GHC.List.reverse :: ([tau1] -> [tau1])
                GOAL: (IMode | quota 1 | ?? :: [b] ~ [b]) has 1 solution in memo map
                  retrieved (size 1): [] :: {[tau2]|_v == (Nil)}
              retrieved (size 1): GHC.List.repeat :: (tau1 -> [tau1])
                GOAL: (IMode | quota 1 | ?? :: b ~ b) is being seen for the first time
              retrieved (size 1): GHC.List.last :: ([tau1] -> tau1)
                GOAL: (IMode | quota 1 | ?? :: [[b]] ~ [[b]]) has 1 solution in memo map
                  retrieved (size 1): [] :: {[tau2]|_v == (Nil)}
              retrieved (size 1): GHC.List.init :: ([tau1] -> [tau1])
                GOAL: (IMode | quota 1 | ?? :: [b] ~ [b]) has 1 solution in memo map
                  retrieved (size 1): [] :: {[tau2]|_v == (Nil)}
              retrieved (size 1): GHC.List.head :: ([tau1] -> tau1)
                GOAL: (IMode | quota 1 | ?? :: [[b]] ~ [[b]]) has 1 solution in memo map
                  retrieved (size 1): [] :: {[tau2]|_v == (Nil)}
              retrieved (size 1): GHC.List.cycle :: ([tau1] -> [tau1])
                GOAL: (IMode | quota 1 | ?? :: [b] ~ [b]) has 1 solution in memo map
                  retrieved (size 1): [] :: {[tau2]|_v == (Nil)}
              retrieved (size 1): GHC.List.concat :: ([[tau1]] -> [tau1])
                GOAL: (IMode | quota 1 | ?? :: [[b]] ~ [[b]]) has 1 solution in memo map
                  retrieved (size 1): [] :: {[tau2]|_v == (Nil)}
              retrieved (size 1): Data.Tuple.snd :: ((tau2 , tau1) -> tau1)
                GOAL: (IMode | quota 1 | ?? :: (tau2 , [b]) ~ (beta0 , [b])) is being seen for the first time
              retrieved (size 1): Data.Tuple.fst :: ((tau2 , tau1) -> tau2)
                GOAL: (IMode | quota 1 | ?? :: ([b] , tau1) ~ ([b] , beta0)) is being seen for the first time
              retrieved (size 1): Data.Maybe.maybeToList :: (Maybe (tau1) -> [tau1])
                GOAL: (IMode | quota 1 | ?? :: Maybe (b) ~ Maybe (b)) has 1 solution in memo map
                  retrieved (size 1): Data.Maybe.Nothing :: Maybe (tau2)
              retrieved (size 1): Data.Maybe.fromJust :: (Maybe (tau1) -> tau1)
                GOAL: (IMode | quota 1 | ?? :: Maybe (([b])) ~ Maybe (([b]))) has 1 solution in memo map
                  retrieved (size 1): Data.Maybe.Nothing :: Maybe (tau2)
              retrieved (size 1): Data.Maybe.catMaybes :: ([Maybe (tau1)] -> [tau1])
                GOAL: (IMode | quota 1 | ?? :: [Maybe (b)] ~ [Maybe (b)]) has 1 solution in memo map
                  retrieved (size 1): [] :: {[tau2]|_v == (Nil)}
              retrieved (size 1): Data.Either.rights :: ([Either (tau2) (tau1)] -> [tau1])
                GOAL: (IMode | quota 1 | ?? :: [Either (tau2) (b)] ~ [Either (beta0) (b)]) has 1 solution in memo map
                  retrieved (size 1): [] :: {[tau3]|_v == (Nil)}
              retrieved (size 1): Data.Either.lefts :: ([Either (tau2) (tau1)] -> [tau2])
                GOAL: (IMode | quota 1 | ?? :: [Either (b) (tau1)] ~ [Either (b) (beta0)]) has 1 solution in memo map
                  retrieved (size 1): [] :: {[tau3]|_v == (Nil)}
      retrieved (size 1): Data.Tuple.snd :: ((tau1 , tau0) -> tau0)
        GOAL: (IMode | quota 2 | ?? :: (tau1 , b) ~ (beta0 , b)) is being seen for the first time
          done with inEnv, split since quota (2) > 1
            GOAL: (EMode | quota 1 | ?? :: (alpha1 -> (tau1 , b)) ~ (beta0 -> (beta1 , b))) has 8 solutions in memo map
              retrieved (size 1): snd :: ((tau3 , tau2) -> tau2)
                GOAL: (IMode | quota 1 | ?? :: (tau3 , (tau1 , b)) ~ (beta1 , (beta0 , b))) is being seen for the first time
              retrieved (size 1): fst :: ((tau3 , tau2) -> tau3)
                GOAL: (IMode | quota 1 | ?? :: ((tau1 , b) , tau2) ~ ((beta0 , b) , beta1)) is being seen for the first time
              retrieved (size 1): GHC.List.last :: ([tau2] -> tau2)
                GOAL: (IMode | quota 1 | ?? :: [(tau1 , b)] ~ [(beta0 , b)]) has 1 solution in memo map
                  retrieved (size 1): [] :: {[tau3]|_v == (Nil)}
              retrieved (size 1): GHC.List.head :: ([tau2] -> tau2)
                GOAL: (IMode | quota 1 | ?? :: [(tau1 , b)] ~ [(beta0 , b)]) has 1 solution in memo map
                  retrieved (size 1): [] :: {[tau3]|_v == (Nil)}
              retrieved (size 1): Data.Tuple.swap :: ((tau3 , tau2) -> (tau2 , tau3))
                GOAL: (IMode | quota 1 | ?? :: (b , tau2) ~ (b , beta0)) is being seen for the first time
              retrieved (size 1): Data.Tuple.snd :: ((tau3 , tau2) -> tau2)
                GOAL: (IMode | quota 1 | ?? :: (tau3 , (tau1 , b)) ~ (beta1 , (beta0 , b))) is being seen for the first time
              retrieved (size 1): Data.Tuple.fst :: ((tau3 , tau2) -> tau3)
                GOAL: (IMode | quota 1 | ?? :: ((tau1 , b) , tau2) ~ ((beta0 , b) , beta1)) is being seen for the first time
              retrieved (size 1): Data.Maybe.fromJust :: (Maybe (tau2) -> tau2)
                GOAL: (IMode | quota 1 | ?? :: Maybe (((tau1 , b))) ~ Maybe (((beta0 , b)))) has 1 solution in memo map
                  retrieved (size 1): Data.Maybe.Nothing :: Maybe (tau3)
      retrieved (size 1): Data.Tuple.fst :: ((tau1 , tau0) -> tau1)
        GOAL: (IMode | quota 2 | ?? :: (b , tau0) ~ (b , beta0)) is being seen for the first time
          done with inEnv, split since quota (2) > 1
            GOAL: (EMode | quota 1 | ?? :: (alpha1 -> (b , tau0)) ~ (beta0 -> (b , beta1))) has 8 solutions in memo map
              retrieved (size 1): snd :: ((tau3 , tau2) -> tau2)
                GOAL: (IMode | quota 1 | ?? :: (tau3 , (b , tau0)) ~ (beta1 , (b , beta0))) is being seen for the first time
              retrieved (size 1): fst :: ((tau3 , tau2) -> tau3)
                GOAL: (IMode | quota 1 | ?? :: ((b , tau0) , tau2) ~ ((b , beta0) , beta1)) is being seen for the first time
              retrieved (size 1): GHC.List.last :: ([tau2] -> tau2)
                GOAL: (IMode | quota 1 | ?? :: [(b , tau0)] ~ [(b , beta0)]) has 1 solution in memo map
                  retrieved (size 1): [] :: {[tau3]|_v == (Nil)}
              retrieved (size 1): GHC.List.head :: ([tau2] -> tau2)
                GOAL: (IMode | quota 1 | ?? :: [(b , tau0)] ~ [(b , beta0)]) has 1 solution in memo map
                  retrieved (size 1): [] :: {[tau3]|_v == (Nil)}
              retrieved (size 1): Data.Tuple.swap :: ((tau3 , tau2) -> (tau2 , tau3))
                GOAL: (IMode | quota 1 | ?? :: (tau3 , b) ~ (beta0 , b)) is being seen for the first time
              retrieved (size 1): Data.Tuple.snd :: ((tau3 , tau2) -> tau2)
                GOAL: (IMode | quota 1 | ?? :: (tau3 , (b , tau0)) ~ (beta1 , (b , beta0))) is being seen for the first time
              retrieved (size 1): Data.Tuple.fst :: ((tau3 , tau2) -> tau3)
                GOAL: (IMode | quota 1 | ?? :: ((b , tau0) , tau2) ~ ((b , beta0) , beta1)) is being seen for the first time
              retrieved (size 1): Data.Maybe.fromJust :: (Maybe (tau2) -> tau2)
                GOAL: (IMode | quota 1 | ?? :: Maybe (((b , tau0))) ~ Maybe (((b , beta0)))) has 1 solution in memo map
                  retrieved (size 1): Data.Maybe.Nothing :: Maybe (tau3)
      retrieved (size 1): Data.Maybe.fromJust :: (Maybe (tau0) -> tau0)
        GOAL: (IMode | quota 2 | ?? :: Maybe (b) ~ Maybe (b)) is being seen for the first time
          done with inEnv, split since quota (2) > 1
            GOAL: (EMode | quota 1 | ?? :: (alpha1 -> Maybe (b)) ~ (beta0 -> Maybe (b))) is being seen for the first time
              unified (size 1): Data.Maybe.Just :: <a> . (a -> Maybe (a)) ~ (alpha1 -> Maybe (b)) via {"alpha0" ==> Maybe (b), "alpha1" ==> b, "tau0" ==> b, "tau1" ==> b}
                GOAL: (IMode | quota 1 | ?? :: b ~ b) is being seen for the first time
              unified (size 1): Data.Maybe.fromJust :: <a> . (Maybe (a) -> a) ~ (alpha1 -> Maybe (b)) via {"alpha0" ==> Maybe (b), "alpha1" ==> Maybe (Maybe (b)), "tau0" ==> b, "tau1" ==> Maybe (b)}
                GOAL: (IMode | quota 1 | ?? :: Maybe ((Maybe (b))) ~ Maybe ((Maybe (b)))) is being seen for the first time
                  unified (size 1): Data.Maybe.Nothing :: <a> . Maybe (a) ~ Maybe ((Maybe (b))) via {"alpha0" ==> Maybe (b), "alpha1" ==> Maybe (Maybe (b)), "tau0" ==> b, "tau1" ==> Maybe (b), "tau2" ==> Maybe (b)}
              unified (size 1): Data.Maybe.listToMaybe :: <a> . ([a] -> Maybe (a)) ~ (alpha1 -> Maybe (b)) via {"alpha0" ==> Maybe (b), "alpha1" ==> [b], "tau0" ==> b, "tau1" ==> b}
                GOAL: (IMode | quota 1 | ?? :: [b] ~ [b]) has 1 solution in memo map
                  retrieved (size 1): [] :: {[tau2]|_v == (Nil)}
              unified (size 1): Data.Tuple.fst :: <b> . <a> . ((a , b) -> a) ~ (alpha1 -> Maybe (b)) via {"alpha0" ==> Maybe (b), "alpha1" ==> (Maybe (b) , tau1), "tau0" ==> b, "tau2" ==> Maybe (b)}
                GOAL: (IMode | quota 1 | ?? :: (Maybe (b) , tau1) ~ (Maybe (b) , beta0)) is being seen for the first time
              unified (size 1): Data.Tuple.snd :: <b> . <a> . ((a , b) -> b) ~ (alpha1 -> Maybe (b)) via {"alpha0" ==> Maybe (b), "alpha1" ==> (tau2 , Maybe (b)), "tau0" ==> b, "tau1" ==> Maybe (b)}
                GOAL: (IMode | quota 1 | ?? :: (tau2 , Maybe (b)) ~ (beta0 , Maybe (b))) is being seen for the first time
              unified (size 1): GHC.List.head :: <a> . ([a] -> a) ~ (alpha1 -> Maybe (b)) via {"alpha0" ==> Maybe (b), "alpha1" ==> [Maybe (b)], "tau0" ==> b, "tau1" ==> Maybe (b)}
                GOAL: (IMode | quota 1 | ?? :: [Maybe (b)] ~ [Maybe (b)]) has 1 solution in memo map
                  retrieved (size 1): [] :: {[tau2]|_v == (Nil)}
              unified (size 1): GHC.List.last :: <a> . ([a] -> a) ~ (alpha1 -> Maybe (b)) via {"alpha0" ==> Maybe (b), "alpha1" ==> [Maybe (b)], "tau0" ==> b, "tau1" ==> Maybe (b)}
                GOAL: (IMode | quota 1 | ?? :: [Maybe (b)] ~ [Maybe (b)]) has 1 solution in memo map
                  retrieved (size 1): [] :: {[tau2]|_v == (Nil)}
              unified (size 1): fst :: <b> . <a> . ((a , b) -> a) ~ (alpha1 -> Maybe (b)) via {"alpha0" ==> Maybe (b), "alpha1" ==> (Maybe (b) , tau1), "tau0" ==> b, "tau2" ==> Maybe (b)}
                GOAL: (IMode | quota 1 | ?? :: (Maybe (b) , tau1) ~ (Maybe (b) , beta0)) is being seen for the first time
              unified (size 1): snd :: <b> . <a> . ((a , b) -> b) ~ (alpha1 -> Maybe (b)) via {"alpha0" ==> Maybe (b), "alpha1" ==> (tau2 , Maybe (b)), "tau0" ==> b, "tau1" ==> Maybe (b)}
                GOAL: (IMode | quota 1 | ?? :: (tau2 , Maybe (b)) ~ (beta0 , Maybe (b))) is being seen for the first time
    GOAL: (EMode | quota 2 | ?? :: (alpha0 -> b) ~ (beta0 -> b)) is being seen for the first time
      done with inEnv, split since quota (2) > 1
        GOAL: (EMode | quota 1 | ?? :: (alpha1 -> (alpha0 -> b)) ~ (beta1 -> (beta0 -> b))) is being seen for the first time
          unified (size 1): (GHC.List.!!) :: <a> . ([a] -> (Int -> a)) ~ (alpha1 -> (alpha0 -> b)) via {"alpha0" ==> Int, "alpha1" ==> [b], "tau0" ==> b}
            GOAL: (IMode | quota 1 | ?? :: [b] ~ [b]) has 1 solution in memo map
              retrieved (size 1): [] :: {[tau1]|_v == (Nil)}
          unified (size 1): Data.Either.fromLeft :: <b> . <a> . (a -> (Either (a) (b) -> a)) ~ (alpha1 -> (alpha0 -> b)) via {"alpha0" ==> Either (b) (tau0), "alpha1" ==> b, "tau1" ==> b}
            GOAL: (IMode | quota 1 | ?? :: b ~ b) is being seen for the first time
          unified (size 1): Data.Either.fromRight :: <b> . <a> . (b -> (Either (a) (b) -> b)) ~ (alpha1 -> (alpha0 -> b)) via {"alpha0" ==> Either (tau1) (b), "alpha1" ==> b, "tau0" ==> b}
            GOAL: (IMode | quota 1 | ?? :: b ~ b) is being seen for the first time
          unified (size 1): Data.Maybe.fromJust :: <a> . (Maybe (a) -> a) ~ (alpha1 -> (alpha0 -> b)) via {"alpha1" ==> Maybe (alpha0 -> b), "tau0" ==> alpha0 -> b}
            GOAL: (IMode | quota 1 | ?? :: Maybe (((alpha0 -> b))) ~ Maybe (((beta0 -> b)))) is being seen for the first time
              unified (size 1): arg0 :: Maybe (((a -> b))) ~ Maybe (((alpha0 -> b))) via {"alpha0" ==> a, "alpha1" ==> Maybe (a -> b), "tau0" ==> a -> b}
        GOAL: (IMode | quota 1 | ?? :: a ~ a) is being seen for the first time
          unified (size 1): arg1 :: a ~ a via {"alpha0" ==> a, "alpha1" ==> Maybe (a -> b), "tau0" ==> a -> b}

(Quota 3) Done with <b> . <a> . (Maybe (((a -> b))) -> (a -> b))!
size + subSize solution
   3 + 0       Data.Maybe.fromJust arg0 arg1

dfs has been entered 72 times



without: 2796 times
with:    1663 times