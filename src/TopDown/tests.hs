{-
appBoth
synGuard' "(a -> b) -> (a -> c) -> a -> (b, c)" ["Pair"] [(["\\x -> x + 1", "\\x -> x * 3", "3"], "(4, 9)"), (["\\x -> x ++ x", "Data.List.reverse", "[1,2,3]"], "([1,2,3,1,2,3], [3,2,1])")]
solution: "\\arg0 arg1 arg2 -> (arg0 arg2 , arg1 arg2)"

test
synGuard' "Bool -> a -> Maybe a" ["Data.Bool.bool", ".Nothing", ".Just"] [(["True", "1"], "Just 1"), (["False", "2"], "Nothing")]
solution: "\\arg0 arg1 -> Data.Bool.bool Nothing (Just arg1) arg0"

both
synGuard' "(a -> b) -> (a, a) -> (b, b)" ["Data.Tuple.fst", "Pair", "Data.Tuple.snd"] [(["\\x -> x + 1", "(43, 25)"], "(44, 26)"), (["\\x -> GHC.List.length x", "([1,2,3,4],[2,3,4])"], "(4, 3)")]
solution: "\\arg0 arg1 -> (arg0 (Data.Tuple.fst arg1), arg0 (Data.Tuple.snd arg1))"

firstJust
synGuard' "a -> [Maybe a] -> a" ["Data.Maybe.fromMaybe", "Data.Maybe.listToMaybe", "Data.Maybe.catMaybes"] [(["3", "[Nothing, Just 2, Nothing]"], "2"), (["3", "[]"], "3")]
solution: "\\x xs -> Data.Maybe.fromMaybe x (Data.Maybe.listToMaybe (Data.Maybe.catMaybes xs))"

mapEither
synGuard' "(a -> Either b c) -> [a] -> ([b], [c])" ["Data.Either.partitionEithers", "GHC.List.map"] [(["\\x -> if x < 10 then Left x else Right x", "[0,10,20,30]"], "([0], [10, 20, 30])"), (["\\x -> if x < 10 then Left \"error\" else Right (x * 2)", "[1,3,11,20]"], "([\"error\", \"error\"], [22, 40])")]
solution: "\\f xs -> Data.Either.partitionEithers (Data.List.map f xs)"

mapMaybes
synGuard' "(a -> Maybe b) -> [a] -> Maybe b" ["Data.Maybe.listToMaybe", "Data.Maybe.mapMaybe"] [(["\\x -> if x < 3 then Nothing else Just (x * x)", "[2,4,6]"], "Just 16")]
solution: "\\f xs -> Data.Maybe.listToMaybe (Data.Maybe.mapMaybe f xs)"

mergeEither
synGuard' "Either a (Either a b) -> Either a b" ["Data.Either.either", ".Left", "Data.Either.either", ".Left", ".Right"] [(["Left 2"], "Left 2"), (["Right (Left 2)"], "Left 2"), (["Right (Right 2.2)"], "Right 2.2")]
solution: "\\arg0 -> Data.Either.either Left (Data.Either.either Left Right) arg0"

(Quota 9) Done with <b> . <a> . (Either (a) ((Either (a) (b))) -> Either (a) (b))!
size    subSize solution
7       25      Data.Either.either (\arg1 ->
    Data.Either.Left arg1) (\arg2 ->
    arg2) arg0


multiApp
synGuard' "(a -> b -> c) -> (a -> b) -> a -> c" [] [(["\\x y -> x + y", "\\x -> x * x", "3"], "12"), (["\\x y -> GHC.List.length x * GHC.List.length y", "\\x -> x ++ x", "[1,2,3]"], "18")]
solution: "\\f g x -> f x (g x)"

singleList
synGuard' "a -> [a]" ["Cons", "Nil"] [(["2"], "[2]"), (["\"abc\""], "[\"abc\"]")]
syn' "a -> [a]" [(["2"], "[2]"), (["\"abc\""], "[\"abc\"]")]
solution: "\\arg0 -> (:) arg0 []"

head-last
synGuard' "[a] -> (a,a)" ["GHC.List.head", "Pair", "GHC.List.last"] [(["[1,2,3,4]"], "(1, 4)")]
syn' "[a] -> (a,a)" [(["[1,2,3,4]"], "(1, 4)")]
solution: "\\arg1 -> (Data.List.head arg1, Data.List.last arg1)"

head-rest
synGuard' "[a] -> (a, [a])" ["GHC.List.head", "Pair", "GHC.List.tail"] [(["[1,2,3,4]"], "(1, [2,3,4])")]
solution: "\\arg1 -> (Data.List.head arg1, Data.List.tail arg1)"

pred-match
synGuard' "[a] -> (a -> Bool) -> Int" ["GHC.List.length", "GHC.List.filter"] [(["[1,2,3,4,5]", "\\x -> x `mod` 2 == 0"], "2"), (["[[1,2,3], [2,3,4,5], [3,4,5,6,7]]", "\\xs -> 2 `GHC.List.elem` xs"], "2")]
syn' "[a] -> (a -> Bool) -> Int" [(["[1,2,3,4,5]", "\\x -> x `mod` 2 == 0"], "2"), (["[[1,2,3], [2,3,4,5], [3,4,5,6,7]]", "\\xs -> 2 `GHC.List.elem` xs"], "2")]
synO' "[a] -> (a -> Bool) -> Int" [(["[1,2,3,4,5]", "\\x -> x `mod` 2 == 0"], "2"), (["[[1,2,3], [2,3,4,5], [3,4,5,6,7]]", "\\xs -> 2 `GHC.List.elem` xs"], "2")]
solution: "\\xs f -> Data.List.length (Data.List.filter f xs)"

stack exec -- hplus --json='{
  "query": "a -> [Maybe a] -> a", 
  "inExamples": [
    {"inputs": ["1", "[Nothing ,Just 3, Nothing]"], "output":"3"}
  ]
}'

stack run -- hplus topdown --json='{
    "query": "[a] -> (a -> Bool) -> Int", 
    "inExamples": [
      {"inputs": ["[1,2,3,4,5]", "\\x -> x `mod` 2 == 0"], "output": "2"},
      {"inputs": ["[[1,2,3], [2,3,4,5], [3,4,5,6,7]]", "\\xs -> 2 `GHC.List.elem` xs"], "output": "2"}
    ] }' --disable-alt-imode
stack run -- hplus topdown --json='{ "query": "[a] -> (a -> Bool) -> Int", "inExamples": [ {"inputs": ["[1,2,3,4,5]", "\\x -> x `mod` 2 == 0"], "output": "2"}, {"inputs": ["[[1,2,3], [2,3,4,5], [3,4,5,6,7]]", "\\xs -> 2 `GHC.List.elem` xs"], "output": "2"} ] }' --disable-alt-imode

splitStr
synGuard' "String -> Char -> [String]" ["GHC.List.words", "GHC.List.map", "Data.Bool.bool", "=="] []
solution: "\\xs x -> Data.List.words (Data.List.map (\\y -> Data.Bool.bool y ' ' (y == x)) xs)"

splitAtFirst
synGuard' "a -> [a] -> ([a], [a])" ["GHC.List.splitAt", "Data.Maybe.fromMaybe", "GHC.List.elemIndex", "GHC.List.length"] [(["1", "[2,3,1,4,1,5]"], "([2,3],[4,1,5])")]
solution: "\\x xs -> Data.List.splitAt (Data.Maybe.fromMaybe 0 (Data.List.elemIndex x xs)) xs"
solution: "\\x xs -> GHC.List.splitAt (Data.Maybe.fromMaybe (GHC.List.length []) (GHC.List.elemIndex x xs)) xs"

-- this one errors a lot
mbToEither
synGuard' "Maybe a -> b -> Either a b" ["Data.Bool.bool", ".Right", ".Left", "Data.Maybe.fromJust", "Data.Maybe.isJust"] [([Nothing, "1"], "Right 1"), ([Just 2, "3"], "Left 2")]
solution: "\\mb x -> Data.Bool.bool (Right x) (Left (Data.Maybe.fromJust mb)) (Data.Maybe.isJust mb)"


cartProduct
synGuard' "[a] -> [b] -> [[(a,b)]]" ["GHC.List.map", "GHC.List.map", "Pair"] [(["[1,2,3]","[2,3,4]"], "[[(1,2), (1,3), (1,4)], [(2,2), (2,3), (2,4)], [(3,2), (3,3), (3,4)]]")]
synGuardO' "[a] -> [b] -> [[(a,b)]]" ["GHC.List.map", "repeat", "zip"] [(["[1,2,3]","[2,3,4]"], "[[(1,2), (1,3), (1,4)], [(2,2), (2,3), (2,4)], [(3,2), (3,3), (3,4)]]")]
synO' "[a] -> [b] -> [[(a,b)]]" [(["[1,2,3]","[2,3,4]"], "[[(1,2), (1,3), (1,4)], [(2,2), (2,3), (2,4)], [(3,2), (3,3), (3,4)]]")]
syn' "[a] -> [b] -> [[(a,b)]]" [(["[1,2,3]","[2,3,4]"], "[[(1,2), (1,3), (1,4)], [(2,2), (2,3), (2,4)], [(3,2), (3,3), (3,4)]]")]

solution: "\\xs ys -> Data.List.map (\\x -> Data.List.map ((,) x) ys) xs"
ours: \arg0 arg1 -> GHC.List.map (\arg2 -> GHC.List.zip (GHC.List.repeat arg2) arg1) arg0
solution: "\\xs ys -> Data.List.map (\\x -> Data.List.map (\y -> (,) x y) ys) xs"
                    GHC.List.map (\arg2 -> GHC.List.map (\arg3 -> (arg2 , arg3)) arg1) arg0

multiAppPair
synGuardO' "(a -> b, a -> c) -> a -> (b, c)" ["Pair", "Data.Tuple.fst", "Data.Tuple.snd"] [(["(\\x -> x * 3, \\x -> x * x)", "2"], "(6, 4)")]
synO' "(a -> b, a -> c) -> a -> (b, c)" [(["(\\x -> x * 3, \\x -> x * x)", "2"], "(6, 4)")]
solution: "\\arg0 arg1 -> (,) ((Data.Tuple.fst arg0) arg1) ((Data.Tuple.snd arg0) arg1)"

(Quota 8) Done with <c> . <b> . <a> . (((a -> b) , (a -> c)) -> (a -> (b , c)))!
size    subSize solution
7       24      ((Data.Tuple.fst arg0 arg1) , (Data.Tuple.snd arg0 arg1))

(GHC.List.concat [])

-----------------
--- BACKTRACE ---
-----------------
(?? :: (b , c))
((?? :: (tau0 -> (b , c))) (?? :: tau0))
(((?? :: (tau1 -> (tau0 -> (b , c)))) (?? :: tau1)) (?? :: tau0))
(((,) (?? :: tau1)) (?? :: tau0))
(((,) ((?? :: (tau4 -> b)) (?? :: tau4))) (?? :: tau0))
(((,) (((?? :: (tau5 -> (tau4 -> b))) (?? :: tau5)) (?? :: tau4))) (?? :: tau0))
(((,) ((Data.Tuple.fst (?? :: tau5)) (?? :: tau4))) (?? :: tau0))
(((,) (Data.Tuple.fst arg0 (?? :: tau4))) (?? :: tau0))
((,) (Data.Tuple.fst arg0 arg1) (?? :: tau0))
((,) (Data.Tuple.fst arg0 arg1) ((?? :: (tau8 -> c)) (?? :: tau8)))
((,) (Data.Tuple.fst arg0 arg1) (((?? :: (tau9 -> (tau8 -> c))) (?? :: tau9)) (?? :: tau8)))
((,) (Data.Tuple.fst arg0 arg1) ((Data.Tuple.snd (?? :: tau9)) (?? :: tau8)))
((,) (Data.Tuple.fst arg0 arg1) (Data.Tuple.snd arg0 (?? :: tau8)))
((Data.Tuple.fst arg0 arg1) , (Data.Tuple.snd arg0 arg1))
-----------------

hoogle01
synGuard' "(a -> b) -> [a] -> b" ["GHC.List.head"] [(["\\xs -> GHC.List.length xs", "[[1,2,3], [1,2,3,4,5,6,7]]"], "3"), (["\\x -> [x, x]", "[6,5,4]"], "[6, 6]")]
solution: "\\f xs -> f (Data.List.head xs)"

firstMatch
synGuard' "[a] -> (a -> Bool) -> a" ["GHC.List.head", "GHC.List.filter"] [(["[1,2,3,4,5]", "\\x -> x `mod` 2 == 0"], "2"), (["[[1,2,3], [2,3,4,5], [3,4,5,6,7]]", "\\xs -> 2 `GHC.List.elem` xs"], "[1,2,3]")]
synGuardO' "[a] -> (a -> Bool) -> a" ["GHC.List.head", "GHC.List.filter"] [(["[1,2,3,4,5]", "\\x -> x `mod` 2 == 0"], "2"), (["[[1,2,3], [2,3,4,5], [3,4,5,6,7]]", "\\xs -> 2 `GHC.List.elem` xs"], "[1,2,3]")]
synO' "[a] -> (a -> Bool) -> a" [(["[1,2,3,4,5]", "\\x -> x `mod` 2 == 0"], "2"), (["[[1,2,3], [2,3,4,5], [3,4,5,6,7]]", "\\xs -> 2 `GHC.List.elem` xs"], "[1,2,3]")]
syn' "[a] -> (a -> Bool) -> a" [(["[1,2,3,4,5]", "\\x -> x `mod` 2 == 0"], "2"), (["[[1,2,3], [2,3,4,5], [3,4,5,6,7]]", "\\xs -> 2 `GHC.List.elem` xs"], "[1,2,3]")]
solution: "\\xs f -> Data.List.head (Data.List.filter f xs)"

firstMaybe
synGuard' "[Maybe a] -> a" ["GHC.List.head", "Data.Maybe.catMaybes"] [(["[Nothing, Just 1, Just 2]"], "1")]
solution: "\\arg0 -> Data.List.head (Data.Maybe.catMaybes arg0)"

rights
synGuard' "[Either a b] -> Either a [b]" [".Right", "Data.Either.rights"] [(["[Left 1, Right 2, Right 3]"], "Right [2, 3]")]
solution: "\\arg0 -> Right (Data.Either.rights arg0)"

firstKey
synGuard' "[(a,b)] -> a" ["Data.Tuple.fst", "GHC.List.head"] [(["[(1, [1,2,3]), (2, [2,3,4]), (4, [4,5,6])]"], "1")]
solution: "\\arg0 -> Data.Tuple.fst (Data.List.head arg0)"

applyPair
synGuard' "(a -> b, a) -> b" ["Data.Tuple.fst", "Data.Tuple.snd"] [(["(\\x -> x * x, 10)"], "100")]
solution: "\\arg0 -> (Data.Tuple.fst arg0) (Data.Tuple.snd arg0)"

firstRight
synGuard' "[Either a b] -> Either a b" [".Right", "GHC.List.head", "Data.Either.rights"] [(["[Left 1, Left 2, Right 3, Right 4]"], "Right 3")maybe
synGuard' "Maybe a -> a -> Maybe a" [".Just", "Data.Maybe.fromMaybe"] [, (["Nothing", "2"], "Just 2"), (["Just 1", "2"], "Just 1")]
solution: "\\mb x -> Just (Data.Maybe.fromMaybe x mb)"

app3
synGuard' "(a -> b -> c -> d) -> a -> c -> b -> d" [] [(["\\x y z -> x + y - z", "2", "34", "12"], "-20")]
solution: "\\f x z y -> f x y z"

appendN
synGuard' "Int -> [a] -> [a]" ["GHC.List.concat", "GHC.List.replicate"] [(["2", "[1,2,3]"], "[1,2,3,1,2,3]")]
solution: "\\n xs -> Data.List.concat (Data.List.replicate n xs)"

flatten
synGuard' "[[[a]]] -> [a]" ["GHC.List.concat", "GHC.List.concat"] [(["[[[1,2,3], [2,3,4]],[[1,2]]]"], "[1,2,3,2,3,4,1,2]")]
solution: "\\xs -> Data.List.concat (Data.List.concat xs)"

takeNdropM
synGuard' "Int -> Int -> [a] -> ([a], [a])" ["GHC.List.take", "Pair", "GHC.List.drop"] [(["3", "5", "[1,2,3,4,5,6,7]"], "([1,2,3], [6,7])")]
solution: "\\n m l -> (Data.List.take n l, Data.List.drop m l)"

map
synGuard' "(a->b)->[a]->[b]" ["GHC.List.map"] [(["\\x -> x * x", "[1,2,3]"], "[1,4,9]")]
solution: "\\f xs -> Data.List.map f xs"

stack run -- hplus topdown --json='{"query": "(a->b)->[a]->[b]", "inExamples": [{"inputs": ["\\x -> x * x", "[1,2,3]"], "output":"[1,4,9]"}]}' --disable-memoize


repl-funcs
synGuard' "(a->b)->Int->[a->b]" ["GHC.List.replicate"] []
solution: "\\f n -> Data.List.replicate n f"

mbAppFirst
synGuard' "b -> (a -> b) -> [a] -> b" ["Data.Maybe.maybe", "Data.Maybe.listToMaybe"] [(["2", "\\x -> x * x", "[3,4,5]"], "9"), (["2", "\\x -> x * x", "[]"], "2")]
synGuardO' "b -> (a -> b) -> [a] -> b" ["Data.Maybe.maybe", "Data.Maybe.listToMaybe", "first", "sum"] [(["2", "\\x -> x * x", "[3,4,5]"], "9"), (["2", "\\x -> x * x", "[]"], "2")]
synGuardO' "b -> (a -> b) -> [a] -> b" ["fst", "sum"] [(["2", "\\x -> x * x", "[3,4,5]"], "9"), (["2", "\\x -> x * x", "[]"], "2")]
syn' "b -> (a -> b) -> [a] -> b" [(["2", "\\x -> x * x", "[3,4,5]"], "9"), (["2", "\\x -> x * x", "[]"], "2")]
synO' "b -> (a -> b) -> [a] -> b" [(["2", "\\x -> x * x", "[3,4,5]"], "9"), (["2", "\\x -> x * x", "[]"], "2")]
solution: "\\x f xs -> Data.Maybe.maybe x f (Data.Maybe.listToMaybe xs)"

stack run -- hplus topdown --json='{"query": "b -> (a -> b) -> [a] -> b", "inExamples": [ {"inputs": ["2", "\\x -> x * x", "[3,4,5]"], "output":"9"}, {"inputs": ["2", "\\x -> x * x", "[]"], "output":"2"} ]}' --disable-memoize



(Quota 5) Done with <b> . <a> . (b -> (((a -> b)) -> ([a] -> b)))!
size    subSize solution
5       10      Data.Maybe.maybe arg0 arg1 (Data.Maybe.listToMaybe arg2)

(46.89 secs, 25,324,355,536 bytes)

/tmp/4959440b-fb3d-4143-bf04-50d76fc30fb8.hs:16:47: error:
    • Occurs check: cannot construct the infinite type: a ~ [a]
    • In the first argument of ‘arg1’, namely ‘(snd ((arg0, arg2)))’
      In the expression: arg1 (snd ((arg0, arg2)))
      In the expression: \ arg0 arg1 arg2 -> arg1 (snd ((arg0, arg2)))
    • Relevant bindings include
        arg2 :: [a]
          (bound at /tmp/4959440b-fb3d-4143-bf04-50d76fc30fb8.hs:16:33)
        arg1 :: a -> b
          (bound at /tmp/4959440b-fb3d-4143-bf04-50d76fc30fb8.hs:16:28)
        ghcCheckedFunction :: b -> (a -> b) -> [a] -> b
          (bound at /tmp/4959440b-fb3d-4143-bf04-50d76fc30fb8.hs:16:1)
   |
16 | ghcCheckedFunction = \arg0 arg1 arg2 -> arg1 (snd ((arg0 , arg2)))
16 | ghcCheckedFunction = (\arg0 arg1 arg2 -> arg1 (?? :: a)) :: b
16 | ghcCheckedFunction = (\arg0 arg1 arg2 -> arg1 (
                                                    snd (?? :: (tau, a))
                                                    )) :: b
   |                                               ^^^^^^^^^^^^^^^^^^^

snd :: tau1 . tau2 . (tau1 , a) -> a
arg0 :: b 
arg1 :: (a -> b)
arg2 :: [a] 


2partApp
synGuard' "(a->b)->(b->c)->[a]->[c]" ["GHC.List.map", "GHC.List.map"] [(["GHC.List.length", "\\x -> x * x", "[[1,2,3], [1,2,3,4], [1,2,3,4,5]]"], "[9, 16, 25]")]
syn' "(a->b)->(b->c)->[a]->[c]" [(["GHC.List.length", "\\x -> x * x", "[[1,2,3], [1,2,3,4], [1,2,3,4,5]]"], "[9, 16, 25]")]
solution: "\\f g xs -> Data.List.map g (Data.List.map f xs)"
solution: "\\arg0 arg1 arg2 -> Data.List.map arg1 (Data.List.map arg0 arg2)"
\arg0 arg1 arg2 ->             GHC.List.map (\arg3 -> arg1 (arg0 arg3)) arg2

zipWithResult
synGuard' "(a->b)->[a]->[(a,b)]" ["GHC.List.zip", "GHC.List.map"] [(["\\x -> x * 3", "[1,2,3]"], "[(1,3), (2,6), (3,9)]")]
solution: "\\f xs -> Data.List.zip xs (Data.List.map f xs)"

resolveEither
synGuard' "Either a b -> (a->b) -> b" ["Data.Either.either"] [(["Left 3", "\\x -> x + 1"], "4"), (["Right 3", "\\x -> x * x"], "3")]
solution: "\\x f -> Data.Either.either f id x"

applyNtimes
synGuard' "(a->a) -> a -> Int -> a" ["GHC.List.foldr", "GHC.List.replicate"] [(["\\x -> x ++ x", "\"f-\"", "3"], "\"f-f-f-f-f-f-f-f-\"")]
solution: "\\f x n -> Data.List.foldr ($) x (Data.List.replicate n f)"

eitherTriple
synGuard' "Either a b -> Either a b -> Either a b" ["Data.Either.either", ".Left", "Data.Either.either", ".Left", ".Right"] [(["Left 1", "Left 2"], "Left 1"), (["Left 1", "Right 2"], "Left 1"), (["Right 2", "Right 3"], "Right 3")]
solution: "\\x y -> Data.Either.either Left (const (Data.Either.either Left Right y)) x"

pipe
synGuard' "[(a -> a)] -> (a -> a)" ["GHC.List.foldr"] [(["[\\x -> x + 1, \\x -> x * 2, \\x -> x * x]", "3"], "19")]
solution: "\\xs x -> Data.List.foldr ($) x xs"

lookup
synGuard' "Eq a => [(a,b)] -> a -> b" ["Data.Maybe.fromJust", "GHC.List.lookup"] [(["[(1,2), (2,3), (4,6)]", "2"], "3")]
solution: "\\xs k -> Data.Maybe.fromJust (Data.List.lookup k xs)"

mbElem
synGuard' "Eq a => a -> [a] -> Maybe a" [".bool", ".Nothing", ".Just", "GHC.List.elem"] [(["2", "[1,3,5,7,9]"], "Nothing"), (["3", "[1,3,5,7,9]"], "Just 3")]
synGuardO' "Eq a => a -> [a] -> Maybe a" [".bool", ".Nothing", ".Just", "GHC.List.elem"] [(["2", "[1,3,5,7,9]"], "Nothing"), (["3", "[1,3,5,7,9]"], "Just 3")]
synO' "Eq a => a -> [a] -> Maybe a" [(["2", "[1,3,5,7,9]"], "Nothing"), (["3", "[1,3,5,7,9]"], "Just 3")]
solution: "\\x xs -> bool Nothing (Just x) (GHC.List.elem x xs)"
tygar: \arg0 arg1 -> Data.Maybe.listToMaybe (GHC.List.dropWhile ((Data.Eq./=) arg0) arg1)
synGuardO' "Eq a => a -> [a] -> Maybe a" [".listToMaybe", ".dropWhile", "/="] [(["2", "[1,3,5,7,9]"], "Nothing"), (["3", "[1,3,5,7,9]"], "Just 3")]


areEq
synGuard' "Eq a => a -> a -> Maybe a" [".bool", ".Nothing", ".Just", "=="] [(["1", "2"], "Nothing"), (["1", "1"], "Just 1")]
synGuardO' "Eq a => a -> a -> Maybe a" [".bool", ".Nothing", ".Just", "=="] [(["1", "2"], "Nothing"), (["1", "1"], "Just 1")]
solution: "\\arg0 arg1 -> bool Nothing (Just arg0) ((==) arg0 arg1)"
ours:              Data.Bool.bool (\arg2 -> Data.Maybe.Nothing) (\arg3 -> Data.Maybe.Just arg0) (arg0 == arg1) (Data.Maybe.Just tcarg0)

containsEdge
synGuard' "[Int] -> (Int,Int) -> Bool" ["Pair", "GHC.List.elem", "&&", "GHC.List.elem", "Data.Tuple.fst", "Data.Tuple.snd"] [(["[1,2,3,4]", "(1,2)"], "True"), (["[1,2,3,4]", "(1,5)"], "False")]
solution: "\\xs p -> ((fst p) `Data.List.elem` xs) && ((snd p) `Data.List.elem` xs)"

dedupe
synGuard' "Eq a => [a] -> [a]" ["GHC.List.map", "GHC.List.head", "Data.List.group"] [(["\"aaabbbccc\""], "\"abc\"")]
synO' "Eq a => [a] -> [a]" [(["\"aaabbbccc\""], "\"abc\"")]
solution: "\\xs -> Data.List.map Data.List.head (Data.List.group xs)"

(Quota 7) Done with <a> . (@@hplusTC@@Eq (a) -> ([a] -> [a]))!
size    subSize solution
7       17      GHC.List.map (\arg1 -> GHC.List.head arg1) (Data.List.group tcarg0 arg0)

inverseMap
synGuard' "[a -> b] -> a -> [b]" ["GHC.List.map"] [(["[\\x -> x + 3, \\x -> x * x]", "4"], "[7, 16]")]
syn' "[a -> b] -> a -> [b]" [(["[\\x -> x + 3, \\x -> x * x]", "4"], "[7, 16]")]
solution: "\\fs x -> Data.List.map ($ x) fs"
solution: "\\arg0 arg1 -> GHC.List.map (\arg2 -> arg2 arg1) arg0"


running dfs on <b> . <a> . ([(a -> b)] -> (a -> [b])) at size 8

new program: GHC.List.map (\arg2 ->
    arg2) (GHC.List.map (\arg3 ->
               arg3 arg1) arg0)

new program: GHC.List.map (\arg2 ->
    arg2 arg1) arg0
"[7, 16]"
"[7, 16]"
RESULTS:{"outCandidates":[{"outExamples":[{"inputs":["[\\x -> x + 3, \\x -> x * x]","4"],"output":"[7, 16]"}],"solution":"\\arg0 arg1 -> GHC.List.map (\\arg2 -> arg2 arg1) arg0"}],"outDocs":[{"functionSig":"(a -> b) -> [a] -> [b]","functionName":"map","functionDesc":"<math>. map f xs is the list obtained by\napplying f to each element of xs, i.e.,\n\n\nmap f [x1, x2, ..., xn] == [f x1, f x2, ..., f xn]\nmap f [x1, x2, ...] == [f x1, f x2, ...]\n\n\n\n>>> map (+1) [1, 2, 3]\n\n"},{"functionSig":"(a -> b -> c) -> b","functionName":"arg2","functionDesc":""},{"functionSig":"[(a -> b)]","functionName":"arg0","functionDesc":""},{"functionSig":"a","functionName":"arg1","functionDesc":""}],"outError":""}


Done with <b> . <a> . ([(a -> b)] -> (a -> [b]))!
size    subSize solution
5       10      GHC.List.map (\arg2 ->
    arg2 arg1) arg0






Done with <b> . <a> . ([(a -> b)] -> (a -> [b]))!
size    subSize solution
5       10      GHC.List.map (\arg2 ->
    arg2 arg1) arg0

-}