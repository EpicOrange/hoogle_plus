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
synGuard' "(a -> Either b c) -> [a] -> ([b], [c])" ["Data.Either.partitionEithers", "GHC.List.map"] [(["\\x -> if x < 10 then Left x else Right x", "[0,10,20,30]"], "([0], [10, 20, 30])"), (["\\x -> if x < 10 then Left \"error\" else Right (x * 2)", "[1,3,11,20]"], "(["error", "error"], [22, 40])")]
solution: "\\f xs -> Data.Either.partitionEithers (Data.List.map f xs)"

mapMaybes
synGuard' "(a -> Maybe b) -> [a] -> Maybe b" ["Data.Maybe.listToMaybe", "Data.Maybe.mapMaybe"] [(["\\x -> if x < 3 then Nothing else Just (x * x)", "[2,4,6]"], "Just 16")]
solution: "\\f xs -> Data.Maybe.listToMaybe (Data.Maybe.mapMaybe f xs)"

mergeEither
synGuard' "Either a (Either a b) -> Either a b" ["Data.Either.either", ".Left", "Data.Either.either", ".Left", ".Right"] [(["Left 2"], "Left 2"), (["Right (Left 2)"], "Left 2"), (["Right (Right 2.2)"], "Right 2.2")]
solution: "\\arg0 -> Data.Either.either Left (Data.Either.either Left Right) arg0"

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
solution: "\\xs f -> Data.List.length (Data.List.filter f xs)"

splitStr
synGuard' "String -> Char -> [String]" ["GHC.List.words", "GHC.List.map", "Data.Bool.bool", "=="] []
solution: "\\xs x -> Data.List.words (Data.List.map (\\y -> Data.Bool.bool y ' ' (y == x)) xs)"

splitAtFirst
synGuard' "a -> [a] -> ([a], [a])" ["GHC.List.splitAt", "Data.Maybe.fromMaybe", "GHC.List.elemIndex"] [(["1", "[2,3,1,4,1,5]"], "([2,3],[4,1,5])")]
solution: "\\x xs -> Data.List.splitAt (Data.Maybe.fromMaybe 0 (Data.List.elemIndex x xs)) xs"

mbToEither
synGuard' "Maybe a -> b -> Either a b" ["Data.Bool.bool", ".Right", ".Left", "Data.Maybe.fromJust", "Data.Maybe.isJust"] [([Nothing, "1"], "Right 1"), ([Just 2, "3"], "Left 2")]
solution: "\\mb x -> Data.Bool.bool (Right x) (Left (Data.Maybe.fromJust mb)) (Data.Maybe.isJust mb)"

cartProduct
synGuard' "[a] -> [b] -> [[(a,b)]]" ["GHC.List.map", "GHC.List.map", "Pair"] [(["[1,2,3]","[2,3,4]"], "[[(1,2), (1,3), (1,4)], [(2,2), (2,3), (2,4)], [(3,2), (3,3), (3,4)]]")]
solution: "\\xs ys -> Data.List.map (\\x -> Data.List.map ((,) x) ys) xs"

multiAppPair
synGuard' "(a -> b, a -> c) -> a -> (b, c)" ["Pair", "Data.Tuple.fst", "Data.Tuple.snd"] [(["(\\x -> x * 3, \\x -> x * x)", "2"], "(6, 4)")]
solution: "\\arg0 arg1 -> (,) ((Data.Tuple.fst arg0) arg1) ((Data.Tuple.snd arg0) arg1)"

hoogle01
synGuard' "(a -> b) -> [a] -> b" ["GHC.List.head"] [(["\\xs -> GHC.List.length xs", "[[1,2,3], [1,2,3,4,5,6,7]]"], "3"), (["\\x -> [x, x]", "[6,5,4]"], "[6, 6]")]
solution: "\\f xs -> f (Data.List.head xs)"

firstMatch
synGuard' "[a] -> (a -> Bool) -> a" ["GHC.List.head", "GHC.List.filter"] [(["[1,2,3,4,5]", "\\x -> x `mod` 2 == 0"], "2"), (["[[1,2,3], [2,3,4,5], [3,4,5,6,7]]", "\\xs -> 2 `GHC.List.elem` xs"], "[1,2,3]")]
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

repl-funcs
synGuard' "(a->b)->Int->[a->b]" ["GHC.List.replicate"] []
solution: "\\f n -> Data.List.replicate n f"

mbAppFirst
synGuard' "b -> (a -> b) -> [a] -> b" ["Data.Maybe.maybe", "Data.Maybe.listToMaybe"] [(["2", "\\x -> x * x", "[3,4,5]"], "9"), (["2", "\\x -> x * x", "[]"], "2")]
solution: "\\x f xs -> Data.Maybe.maybe x f (Data.Maybe.listToMaybe xs)"

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

indexesOf
synGuard' "([(a,Int)] -> [(a,Int)]) -> [a] -> [Int] -> [Int]" ["GHC.List.map", "Data.Tuple.snd", "GHC.List.zip"] [(["map (\\(x, y) -> (x, y * y))", "[1,2,3]", "[9,8,7]"], "[81, 64, 49]")]
solution: "\\f xs ys -> Data.List.map Data.Tuple.snd (f (Data.List.zip xs ys))"

lookup
synGuard' "Eq a => [(a,b)] -> a -> b" ["Data.Maybe.fromJust", "GHC.List.lookup"] [(["[(1,2), (2,3), (4,6)]", "2"], "3")]
solution: "\\xs k -> Data.Maybe.fromJust (Data.List.lookup k xs)"

mbElem
synGuard' "Eq a => a -> [a] -> Maybe a" [".bool", ".Nothing", ".Just", "GHC.List.elem"] [(["2", "[1,3,5,7,9]"], "Nothing"), (["3", "[1,3,5,7,9]"], "Just 3")]
solution: "\\x xs -> bool Nothing (Just x) (GHC.List.elem x xs)"

areEq
synGuard' "Eq a => a -> a -> Maybe a" [".bool", ".Nothing", ".Just", "=="] [(["1", "2"], "Nothing"), (["1", "1"], "Just 1")]
solution: "\\x y -> bool Nothing (Just x) ((==) x y)"

containsEdge
synGuard' "[Int] -> (Int,Int) -> Bool" ["Pair", "GHC.List.elem", "&&", "GHC.List.elem"] [(["[1,2,3,4]", "(1,2)"], "True"), (["[1,2,3,4]", "(1,5)"], "False")]
solution: "\\xs (a,b) -> (a `Data.List.elem` xs) && (b `Data.List.elem` xs)"

dedupe
synGuard' "Eq a => [a] -> [a]" ["GHC.List.map", "GHC.List.head", "GHC.List.group"] [(["\"aaabbbccc\""], "\"abc\"")]
solution: "\\xs -> Data.List.map Data.List.head (Data.List.group xs)"

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