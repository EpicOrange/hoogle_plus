#!/bin/bash

stack exec -- synquid generate \
-p base \
-m "Data.Bits" \
-m "Data.Bool" \
-m "Data.Eq" \
-m "Data.Either" \
-m "Data.Int" \
-m "Data.Function" \
-m "Data.List" \
-m "Data.Maybe" \
-m "Data.Ord" \
-m "Data.Type.Bool" \
-p bytestring \
-m "Data.ByteString" \
-m "Data.ByteString.Builder" \
-m "Data.ByteString.Builder.Prim" \
-m "Data.ByteString.Char8" \
-m "Data.ByteString.Short" \
-p text \
-m "Data.Text" \
-m "Data.Text.Array" \
-m "Data.Text.Encoding" \
-m "Data.Text.Encoding.Error" \
-m "Data.Text.Internal" \
-m "Data.Text.Internal.Builder" \
-m "Data.Text.Internal.Builder.Functions" \
-m "Data.Text.Internal.Builder.RealFloat.Functions" \
-m "Data.Text.Internal.Encoding.Utf16" \
-m "Data.Text.Internal.Encoding.Utf32" \
-m "Data.Text.Internal.Encoding.Utf8" \
-m "Data.Text.Internal.Functions" \
-m "Data.Text.Internal.Fusion" \
-m "Data.Text.Internal.Fusion.CaseMapping" \
-m "Data.Text.Internal.Fusion.Common" \
-m "Data.Text.Internal.Fusion.Size" \
-m "Data.Text.Internal.Fusion.Types" \
-m "Data.Text.Internal.Private" \
-m "Data.Text.Internal.Read" \
-m "Data.Text.Internal.Search" \
-m "Data.Text.Read"
# REMOVED FOR mj_speedtest
#-p containers \
#-m "Data.Containers.ListUtils" \
#-m "Data.IntMap" \
#-m "Data.IntMap.Lazy" \
#-m "Data.IntMap.Strict" \
#-m "Data.IntSet" \
#-m "Data.IntSet.Internal" \
#-m "Data.Map" \
#-m "Data.Map.Lazy" \
#-m "Data.Map.Strict" \
#-m "Data.Set" \
#-m "Data.Set.Internal" \
#-m "Data.Tree" \
#-m "Utils.Containers.Internal.BitQueue" \
#-m "Utils.Containers.Internal.BitUtil" \
#-m "Utils.Containers.Internal.StrictPair" \

# Not in the top packages
#-p network \
#-m "Network.Socket.Internal" \
#-m "Network.Socket" \
#-m "Network.Socket.ByteString" \
#-m "Network.Socket.ByteString.Lazy" \



# WILL NOT WORK
#-m "Data.Time.Format.Internal" \
#-m "Data.Map.Merge.Strict"
#-m "Data.Map.Strict.Internal" \
#-m "Data.IntMap.Internal.Debug"
#-m "Data.IntMap.Internal" \
#-m "Data.IntMap.Merge.Strict" \
#-m "Data.IntMap.Merge.Lazy" \
#-m "Data.Map.Internal"
#-m "Data.Map.Merge.Lazy"
#-m "Data.Map.Internal.Debug" \
#-m "Data.Sequence"
#-m "Data.Sequence.Internal" \
#-m "Data.Sequence.Internal.Sorting"
# -m "Data.Graph" \
#-m "Data.Text.IO" \
#-m "Data.Text.Lazy.IO" \
# -m "Data.Text.Internal.IO" \
# -m "Data.Text.Internal.Encoding.Fusion.Common" \
#-m "Data.Text.Internal.Lazy.Encoding.Fusion" \
#-m "Data.Text.Internal.Encoding.Fusion" \
# -m "Data.Text.Internal.Lazy.Fusion" \
# -m "Data.Text.Internal.Builder.Int.Digits" \
#-m "Data.ByteString.Builder.Extra"
#-m "Data.ByteString.Lazy.Builder.Extras" \
