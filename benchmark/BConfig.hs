module BConfig where

import Types.Generate
import Types.Experiments
import Types.Environment
import BTypes


defaultTimeout = 60 :: Int
defaultQueryFile = "benchmark/suites/working.yml"
defaultExperiment = TrackTypesAndTransitions

expTyGarQ = "TYGAR-Q"
expTyGarQBNoDmd = expTyGarQB ++ "-no-demand"
expTyGarQBNoRel = expTyGarQB ++ "-no-relevancy"
expTyGarQNoCoalesce = expTyGarQ ++ "-no-coalescing"
expTyGarQCoalesceFirst = expTyGarQ ++ "-coalesce naive"
expTyGarQCoalesceLeast = expTyGarQ ++ "-coalesce least"
expTyGarQCoalesceMost = expTyGarQ ++ "-coalesce most"
expQueryRefinementHOF = "Query Refinement-HOF"
expSypetClone = "Sypet-Clone"
expTyGar0 = "TYGAR-0"
expTyGarQB = "TYGAR-QB"
expTyGar0B = "TYGAR-0B"
expNoGar = "NoGar"
expNoGar0 = "NoGar0"
expNoGarInc = "NoGar-Incremental"
expTyGar0BInc = "TyGar0B-Incremental"
expTyGarQInc = "TyGarQ-Incremental"
expTyGarQBInc = "TyGarQB-Incremental"
expILP = "Integer Linear Programming"

searchParamsTyGarQ = defaultSearchParams
searchParamsTyGarQBNoDmd = searchParamsTyGarQB{_disableDemand = True}
searchParamsTyGarQBNoRel = searchParamsTyGarQB{_disableDemand = True, _disableRelevancy = True}
searchParamsSypetClone = defaultSearchParams{_refineStrategy=SypetClone}
searchParamsTyGar0 = defaultSearchParams{_refineStrategy=TyGar0}
searchParamsTyGarQB = defaultSearchParams{_stopRefine=True, _threshold=5}
searchParamsTyGar0B = searchParamsTyGarQB{_refineStrategy=TyGar0, _threshold=5}
searchParamsNoGar = defaultSearchParams{_refineStrategy=NoGar}
searchParamsNoGar0 = defaultSearchParams{_refineStrategy=NoGar0}
searchParamsILP = defaultSearchParams
searchParamsTyGar0BInc = searchParamsTyGar0B{_incrementalSolving=True}
searchParamsNoGarInc = searchParamsNoGar{_incrementalSolving=True}
searchParamsTyGarQBInc = searchParamsTyGarQB{_incrementalSolving=True}
searchParamsTyGarQInc = searchParamsTyGarQ{_incrementalSolving=True}
searchParamsTyGarQNoCopy = searchParamsTyGarQ{_disableCopy=True}
searchParamsTyGarQNoFS = searchParamsTyGarQ{_disableBlack=True}
