{-# LANGUAGE LambdaCase #-}

-- | GHC details I wish I didn't have to explicitly code

module FrisbyGlobalTagsCabalGhcGhc (
  addOptP,
  cnvExtension,
  cnvLanguage,
  impliedXFlags,
  runGhc,
  ) where

import qualified DynFlags                                              as GHC
import qualified GHC                                                   as GHC
import qualified GHC.LanguageExtensions.Type                           as GHC
import qualified GHC.Paths                                       as GHC.Paths
import qualified Language.Haskell.Extension                          as Cabal
import qualified ToolSettings                                          as GHC

-----

cnvLanguage :: Maybe Cabal.Language -> GHC.DynFlags -> GHC.DynFlags
cnvLanguage = \case
    Nothing   -> id
    Just lang -> case lang of
      Cabal.Haskell2010       -> lopt_set GHC.Haskell2010
      Cabal.Haskell98         -> lopt_set GHC.Haskell98
      Cabal.UnknownLanguage{} -> id
  where
    lopt_set lang dflags = GHC.lang_set dflags (Just lang)

cnvExtension :: Cabal.Extension -> GHC.DynFlags -> GHC.DynFlags
cnvExtension = \case
    Cabal.DisableExtension ext -> cabalLangToGhcLang xopt_unset (error "impossible!") ext
    Cabal.UnknownExtension{} -> error "impossible!"
    Cabal.EnableExtension ext -> cabalLangToGhcLang xopt_set sopt_set ext
  where
    sopt_set   mode dflags = dflags{GHC.safeHaskell = mode}
    xopt_set   ext  dflags = setExtensionFlag' ext dflags
    xopt_unset ext  dflags = GHC.xopt_unset dflags ext

cabalLangToGhcLang ::
    (GHC.Extension -> r)
 -> (GHC.SafeHaskellMode -> r)
 -> Cabal.KnownExtension
 -> r
cabalLangToGhcLang
  xopt
  sopt
  = \case
        Cabal.AllowAmbiguousTypes -> xopt GHC.AllowAmbiguousTypes
--        					   AlternativeLayoutRule
--        					   AlternativeLayoutRuleTransitional
        Cabal.ApplicativeDo -> xopt GHC.ApplicativeDo
        Cabal.Arrows -> xopt GHC.Arrows
        Cabal.AutoDeriveTypeable -> xopt GHC.AutoDeriveTypeable
        Cabal.BangPatterns -> xopt GHC.BangPatterns
        Cabal.BinaryLiterals -> xopt GHC.BinaryLiterals
        Cabal.BlockArguments -> xopt GHC.BlockArguments
        Cabal.CApiFFI -> xopt GHC.CApiFFI
        Cabal.CPP -> xopt GHC.Cpp
        Cabal.CUSKs -> xopt GHC.CUSKs
        Cabal.ConstrainedClassMethods -> xopt GHC.ConstrainedClassMethods
        Cabal.ConstraintKinds -> xopt GHC.ConstraintKinds
        Cabal.DataKinds -> xopt GHC.DataKinds
        Cabal.DatatypeContexts -> xopt GHC.DatatypeContexts
        Cabal.DefaultSignatures -> xopt GHC.DefaultSignatures
        Cabal.DeriveAnyClass -> xopt GHC.DeriveAnyClass
        Cabal.DeriveDataTypeable -> xopt GHC.DeriveDataTypeable
        Cabal.DeriveFoldable -> xopt GHC.DeriveFoldable
        Cabal.DeriveFunctor -> xopt GHC.DeriveFunctor
        Cabal.DeriveGeneric -> xopt GHC.DeriveGeneric
        Cabal.DeriveLift -> xopt GHC.DeriveLift
        Cabal.DeriveTraversable -> xopt GHC.DeriveTraversable
        Cabal.DerivingStrategies -> xopt GHC.DerivingStrategies
        Cabal.DerivingVia -> xopt GHC.DerivingVia
        Cabal.DisambiguateRecordFields -> xopt GHC.DisambiguateRecordFields
        Cabal.DoAndIfThenElse -> xopt GHC.DoAndIfThenElse
        Cabal.DoRec -> xopt GHC.RecursiveDo
        Cabal.DuplicateRecordFields -> xopt GHC.DuplicateRecordFields
        Cabal.EmptyCase -> xopt GHC.EmptyCase
        Cabal.EmptyDataDecls -> xopt GHC.EmptyDataDecls
        Cabal.EmptyDataDeriving -> xopt GHC.EmptyDataDeriving
        Cabal.ExistentialQuantification -> xopt GHC.ExistentialQuantification
        Cabal.ExplicitForAll -> xopt GHC.ExplicitForAll
        Cabal.ExplicitNamespaces -> xopt GHC.ExplicitNamespaces
        Cabal.ExtendedDefaultRules -> xopt GHC.ExtendedDefaultRules
        Cabal.ExtensibleRecords -> error "GHC 8 does not support ExtensibleRecords language extension"
        Cabal.FlexibleContexts -> xopt GHC.FlexibleContexts
        Cabal.FlexibleInstances -> xopt GHC.FlexibleInstances
        Cabal.ForeignFunctionInterface -> xopt GHC.ForeignFunctionInterface
        Cabal.FunctionalDependencies -> xopt GHC.FunctionalDependencies
        Cabal.GADTSyntax -> xopt GHC.GADTSyntax
        Cabal.GADTs -> xopt GHC.GADTs
        Cabal.GHCForeignImportPrim -> xopt GHC.GHCForeignImportPrim
        Cabal.GeneralisedNewtypeDeriving -> xopt GHC.GeneralizedNewtypeDeriving
        Cabal.GeneralizedNewtypeDeriving -> xopt GHC.GeneralizedNewtypeDeriving
        Cabal.Generics -> error "Cabal deprecated the Generics language extension"
        Cabal.HereDocuments -> error "GHC 8 does not support the HereDocuments language extension"
        Cabal.HexFloatLiterals -> xopt GHC.HexFloatLiterals
        Cabal.ImplicitParams -> xopt GHC.ImplicitParams
        Cabal.ImplicitPrelude -> xopt GHC.ImplicitPrelude
        Cabal.ImportQualifiedPost -> xopt GHC.ImportQualifiedPost
        Cabal.ImpredicativeTypes -> xopt GHC.ImpredicativeTypes
        Cabal.IncoherentInstances -> xopt GHC.IncoherentInstances
        Cabal.InstanceSigs -> xopt GHC.InstanceSigs
        Cabal.InterruptibleFFI -> xopt GHC.InterruptibleFFI
        Cabal.JavaScriptFFI -> xopt GHC.JavaScriptFFI
        Cabal.KindSignatures -> xopt GHC.KindSignatures
        Cabal.LambdaCase -> xopt GHC.LambdaCase
--        Cabal.LexicalNegation -> error "GHC 8 does not support the LexicalNegation language extension"
        Cabal.LiberalTypeSynonyms -> xopt GHC.LiberalTypeSynonyms
--        Cabal.LinearType -> error "GHC 8 does not support the LinearType language extension"
        Cabal.MagicHash -> xopt GHC.MagicHash
        Cabal.MonadComprehensions -> xopt GHC.MonadComprehensions
        Cabal.MonadFailDesugaring -> xopt GHC.MonadFailDesugaring
        Cabal.MonoLocalBinds -> xopt GHC.MonoLocalBinds
        Cabal.MonoPatBinds -> xopt GHC.MonoPatBinds
        Cabal.MonomorphismRestriction -> xopt GHC.MonomorphismRestriction
        Cabal.MultiParamTypeClasses -> xopt GHC.MultiParamTypeClasses
        Cabal.MultiWayIf -> xopt GHC.MultiWayIf
        Cabal.NPlusKPatterns -> xopt GHC.NPlusKPatterns
        Cabal.NamedFieldPuns -> xopt GHC.RecordPuns
        Cabal.NamedWildCards -> xopt GHC.NamedWildCards
        Cabal.NegativeLiterals -> xopt GHC.NegativeLiterals
        Cabal.NewQualifiedOperators -> error "GHC 8 does not support the NewQualifiedOperators language extension"
        Cabal.NondecreasingIndentation -> xopt GHC.NondecreasingIndentation
        Cabal.NullaryTypeClasses -> xopt GHC.NullaryTypeClasses
        Cabal.NumDecimals -> xopt GHC.NumDecimals
        Cabal.NumericUnderscores -> xopt GHC.NumericUnderscores
        Cabal.OverlappingInstances -> xopt GHC.OverlappingInstances
        Cabal.OverloadedLabels -> xopt GHC.OverloadedLabels
        Cabal.OverloadedLists -> xopt GHC.OverloadedLists
        Cabal.OverloadedStrings -> xopt GHC.OverloadedStrings
        Cabal.PackageImports -> xopt GHC.PackageImports
        Cabal.ParallelArrays -> xopt GHC.ParallelArrays
        Cabal.ParallelListComp -> xopt GHC.ParallelListComp
        Cabal.PartialTypeSignatures -> xopt GHC.PartialTypeSignatures
        Cabal.PatternGuards -> xopt GHC.PatternGuards
        Cabal.PatternSignatures -> xopt GHC.ScopedTypeVariables
        Cabal.PatternSynonyms -> xopt GHC.PatternSynonyms
        Cabal.PolyKinds -> xopt GHC.PolyKinds
        Cabal.PolymorphicComponents -> xopt GHC.RankNTypes
        Cabal.PostfixOperators -> xopt GHC.PostfixOperators
--        Cabal.QualifiedDo -> error "GHC 8 does not support the QualifiedDo language extension"
        Cabal.QuantifiedConstraints -> xopt GHC.QuantifiedConstraints
        Cabal.QuasiQuotes -> xopt GHC.QuasiQuotes
        Cabal.Rank2Types -> xopt GHC.RankNTypes
        Cabal.RankNTypes -> xopt GHC.RankNTypes
        Cabal.RebindableSyntax -> xopt GHC.RebindableSyntax
        Cabal.RecordPuns -> xopt GHC.RecordPuns
        Cabal.RecordWildCards -> xopt GHC.RecordWildCards
        Cabal.RecursiveDo -> xopt GHC.RecursiveDo
        Cabal.RegularPatterns -> error "GHC 8 does not support the RegularPatterns language extension"
--        					   RelaxedLayout
        Cabal.RelaxedPolyRec -> xopt GHC.RelaxedPolyRec
        Cabal.RestrictedTypeSynonyms -> error "GHC 8 does not support the RestrictedTypeSynonyms language extension"
        Cabal.RoleAnnotations -> xopt GHC.RoleAnnotations
        Cabal.Safe -> sopt GHC.Sf_Safe
        Cabal.SafeImports -> error "GHC 8 does not support the SafeImports language extension"
        Cabal.ScopedTypeVariables -> xopt GHC.ScopedTypeVariables
        Cabal.StandaloneDeriving -> xopt GHC.StandaloneDeriving
        Cabal.StandaloneKindSignatures -> xopt GHC.StandaloneKindSignatures
        Cabal.StarIsType -> xopt GHC.StarIsType
        Cabal.StaticPointers -> xopt GHC.StaticPointers
        Cabal.Strict -> xopt GHC.Strict
        Cabal.StrictData -> xopt GHC.StrictData
        Cabal.TemplateHaskell -> xopt GHC.TemplateHaskell
        Cabal.TemplateHaskellQuotes -> xopt GHC.TemplateHaskellQuotes
        Cabal.TraditionalRecordSyntax -> xopt GHC.TraditionalRecordSyntax
        Cabal.TransformListComp -> xopt GHC.TransformListComp
        Cabal.Trustworthy -> sopt GHC.Sf_Trustworthy
        Cabal.TupleSections -> xopt GHC.TupleSections
        Cabal.TypeApplications -> xopt GHC.TypeApplications
        Cabal.TypeFamilies -> xopt GHC.TypeFamilies
        Cabal.TypeFamilyDependencies -> xopt GHC.TypeFamilyDependencies
        Cabal.TypeInType -> xopt GHC.TypeInType
        Cabal.TypeOperators -> xopt GHC.TypeOperators
        Cabal.TypeSynonymInstances -> xopt GHC.TypeSynonymInstances
        Cabal.UnboxedSums -> xopt GHC.UnboxedSums
        Cabal.UnboxedTuples -> xopt GHC.UnboxedTuples
        Cabal.UndecidableInstances -> xopt GHC.UndecidableInstances
        Cabal.UndecidableSuperClasses -> xopt GHC.UndecidableSuperClasses
        Cabal.UnicodeSyntax -> xopt GHC.UnicodeSyntax
        Cabal.UnliftedFFITypes -> xopt GHC.UnliftedFFITypes
        Cabal.UnliftedNewtypes -> xopt GHC.UnliftedNewtypes
        Cabal.Unsafe -> sopt GHC.Sf_Unsafe
        Cabal.ViewPatterns -> xopt GHC.ViewPatterns
        Cabal.XmlSyntax -> error "GHC 8 does not support the XmlSyntax language extension"

-----

addOptP :: String -> GHC.DynFlags -> GHC.DynFlags
addOptP f = alterToolSettings (\s -> s { GHC.toolSettings_opt_P = f : GHC.toolSettings_opt_P s})

alterToolSettings :: (GHC.ToolSettings -> GHC.ToolSettings) -> GHC.DynFlags -> GHC.DynFlags
alterToolSettings f dynFlags = dynFlags { GHC.toolSettings = f (GHC.toolSettings dynFlags) }

-----

-- copied from https://github.com/ghc/ghc/blob/ghc-8.10.7-release/compiler/main/DynFlags.hs

setExtensionFlag', unSetExtensionFlag' :: GHC.Extension -> GHC.DynFlags -> GHC.DynFlags
setExtensionFlag' f dflags = foldr ($) (GHC.xopt_set dflags f) deps
  where
    deps = [ if turn_on then setExtensionFlag'   d
                        else unSetExtensionFlag' d
           | (f', turn_on, d) <- impliedXFlags, f' == f ]

unSetExtensionFlag' f dflags = GHC.xopt_unset dflags f
   -- When you un-set f, however, we don't un-set the things it implies
   --      (except for -fno-glasgow-exts, which is treated specially)

type TurnOnFlag = Bool

impliedXFlags :: [(GHC.Extension, TurnOnFlag, GHC.Extension)]
impliedXFlags
-- See Note [Updating flag description in the User's Guide]
  = [ (GHC.RankNTypes,                turnOn, GHC.ExplicitForAll)
    , (GHC.QuantifiedConstraints,     turnOn, GHC.ExplicitForAll)
    , (GHC.ScopedTypeVariables,       turnOn, GHC.ExplicitForAll)
    , (GHC.LiberalTypeSynonyms,       turnOn, GHC.ExplicitForAll)
    , (GHC.ExistentialQuantification, turnOn, GHC.ExplicitForAll)
    , (GHC.FlexibleInstances,         turnOn, GHC.TypeSynonymInstances)
    , (GHC.FunctionalDependencies,    turnOn, GHC.MultiParamTypeClasses)
    , (GHC.MultiParamTypeClasses,     turnOn, GHC.ConstrainedClassMethods)  -- c.f. #7854
    , (GHC.TypeFamilyDependencies,    turnOn, GHC.TypeFamilies)

    , (GHC.RebindableSyntax, turnOff, GHC.ImplicitPrelude)      -- NB: turn off!

    , (GHC.DerivingVia, turnOn, GHC.DerivingStrategies)

    , (GHC.GADTs,            turnOn, GHC.GADTSyntax)
    , (GHC.GADTs,            turnOn, GHC.MonoLocalBinds)
    , (GHC.TypeFamilies,     turnOn, GHC.MonoLocalBinds)

    , (GHC.TypeFamilies,     turnOn, GHC.KindSignatures)  -- Type families use kind signatures
    , (GHC.PolyKinds,        turnOn, GHC.KindSignatures)  -- Ditto polymorphic kinds

    -- TypeInType is now just a synonym for a couple of other extensions.
    , (GHC.TypeInType,       turnOn, GHC.DataKinds)
    , (GHC.TypeInType,       turnOn, GHC.PolyKinds)
    , (GHC.TypeInType,       turnOn, GHC.KindSignatures)

    -- Standalone kind signatures are a replacement for CUSKs.
    , (GHC.StandaloneKindSignatures, turnOff, GHC.CUSKs)

    -- AutoDeriveTypeable is not very useful without DeriveDataTypeable
    , (GHC.AutoDeriveTypeable, turnOn, GHC.DeriveDataTypeable)

    -- We turn this on so that we can export associated type
    -- type synonyms in subordinates (e.g. MyClass(type AssocType))
    , (GHC.TypeFamilies,     turnOn, GHC.ExplicitNamespaces)
    , (GHC.TypeOperators, turnOn, GHC.ExplicitNamespaces)

    , (GHC.ImpredicativeTypes,  turnOn, GHC.RankNTypes)

        -- Record wild-cards implies field disambiguation
        -- Otherwise if you write (C {..}) you may well get
        -- stuff like " 'a' not in scope ", which is a bit silly
        -- if the compiler has just filled in field 'a' of constructor 'C'
    , (GHC.RecordWildCards,     turnOn, GHC.DisambiguateRecordFields)

    , (GHC.ParallelArrays, turnOn, GHC.ParallelListComp)

    , (GHC.JavaScriptFFI, turnOn, GHC.InterruptibleFFI)

    , (GHC.DeriveTraversable, turnOn, GHC.DeriveFunctor)
    , (GHC.DeriveTraversable, turnOn, GHC.DeriveFoldable)

    -- Duplicate record fields require field disambiguation
    , (GHC.DuplicateRecordFields, turnOn, GHC.DisambiguateRecordFields)

    , (GHC.TemplateHaskell, turnOn, GHC.TemplateHaskellQuotes)
    , (GHC.Strict, turnOn, GHC.StrictData)
    ]
  where
    turnOn  = True
    turnOff = False

-----

runGhc :: GHC.Ghc a -> IO a
runGhc x = GHC.runGhc (Just GHC.Paths.libdir) x
