{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Hercules.Database where

import           Data.Aeson
import qualified Data.Aeson                 as JSON
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Text
import           GHC.Generics
import           GHC.Int
import           Opaleye
import           Servant.Elm

---- Types for table: aggregateconstituents ----

data Aggregateconstituent' c1 c2 =
  Aggregateconstituent
    { aggregateconstituentAggregate   :: c1
    , aggregateconstituentConstituent :: c2
    }

type Aggregateconstituent = Aggregateconstituent' Int32 Int32

type AggregateconstituentReadColumns = Aggregateconstituent' (Column PGInt4) (Column PGInt4)

type AggregateconstituentWriteColumns = Aggregateconstituent' (Column PGInt4) (Column PGInt4)

type AggregateconstituentNullableColumns = Aggregateconstituent' (Column (Nullable PGInt4)) (Column (Nullable PGInt4))

type AggregateconstituentMaybe = Aggregateconstituent' (Maybe Int32) (Maybe Int32)

sequenceAggregateconstituent :: AggregateconstituentMaybe -> Maybe Aggregateconstituent
sequenceAggregateconstituent (Aggregateconstituent c1 c2) = pure Aggregateconstituent <*> c1 <*> c2

$(makeAdaptorAndInstance "pAggregateconstituent" ''Aggregateconstituent')

aggregateconstituentTable :: Table AggregateconstituentWriteColumns AggregateconstituentReadColumns
aggregateconstituentTable = Table "aggregateconstituents" (pAggregateconstituent
  Aggregateconstituent
    { aggregateconstituentAggregate = required "aggregate"
    , aggregateconstituentConstituent = required "constituent"
    }
  )

---- Types for table: buildinputs ----

data Buildinput' c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 =
  Buildinput
    { buildinputId               :: c1
    , buildinputBuild            :: c2
    , buildinputName             :: c3
    , buildinputType             :: c4
    , buildinputUri              :: c5
    , buildinputRevision         :: c6
    , buildinputValue            :: c7
    , buildinputEmailresponsible :: c8
    , buildinputDependency       :: c9
    , buildinputPath             :: c10
    , buildinputSha256Hash       :: c11
    }

type Buildinput = Buildinput' Int32 (Maybe Int32) Text Text (Maybe Text) (Maybe Text) (Maybe Text) Int32 (Maybe Int32) (Maybe Text) (Maybe Text)

type BuildinputReadColumns = Buildinput' (Column PGInt4) (Column (Nullable PGInt4)) (Column PGText) (Column PGText) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column PGInt4) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText))

type BuildinputWriteColumns = Buildinput' (Maybe (Column PGInt4)) (Maybe (Column (Nullable PGInt4))) (Column PGText) (Column PGText) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Column PGInt4) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText)))

type BuildinputNullableColumns = Buildinput' (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText))

type BuildinputMaybe = Buildinput' (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe Text)

sequenceBuildinput :: BuildinputMaybe -> Maybe Buildinput
sequenceBuildinput (Buildinput c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11) = pure Buildinput <*> c1 <*> pure c2 <*> c3 <*> c4 <*> pure c5 <*> pure c6 <*> pure c7 <*> c8 <*> pure c9 <*> pure c10 <*> pure c11

$(makeAdaptorAndInstance "pBuildinput" ''Buildinput')

buildinputTable :: Table BuildinputWriteColumns BuildinputReadColumns
buildinputTable = Table "buildinputs" (pBuildinput
  Buildinput
    { buildinputId = optional "id"
    , buildinputBuild = optional "build"
    , buildinputName = required "name"
    , buildinputType = required "type"
    , buildinputUri = optional "uri"
    , buildinputRevision = optional "revision"
    , buildinputValue = optional "value"
    , buildinputEmailresponsible = required "emailresponsible"
    , buildinputDependency = optional "dependency"
    , buildinputPath = optional "path"
    , buildinputSha256Hash = optional "sha256hash"
    }
  )

---- Types for table: buildmetrics ----

data Buildmetric' c1 c2 c3 c4 c5 c6 c7 c8 =
  Buildmetric
    { buildmetricBuild     :: c1
    , buildmetricName      :: c2
    , buildmetricUnit      :: c3
    , buildmetricValue     :: c4
    , buildmetricProject   :: c5
    , buildmetricJobset    :: c6
    , buildmetricJob       :: c7
    , buildmetricTimestamp :: c8
    }

type Buildmetric = Buildmetric' Int32 Text (Maybe Text) Double Text Text Text Int32

type BuildmetricReadColumns = Buildmetric' (Column PGInt4) (Column PGText) (Column (Nullable PGText)) (Column PGFloat8) (Column PGText) (Column PGText) (Column PGText) (Column PGInt4)

type BuildmetricWriteColumns = Buildmetric' (Column PGInt4) (Column PGText) (Maybe (Column (Nullable PGText))) (Column PGFloat8) (Column PGText) (Column PGText) (Column PGText) (Column PGInt4)

type BuildmetricNullableColumns = Buildmetric' (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGFloat8)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4))

type BuildmetricMaybe = Buildmetric' (Maybe Int32) (Maybe Text) (Maybe Text) (Maybe Double) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Int32)

sequenceBuildmetric :: BuildmetricMaybe -> Maybe Buildmetric
sequenceBuildmetric (Buildmetric c1 c2 c3 c4 c5 c6 c7 c8) = pure Buildmetric <*> c1 <*> c2 <*> pure c3 <*> c4 <*> c5 <*> c6 <*> c7 <*> c8

$(makeAdaptorAndInstance "pBuildmetric" ''Buildmetric')

buildmetricTable :: Table BuildmetricWriteColumns BuildmetricReadColumns
buildmetricTable = Table "buildmetrics" (pBuildmetric
  Buildmetric
    { buildmetricBuild = required "build"
    , buildmetricName = required "name"
    , buildmetricUnit = optional "unit"
    , buildmetricValue = required "value"
    , buildmetricProject = required "project"
    , buildmetricJobset = required "jobset"
    , buildmetricJob = required "job"
    , buildmetricTimestamp = required "timestamp"
    }
  )

---- Types for table: buildoutputs ----

data Buildoutput' c1 c2 c3 =
  Buildoutput
    { buildoutputBuild :: c1
    , buildoutputName  :: c2
    , buildoutputPath  :: c3
    }

type Buildoutput = Buildoutput' Int32 Text Text

type BuildoutputReadColumns = Buildoutput' (Column PGInt4) (Column PGText) (Column PGText)

type BuildoutputWriteColumns = Buildoutput' (Column PGInt4) (Column PGText) (Column PGText)

type BuildoutputNullableColumns = Buildoutput' (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText))

type BuildoutputMaybe = Buildoutput' (Maybe Int32) (Maybe Text) (Maybe Text)

sequenceBuildoutput :: BuildoutputMaybe -> Maybe Buildoutput
sequenceBuildoutput (Buildoutput c1 c2 c3) = pure Buildoutput <*> c1 <*> c2 <*> c3

$(makeAdaptorAndInstance "pBuildoutput" ''Buildoutput')

buildoutputTable :: Table BuildoutputWriteColumns BuildoutputReadColumns
buildoutputTable = Table "buildoutputs" (pBuildoutput
  Buildoutput
    { buildoutputBuild = required "build"
    , buildoutputName = required "name"
    , buildoutputPath = required "path"
    }
  )

---- Types for table: buildproducts ----

data Buildproduct' c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 =
  Buildproduct
    { buildproductBuild       :: c1
    , buildproductProductnr   :: c2
    , buildproductType        :: c3
    , buildproductSubtype     :: c4
    , buildproductFilesize    :: c5
    , buildproductSha1Hash    :: c6
    , buildproductSha256Hash  :: c7
    , buildproductPath        :: c8
    , buildproductName        :: c9
    , buildproductDescription :: c10
    , buildproductDefaultpath :: c11
    }

type Buildproduct = Buildproduct' Int32 Int32 Text Text (Maybe Int64) (Maybe Text) (Maybe Text) (Maybe Text) Text (Maybe Text) (Maybe Text)

type BuildproductReadColumns = Buildproduct' (Column PGInt4) (Column PGInt4) (Column PGText) (Column PGText) (Column (Nullable PGInt8)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column PGText) (Column (Nullable PGText)) (Column (Nullable PGText))

type BuildproductWriteColumns = Buildproduct' (Column PGInt4) (Column PGInt4) (Column PGText) (Column PGText) (Maybe (Column (Nullable PGInt8))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Column PGText) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText)))

type BuildproductNullableColumns = Buildproduct' (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt8)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText))

type BuildproductMaybe = Buildproduct' (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe Text) (Maybe Int64) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text)

sequenceBuildproduct :: BuildproductMaybe -> Maybe Buildproduct
sequenceBuildproduct (Buildproduct c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11) = pure Buildproduct <*> c1 <*> c2 <*> c3 <*> c4 <*> pure c5 <*> pure c6 <*> pure c7 <*> pure c8 <*> c9 <*> pure c10 <*> pure c11

$(makeAdaptorAndInstance "pBuildproduct" ''Buildproduct')

buildproductTable :: Table BuildproductWriteColumns BuildproductReadColumns
buildproductTable = Table "buildproducts" (pBuildproduct
  Buildproduct
    { buildproductBuild = required "build"
    , buildproductProductnr = required "productnr"
    , buildproductType = required "type"
    , buildproductSubtype = required "subtype"
    , buildproductFilesize = optional "filesize"
    , buildproductSha1Hash = optional "sha1hash"
    , buildproductSha256Hash = optional "sha256hash"
    , buildproductPath = optional "path"
    , buildproductName = required "name"
    , buildproductDescription = optional "description"
    , buildproductDefaultpath = optional "defaultpath"
    }
  )

---- Types for table: builds ----

data Build' c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 =
  Build
    { buildId             :: c1
    , buildFinished       :: c2
    , buildTimestamp      :: c3
    , buildProject        :: c4
    , buildJobset         :: c5
    , buildJob            :: c6
    , buildNixname        :: c7
    , buildDescription    :: c8
    , buildDrvpath        :: c9
    , buildSystem         :: c10
    , buildLicense        :: c11
    , buildHomepage       :: c12
    , buildMaintainers    :: c13
    , buildMaxsilent      :: c14
    , buildTimeout        :: c15
    , buildIschannel      :: c16
    , buildIscurrent      :: c17
    , buildNixexprinput   :: c18
    , buildNixexprpath    :: c19
    , buildPriority       :: c20
    , buildGlobalpriority :: c21
    , buildStarttime      :: c22
    , buildStoptime       :: c23
    , buildIscachedbuild  :: c24
    , buildBuildstatus    :: c25
    , buildSize           :: c26
    , buildClosuresize    :: c27
    , buildReleasename    :: c28
    , buildKeep           :: c29
    }

type Build = Build' Int32 Int32 Int32 Text Text Text (Maybe Text) (Maybe Text) Text Text (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Int32) (Maybe Int32) Int32 (Maybe Int32) (Maybe Text) (Maybe Text) Int32 Int32 (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Int64) (Maybe Int64) (Maybe Text) Int32

type BuildReadColumns = Build' (Column PGInt4) (Column PGInt4) (Column PGInt4) (Column PGText) (Column PGText) (Column PGText) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column PGText) (Column PGText) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column PGInt4) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column PGInt4) (Column PGInt4) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt8)) (Column (Nullable PGInt8)) (Column (Nullable PGText)) (Column PGInt4)

type BuildWriteColumns = Build' (Maybe (Column PGInt4)) (Column PGInt4) (Column PGInt4) (Column PGText) (Column PGText) (Column PGText) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Column PGText) (Column PGText) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Column PGInt4) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Column PGInt4) (Column PGInt4) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt8))) (Maybe (Column (Nullable PGInt8))) (Maybe (Column (Nullable PGText))) (Column PGInt4)

type BuildNullableColumns = Build' (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt8)) (Column (Nullable PGInt8)) (Column (Nullable PGText)) (Column (Nullable PGInt4))

type BuildMaybe = Build' (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe Text) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Int64) (Maybe Int64) (Maybe Text) (Maybe Int32)

sequenceBuild :: BuildMaybe -> Maybe Build
sequenceBuild (Build c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29) = pure Build <*> c1 <*> c2 <*> c3 <*> c4 <*> c5 <*> c6 <*> pure c7 <*> pure c8 <*> c9 <*> c10 <*> pure c11 <*> pure c12 <*> pure c13 <*> pure c14 <*> pure c15 <*> c16 <*> pure c17 <*> pure c18 <*> pure c19 <*> c20 <*> c21 <*> pure c22 <*> pure c23 <*> pure c24 <*> pure c25 <*> pure c26 <*> pure c27 <*> pure c28 <*> c29

$(makeAdaptorAndInstance "pBuild" ''Build')

buildTable :: Table BuildWriteColumns BuildReadColumns
buildTable = Table "builds" (pBuild
  Build
    { buildId = optional "id"
    , buildFinished = required "finished"
    , buildTimestamp = required "timestamp"
    , buildProject = required "project"
    , buildJobset = required "jobset"
    , buildJob = required "job"
    , buildNixname = optional "nixname"
    , buildDescription = optional "description"
    , buildDrvpath = required "drvpath"
    , buildSystem = required "system"
    , buildLicense = optional "license"
    , buildHomepage = optional "homepage"
    , buildMaintainers = optional "maintainers"
    , buildMaxsilent = optional "maxsilent"
    , buildTimeout = optional "timeout"
    , buildIschannel = required "ischannel"
    , buildIscurrent = optional "iscurrent"
    , buildNixexprinput = optional "nixexprinput"
    , buildNixexprpath = optional "nixexprpath"
    , buildPriority = required "priority"
    , buildGlobalpriority = required "globalpriority"
    , buildStarttime = optional "starttime"
    , buildStoptime = optional "stoptime"
    , buildIscachedbuild = optional "iscachedbuild"
    , buildBuildstatus = optional "buildstatus"
    , buildSize = optional "size"
    , buildClosuresize = optional "closuresize"
    , buildReleasename = optional "releasename"
    , buildKeep = required "keep"
    }
  )

---- Types for table: buildstepoutputs ----

data Buildstepoutput' c1 c2 c3 c4 =
  Buildstepoutput
    { buildstepoutputBuild  :: c1
    , buildstepoutputStepnr :: c2
    , buildstepoutputName   :: c3
    , buildstepoutputPath   :: c4
    }

type Buildstepoutput = Buildstepoutput' Int32 Int32 Text Text

type BuildstepoutputReadColumns = Buildstepoutput' (Column PGInt4) (Column PGInt4) (Column PGText) (Column PGText)

type BuildstepoutputWriteColumns = Buildstepoutput' (Column PGInt4) (Column PGInt4) (Column PGText) (Column PGText)

type BuildstepoutputNullableColumns = Buildstepoutput' (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText))

type BuildstepoutputMaybe = Buildstepoutput' (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe Text)

sequenceBuildstepoutput :: BuildstepoutputMaybe -> Maybe Buildstepoutput
sequenceBuildstepoutput (Buildstepoutput c1 c2 c3 c4) = pure Buildstepoutput <*> c1 <*> c2 <*> c3 <*> c4

$(makeAdaptorAndInstance "pBuildstepoutput" ''Buildstepoutput')

buildstepoutputTable :: Table BuildstepoutputWriteColumns BuildstepoutputReadColumns
buildstepoutputTable = Table "buildstepoutputs" (pBuildstepoutput
  Buildstepoutput
    { buildstepoutputBuild = required "build"
    , buildstepoutputStepnr = required "stepnr"
    , buildstepoutputName = required "name"
    , buildstepoutputPath = required "path"
    }
  )

---- Types for table: buildsteps ----

data Buildstep' c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 =
  Buildstep
    { buildstepBuild          :: c1
    , buildstepStepnr         :: c2
    , buildstepType           :: c3
    , buildstepDrvpath        :: c4
    , buildstepBusy           :: c5
    , buildstepStatus         :: c6
    , buildstepErrormsg       :: c7
    , buildstepStarttime      :: c8
    , buildstepStoptime       :: c9
    , buildstepMachine        :: c10
    , buildstepSystem         :: c11
    , buildstepPropagatedfrom :: c12
    , buildstepOverhead       :: c13
    }

type Buildstep = Buildstep' Int32 Int32 Int32 (Maybe Text) Int32 (Maybe Int32) (Maybe Text) (Maybe Int32) (Maybe Int32) Text (Maybe Text) (Maybe Int32) (Maybe Int32)

type BuildstepReadColumns = Buildstep' (Column PGInt4) (Column PGInt4) (Column PGInt4) (Column (Nullable PGText)) (Column PGInt4) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column PGText) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4))

type BuildstepWriteColumns = Buildstep' (Column PGInt4) (Column PGInt4) (Column PGInt4) (Maybe (Column (Nullable PGText))) (Column PGInt4) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Column PGText) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4)))

type BuildstepNullableColumns = Buildstep' (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4))

type BuildstepMaybe = Buildstep' (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe Text) (Maybe Int32) (Maybe Int32)

sequenceBuildstep :: BuildstepMaybe -> Maybe Buildstep
sequenceBuildstep (Buildstep c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13) = pure Buildstep <*> c1 <*> c2 <*> c3 <*> pure c4 <*> c5 <*> pure c6 <*> pure c7 <*> pure c8 <*> pure c9 <*> c10 <*> pure c11 <*> pure c12 <*> pure c13

$(makeAdaptorAndInstance "pBuildstep" ''Buildstep')

buildstepTable :: Table BuildstepWriteColumns BuildstepReadColumns
buildstepTable = Table "buildsteps" (pBuildstep
  Buildstep
    { buildstepBuild = required "build"
    , buildstepStepnr = required "stepnr"
    , buildstepType = required "type"
    , buildstepDrvpath = optional "drvpath"
    , buildstepBusy = required "busy"
    , buildstepStatus = optional "status"
    , buildstepErrormsg = optional "errormsg"
    , buildstepStarttime = optional "starttime"
    , buildstepStoptime = optional "stoptime"
    , buildstepMachine = required "machine"
    , buildstepSystem = optional "system"
    , buildstepPropagatedfrom = optional "propagatedfrom"
    , buildstepOverhead = optional "overhead"
    }
  )

---- Types for table: cachedbazaarinputs ----

data Cachedbazaarinput' c1 c2 c3 c4 =
  Cachedbazaarinput
    { cachedbazaarinputUri        :: c1
    , cachedbazaarinputRevision   :: c2
    , cachedbazaarinputSha256Hash :: c3
    , cachedbazaarinputStorepath  :: c4
    }

type Cachedbazaarinput = Cachedbazaarinput' Text Int32 Text Text

type CachedbazaarinputReadColumns = Cachedbazaarinput' (Column PGText) (Column PGInt4) (Column PGText) (Column PGText)

type CachedbazaarinputWriteColumns = Cachedbazaarinput' (Column PGText) (Column PGInt4) (Column PGText) (Column PGText)

type CachedbazaarinputNullableColumns = Cachedbazaarinput' (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText))

type CachedbazaarinputMaybe = Cachedbazaarinput' (Maybe Text) (Maybe Int32) (Maybe Text) (Maybe Text)

sequenceCachedbazaarinput :: CachedbazaarinputMaybe -> Maybe Cachedbazaarinput
sequenceCachedbazaarinput (Cachedbazaarinput c1 c2 c3 c4) = pure Cachedbazaarinput <*> c1 <*> c2 <*> c3 <*> c4

$(makeAdaptorAndInstance "pCachedbazaarinput" ''Cachedbazaarinput')

cachedbazaarinputTable :: Table CachedbazaarinputWriteColumns CachedbazaarinputReadColumns
cachedbazaarinputTable = Table "cachedbazaarinputs" (pCachedbazaarinput
  Cachedbazaarinput
    { cachedbazaarinputUri = required "uri"
    , cachedbazaarinputRevision = required "revision"
    , cachedbazaarinputSha256Hash = required "sha256hash"
    , cachedbazaarinputStorepath = required "storepath"
    }
  )

---- Types for table: cachedcvsinputs ----

data Cachedcvsinput' c1 c2 c3 c4 c5 c6 =
  Cachedcvsinput
    { cachedcvsinputUri        :: c1
    , cachedcvsinputModule     :: c2
    , cachedcvsinputTimestamp  :: c3
    , cachedcvsinputLastseen   :: c4
    , cachedcvsinputSha256Hash :: c5
    , cachedcvsinputStorepath  :: c6
    }

type Cachedcvsinput = Cachedcvsinput' Text Text Int32 Int32 Text Text

type CachedcvsinputReadColumns = Cachedcvsinput' (Column PGText) (Column PGText) (Column PGInt4) (Column PGInt4) (Column PGText) (Column PGText)

type CachedcvsinputWriteColumns = Cachedcvsinput' (Column PGText) (Column PGText) (Column PGInt4) (Column PGInt4) (Column PGText) (Column PGText)

type CachedcvsinputNullableColumns = Cachedcvsinput' (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText))

type CachedcvsinputMaybe = Cachedcvsinput' (Maybe Text) (Maybe Text) (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe Text)

sequenceCachedcvsinput :: CachedcvsinputMaybe -> Maybe Cachedcvsinput
sequenceCachedcvsinput (Cachedcvsinput c1 c2 c3 c4 c5 c6) = pure Cachedcvsinput <*> c1 <*> c2 <*> c3 <*> c4 <*> c5 <*> c6

$(makeAdaptorAndInstance "pCachedcvsinput" ''Cachedcvsinput')

cachedcvsinputTable :: Table CachedcvsinputWriteColumns CachedcvsinputReadColumns
cachedcvsinputTable = Table "cachedcvsinputs" (pCachedcvsinput
  Cachedcvsinput
    { cachedcvsinputUri = required "uri"
    , cachedcvsinputModule = required "module"
    , cachedcvsinputTimestamp = required "timestamp"
    , cachedcvsinputLastseen = required "lastseen"
    , cachedcvsinputSha256Hash = required "sha256hash"
    , cachedcvsinputStorepath = required "storepath"
    }
  )

---- Types for table: cacheddarcsinputs ----

data Cacheddarcsinput' c1 c2 c3 c4 c5 =
  Cacheddarcsinput
    { cacheddarcsinputUri        :: c1
    , cacheddarcsinputRevision   :: c2
    , cacheddarcsinputSha256Hash :: c3
    , cacheddarcsinputStorepath  :: c4
    , cacheddarcsinputRevcount   :: c5
    }

type Cacheddarcsinput = Cacheddarcsinput' Text Text Text Text Int32

type CacheddarcsinputReadColumns = Cacheddarcsinput' (Column PGText) (Column PGText) (Column PGText) (Column PGText) (Column PGInt4)

type CacheddarcsinputWriteColumns = Cacheddarcsinput' (Column PGText) (Column PGText) (Column PGText) (Column PGText) (Column PGInt4)

type CacheddarcsinputNullableColumns = Cacheddarcsinput' (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4))

type CacheddarcsinputMaybe = Cacheddarcsinput' (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Int32)

sequenceCacheddarcsinput :: CacheddarcsinputMaybe -> Maybe Cacheddarcsinput
sequenceCacheddarcsinput (Cacheddarcsinput c1 c2 c3 c4 c5) = pure Cacheddarcsinput <*> c1 <*> c2 <*> c3 <*> c4 <*> c5

$(makeAdaptorAndInstance "pCacheddarcsinput" ''Cacheddarcsinput')

cacheddarcsinputTable :: Table CacheddarcsinputWriteColumns CacheddarcsinputReadColumns
cacheddarcsinputTable = Table "cacheddarcsinputs" (pCacheddarcsinput
  Cacheddarcsinput
    { cacheddarcsinputUri = required "uri"
    , cacheddarcsinputRevision = required "revision"
    , cacheddarcsinputSha256Hash = required "sha256hash"
    , cacheddarcsinputStorepath = required "storepath"
    , cacheddarcsinputRevcount = required "revcount"
    }
  )

---- Types for table: cachedgitinputs ----

data Cachedgitinput' c1 c2 c3 c4 c5 =
  Cachedgitinput
    { cachedgitinputUri        :: c1
    , cachedgitinputBranch     :: c2
    , cachedgitinputRevision   :: c3
    , cachedgitinputSha256Hash :: c4
    , cachedgitinputStorepath  :: c5
    }

type Cachedgitinput = Cachedgitinput' Text Text Text Text Text

type CachedgitinputReadColumns = Cachedgitinput' (Column PGText) (Column PGText) (Column PGText) (Column PGText) (Column PGText)

type CachedgitinputWriteColumns = Cachedgitinput' (Column PGText) (Column PGText) (Column PGText) (Column PGText) (Column PGText)

type CachedgitinputNullableColumns = Cachedgitinput' (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText))

type CachedgitinputMaybe = Cachedgitinput' (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text)

sequenceCachedgitinput :: CachedgitinputMaybe -> Maybe Cachedgitinput
sequenceCachedgitinput (Cachedgitinput c1 c2 c3 c4 c5) = pure Cachedgitinput <*> c1 <*> c2 <*> c3 <*> c4 <*> c5

$(makeAdaptorAndInstance "pCachedgitinput" ''Cachedgitinput')

cachedgitinputTable :: Table CachedgitinputWriteColumns CachedgitinputReadColumns
cachedgitinputTable = Table "cachedgitinputs" (pCachedgitinput
  Cachedgitinput
    { cachedgitinputUri = required "uri"
    , cachedgitinputBranch = required "branch"
    , cachedgitinputRevision = required "revision"
    , cachedgitinputSha256Hash = required "sha256hash"
    , cachedgitinputStorepath = required "storepath"
    }
  )

---- Types for table: cachedhginputs ----

data Cachedhginput' c1 c2 c3 c4 c5 =
  Cachedhginput
    { cachedhginputUri        :: c1
    , cachedhginputBranch     :: c2
    , cachedhginputRevision   :: c3
    , cachedhginputSha256Hash :: c4
    , cachedhginputStorepath  :: c5
    }

type Cachedhginput = Cachedhginput' Text Text Text Text Text

type CachedhginputReadColumns = Cachedhginput' (Column PGText) (Column PGText) (Column PGText) (Column PGText) (Column PGText)

type CachedhginputWriteColumns = Cachedhginput' (Column PGText) (Column PGText) (Column PGText) (Column PGText) (Column PGText)

type CachedhginputNullableColumns = Cachedhginput' (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText))

type CachedhginputMaybe = Cachedhginput' (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text)

sequenceCachedhginput :: CachedhginputMaybe -> Maybe Cachedhginput
sequenceCachedhginput (Cachedhginput c1 c2 c3 c4 c5) = pure Cachedhginput <*> c1 <*> c2 <*> c3 <*> c4 <*> c5

$(makeAdaptorAndInstance "pCachedhginput" ''Cachedhginput')

cachedhginputTable :: Table CachedhginputWriteColumns CachedhginputReadColumns
cachedhginputTable = Table "cachedhginputs" (pCachedhginput
  Cachedhginput
    { cachedhginputUri = required "uri"
    , cachedhginputBranch = required "branch"
    , cachedhginputRevision = required "revision"
    , cachedhginputSha256Hash = required "sha256hash"
    , cachedhginputStorepath = required "storepath"
    }
  )

---- Types for table: cachedpathinputs ----

data Cachedpathinput' c1 c2 c3 c4 c5 =
  Cachedpathinput
    { cachedpathinputSrcpath    :: c1
    , cachedpathinputTimestamp  :: c2
    , cachedpathinputLastseen   :: c3
    , cachedpathinputSha256Hash :: c4
    , cachedpathinputStorepath  :: c5
    }

type Cachedpathinput = Cachedpathinput' Text Int32 Int32 Text Text

type CachedpathinputReadColumns = Cachedpathinput' (Column PGText) (Column PGInt4) (Column PGInt4) (Column PGText) (Column PGText)

type CachedpathinputWriteColumns = Cachedpathinput' (Column PGText) (Column PGInt4) (Column PGInt4) (Column PGText) (Column PGText)

type CachedpathinputNullableColumns = Cachedpathinput' (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText))

type CachedpathinputMaybe = Cachedpathinput' (Maybe Text) (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe Text)

sequenceCachedpathinput :: CachedpathinputMaybe -> Maybe Cachedpathinput
sequenceCachedpathinput (Cachedpathinput c1 c2 c3 c4 c5) = pure Cachedpathinput <*> c1 <*> c2 <*> c3 <*> c4 <*> c5

$(makeAdaptorAndInstance "pCachedpathinput" ''Cachedpathinput')

cachedpathinputTable :: Table CachedpathinputWriteColumns CachedpathinputReadColumns
cachedpathinputTable = Table "cachedpathinputs" (pCachedpathinput
  Cachedpathinput
    { cachedpathinputSrcpath = required "srcpath"
    , cachedpathinputTimestamp = required "timestamp"
    , cachedpathinputLastseen = required "lastseen"
    , cachedpathinputSha256Hash = required "sha256hash"
    , cachedpathinputStorepath = required "storepath"
    }
  )

---- Types for table: cachedsubversioninputs ----

data Cachedsubversioninput' c1 c2 c3 c4 =
  Cachedsubversioninput
    { cachedsubversioninputUri        :: c1
    , cachedsubversioninputRevision   :: c2
    , cachedsubversioninputSha256Hash :: c3
    , cachedsubversioninputStorepath  :: c4
    }

type Cachedsubversioninput = Cachedsubversioninput' Text Int32 Text Text

type CachedsubversioninputReadColumns = Cachedsubversioninput' (Column PGText) (Column PGInt4) (Column PGText) (Column PGText)

type CachedsubversioninputWriteColumns = Cachedsubversioninput' (Column PGText) (Column PGInt4) (Column PGText) (Column PGText)

type CachedsubversioninputNullableColumns = Cachedsubversioninput' (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText))

type CachedsubversioninputMaybe = Cachedsubversioninput' (Maybe Text) (Maybe Int32) (Maybe Text) (Maybe Text)

sequenceCachedsubversioninput :: CachedsubversioninputMaybe -> Maybe Cachedsubversioninput
sequenceCachedsubversioninput (Cachedsubversioninput c1 c2 c3 c4) = pure Cachedsubversioninput <*> c1 <*> c2 <*> c3 <*> c4

$(makeAdaptorAndInstance "pCachedsubversioninput" ''Cachedsubversioninput')

cachedsubversioninputTable :: Table CachedsubversioninputWriteColumns CachedsubversioninputReadColumns
cachedsubversioninputTable = Table "cachedsubversioninputs" (pCachedsubversioninput
  Cachedsubversioninput
    { cachedsubversioninputUri = required "uri"
    , cachedsubversioninputRevision = required "revision"
    , cachedsubversioninputSha256Hash = required "sha256hash"
    , cachedsubversioninputStorepath = required "storepath"
    }
  )

---- Types for table: failedpaths ----

data Failedpath' c1 =
  Failedpath
    { failedpathPath :: c1
    }

type Failedpath = Failedpath' Text

type FailedpathReadColumns = Failedpath' (Column PGText)

type FailedpathWriteColumns = Failedpath' (Column PGText)

type FailedpathNullableColumns = Failedpath' (Column (Nullable PGText))

type FailedpathMaybe = Failedpath' (Maybe Text)

sequenceFailedpath :: FailedpathMaybe -> Maybe Failedpath
sequenceFailedpath (Failedpath c1) = pure Failedpath <*> c1

$(makeAdaptorAndInstance "pFailedpath" ''Failedpath')

failedpathTable :: Table FailedpathWriteColumns FailedpathReadColumns
failedpathTable = Table "failedpaths" (pFailedpath
  Failedpath
    { failedpathPath = required "path"
    }
  )

---- Types for table: jobs ----

data Job' c1 c2 c3 =
  Job
    { jobProject :: c1
    , jobJobset  :: c2
    , jobName    :: c3
    }

type Job = Job' Text Text Text

type JobReadColumns = Job' (Column PGText) (Column PGText) (Column PGText)

type JobWriteColumns = Job' (Column PGText) (Column PGText) (Column PGText)

type JobNullableColumns = Job' (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText))

type JobMaybe = Job' (Maybe Text) (Maybe Text) (Maybe Text)

sequenceJob :: JobMaybe -> Maybe Job
sequenceJob (Job c1 c2 c3) = pure Job <*> c1 <*> c2 <*> c3

$(makeAdaptorAndInstance "pJob" ''Job')

jobTable :: Table JobWriteColumns JobReadColumns
jobTable = Table "jobs" (pJob
  Job
    { jobProject = required "project"
    , jobJobset = required "jobset"
    , jobName = required "name"
    }
  )

---- Types for table: jobsetevalinputs ----

data Jobsetevalinput' c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 =
  Jobsetevalinput
    { jobsetevalinputEval       :: c1
    , jobsetevalinputName       :: c2
    , jobsetevalinputAltnr      :: c3
    , jobsetevalinputType       :: c4
    , jobsetevalinputUri        :: c5
    , jobsetevalinputRevision   :: c6
    , jobsetevalinputValue      :: c7
    , jobsetevalinputDependency :: c8
    , jobsetevalinputPath       :: c9
    , jobsetevalinputSha256Hash :: c10
    }

type Jobsetevalinput = Jobsetevalinput' Int32 Text Int32 Text (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Int32) (Maybe Text) (Maybe Text)

type JobsetevalinputReadColumns = Jobsetevalinput' (Column PGInt4) (Column PGText) (Column PGInt4) (Column PGText) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText))

type JobsetevalinputWriteColumns = Jobsetevalinput' (Column PGInt4) (Column PGText) (Column PGInt4) (Column PGText) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText)))

type JobsetevalinputNullableColumns = Jobsetevalinput' (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText))

type JobsetevalinputMaybe = Jobsetevalinput' (Maybe Int32) (Maybe Text) (Maybe Int32) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Int32) (Maybe Text) (Maybe Text)

sequenceJobsetevalinput :: JobsetevalinputMaybe -> Maybe Jobsetevalinput
sequenceJobsetevalinput (Jobsetevalinput c1 c2 c3 c4 c5 c6 c7 c8 c9 c10) = pure Jobsetevalinput <*> c1 <*> c2 <*> c3 <*> c4 <*> pure c5 <*> pure c6 <*> pure c7 <*> pure c8 <*> pure c9 <*> pure c10

$(makeAdaptorAndInstance "pJobsetevalinput" ''Jobsetevalinput')

jobsetevalinputTable :: Table JobsetevalinputWriteColumns JobsetevalinputReadColumns
jobsetevalinputTable = Table "jobsetevalinputs" (pJobsetevalinput
  Jobsetevalinput
    { jobsetevalinputEval = required "eval"
    , jobsetevalinputName = required "name"
    , jobsetevalinputAltnr = required "altnr"
    , jobsetevalinputType = required "type"
    , jobsetevalinputUri = optional "uri"
    , jobsetevalinputRevision = optional "revision"
    , jobsetevalinputValue = optional "value"
    , jobsetevalinputDependency = optional "dependency"
    , jobsetevalinputPath = optional "path"
    , jobsetevalinputSha256Hash = optional "sha256hash"
    }
  )

---- Types for table: jobsetevalmembers ----

data Jobsetevalmember' c1 c2 c3 =
  Jobsetevalmember
    { jobsetevalmemberEval  :: c1
    , jobsetevalmemberBuild :: c2
    , jobsetevalmemberIsnew :: c3
    }

type Jobsetevalmember = Jobsetevalmember' Int32 Int32 Int32

type JobsetevalmemberReadColumns = Jobsetevalmember' (Column PGInt4) (Column PGInt4) (Column PGInt4)

type JobsetevalmemberWriteColumns = Jobsetevalmember' (Column PGInt4) (Column PGInt4) (Column PGInt4)

type JobsetevalmemberNullableColumns = Jobsetevalmember' (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4))

type JobsetevalmemberMaybe = Jobsetevalmember' (Maybe Int32) (Maybe Int32) (Maybe Int32)

sequenceJobsetevalmember :: JobsetevalmemberMaybe -> Maybe Jobsetevalmember
sequenceJobsetevalmember (Jobsetevalmember c1 c2 c3) = pure Jobsetevalmember <*> c1 <*> c2 <*> c3

$(makeAdaptorAndInstance "pJobsetevalmember" ''Jobsetevalmember')

jobsetevalmemberTable :: Table JobsetevalmemberWriteColumns JobsetevalmemberReadColumns
jobsetevalmemberTable = Table "jobsetevalmembers" (pJobsetevalmember
  Jobsetevalmember
    { jobsetevalmemberEval = required "eval"
    , jobsetevalmemberBuild = required "build"
    , jobsetevalmemberIsnew = required "isnew"
    }
  )

---- Types for table: jobsetevals ----

data Jobseteval' c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 =
  Jobseteval
    { jobsetevalId           :: c1
    , jobsetevalProject      :: c2
    , jobsetevalJobset       :: c3
    , jobsetevalTimestamp    :: c4
    , jobsetevalCheckouttime :: c5
    , jobsetevalEvaltime     :: c6
    , jobsetevalHasnewbuilds :: c7
    , jobsetevalHash         :: c8
    , jobsetevalNrbuilds     :: c9
    , jobsetevalNrsucceeded  :: c10
    }

type Jobseteval = Jobseteval' Int32 Text Text Int32 Int32 Int32 Int32 Text (Maybe Int32) (Maybe Int32)

type JobsetevalReadColumns = Jobseteval' (Column PGInt4) (Column PGText) (Column PGText) (Column PGInt4) (Column PGInt4) (Column PGInt4) (Column PGInt4) (Column PGText) (Column (Nullable PGInt4)) (Column (Nullable PGInt4))

type JobsetevalWriteColumns = Jobseteval' (Maybe (Column PGInt4)) (Column PGText) (Column PGText) (Column PGInt4) (Column PGInt4) (Column PGInt4) (Column PGInt4) (Column PGText) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4)))

type JobsetevalNullableColumns = Jobseteval' (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4))

type JobsetevalMaybe = Jobseteval' (Maybe Int32) (Maybe Text) (Maybe Text) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe Int32) (Maybe Int32)

sequenceJobseteval :: JobsetevalMaybe -> Maybe Jobseteval
sequenceJobseteval (Jobseteval c1 c2 c3 c4 c5 c6 c7 c8 c9 c10) = pure Jobseteval <*> c1 <*> c2 <*> c3 <*> c4 <*> c5 <*> c6 <*> c7 <*> c8 <*> pure c9 <*> pure c10

$(makeAdaptorAndInstance "pJobseteval" ''Jobseteval')

jobsetevalTable :: Table JobsetevalWriteColumns JobsetevalReadColumns
jobsetevalTable = Table "jobsetevals" (pJobseteval
  Jobseteval
    { jobsetevalId = optional "id"
    , jobsetevalProject = required "project"
    , jobsetevalJobset = required "jobset"
    , jobsetevalTimestamp = required "timestamp"
    , jobsetevalCheckouttime = required "checkouttime"
    , jobsetevalEvaltime = required "evaltime"
    , jobsetevalHasnewbuilds = required "hasnewbuilds"
    , jobsetevalHash = required "hash"
    , jobsetevalNrbuilds = optional "nrbuilds"
    , jobsetevalNrsucceeded = optional "nrsucceeded"
    }
  )

---- Types for table: jobsetinputalts ----

data Jobsetinputalt' c1 c2 c3 c4 c5 c6 =
  Jobsetinputalt
    { jobsetinputaltProject  :: c1
    , jobsetinputaltJobset   :: c2
    , jobsetinputaltInput    :: c3
    , jobsetinputaltAltnr    :: c4
    , jobsetinputaltValue    :: c5
    , jobsetinputaltRevision :: c6
    }

type Jobsetinputalt = Jobsetinputalt' Text Text Text Int32 (Maybe Text) (Maybe Text)

type JobsetinputaltReadColumns = Jobsetinputalt' (Column PGText) (Column PGText) (Column PGText) (Column PGInt4) (Column (Nullable PGText)) (Column (Nullable PGText))

type JobsetinputaltWriteColumns = Jobsetinputalt' (Column PGText) (Column PGText) (Column PGText) (Column PGInt4) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText)))

type JobsetinputaltNullableColumns = Jobsetinputalt' (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText))

type JobsetinputaltMaybe = Jobsetinputalt' (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Int32) (Maybe Text) (Maybe Text)

sequenceJobsetinputalt :: JobsetinputaltMaybe -> Maybe Jobsetinputalt
sequenceJobsetinputalt (Jobsetinputalt c1 c2 c3 c4 c5 c6) = pure Jobsetinputalt <*> c1 <*> c2 <*> c3 <*> c4 <*> pure c5 <*> pure c6

$(makeAdaptorAndInstance "pJobsetinputalt" ''Jobsetinputalt')

jobsetinputaltTable :: Table JobsetinputaltWriteColumns JobsetinputaltReadColumns
jobsetinputaltTable = Table "jobsetinputalts" (pJobsetinputalt
  Jobsetinputalt
    { jobsetinputaltProject = required "project"
    , jobsetinputaltJobset = required "jobset"
    , jobsetinputaltInput = required "input"
    , jobsetinputaltAltnr = required "altnr"
    , jobsetinputaltValue = optional "value"
    , jobsetinputaltRevision = optional "revision"
    }
  )

---- Types for table: jobsetinputs ----

data Jobsetinput' c1 c2 c3 c4 c5 =
  Jobsetinput
    { jobsetinputProject          :: c1
    , jobsetinputJobset           :: c2
    , jobsetinputName             :: c3
    , jobsetinputType             :: c4
    , jobsetinputEmailresponsible :: c5
    }

type Jobsetinput = Jobsetinput' Text Text Text Text Int32

type JobsetinputReadColumns = Jobsetinput' (Column PGText) (Column PGText) (Column PGText) (Column PGText) (Column PGInt4)

type JobsetinputWriteColumns = Jobsetinput' (Column PGText) (Column PGText) (Column PGText) (Column PGText) (Column PGInt4)

type JobsetinputNullableColumns = Jobsetinput' (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4))

type JobsetinputMaybe = Jobsetinput' (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Int32)

sequenceJobsetinput :: JobsetinputMaybe -> Maybe Jobsetinput
sequenceJobsetinput (Jobsetinput c1 c2 c3 c4 c5) = pure Jobsetinput <*> c1 <*> c2 <*> c3 <*> c4 <*> c5

$(makeAdaptorAndInstance "pJobsetinput" ''Jobsetinput')

jobsetinputTable :: Table JobsetinputWriteColumns JobsetinputReadColumns
jobsetinputTable = Table "jobsetinputs" (pJobsetinput
  Jobsetinput
    { jobsetinputProject = required "project"
    , jobsetinputJobset = required "jobset"
    , jobsetinputName = required "name"
    , jobsetinputType = required "type"
    , jobsetinputEmailresponsible = required "emailresponsible"
    }
  )

---- Types for table: jobsetrenames ----

data Jobsetrename' c1 c2 c3 =
  Jobsetrename
    { jobsetrenameProject :: c1
    , jobsetrenameFrom    :: c2
    , jobsetrenameTo      :: c3
    }

type Jobsetrename = Jobsetrename' Text Text Text

type JobsetrenameReadColumns = Jobsetrename' (Column PGText) (Column PGText) (Column PGText)

type JobsetrenameWriteColumns = Jobsetrename' (Column PGText) (Column PGText) (Column PGText)

type JobsetrenameNullableColumns = Jobsetrename' (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText))

type JobsetrenameMaybe = Jobsetrename' (Maybe Text) (Maybe Text) (Maybe Text)

sequenceJobsetrename :: JobsetrenameMaybe -> Maybe Jobsetrename
sequenceJobsetrename (Jobsetrename c1 c2 c3) = pure Jobsetrename <*> c1 <*> c2 <*> c3

$(makeAdaptorAndInstance "pJobsetrename" ''Jobsetrename')

jobsetrenameTable :: Table JobsetrenameWriteColumns JobsetrenameReadColumns
jobsetrenameTable = Table "jobsetrenames" (pJobsetrename
  Jobsetrename
    { jobsetrenameProject = required "project"
    , jobsetrenameFrom = required "from_"
    , jobsetrenameTo = required "to_"
    }
  )

---- Types for table: jobsets ----

data Jobset' c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 =
  Jobset
    { jobsetName             :: c1
    , jobsetProject          :: c2
    , jobsetDescription      :: c3
    , jobsetNixexprinput     :: c4
    , jobsetNixexprpath      :: c5
    , jobsetErrormsg         :: c6
    , jobsetErrortime        :: c7
    , jobsetLastcheckedtime  :: c8
    , jobsetTriggertime      :: c9
    , jobsetEnabled          :: c10
    , jobsetEnableemail      :: c11
    , jobsetHidden           :: c12
    , jobsetEmailoverride    :: c13
    , jobsetKeepnr           :: c14
    , jobsetCheckinterval    :: c15
    , jobsetSchedulingshares :: c16
    , jobsetFetcherrormsg    :: c17
    }
  deriving(Generic)

type Jobset = Jobset' Text Text (Maybe Text) Text Text (Maybe Text) (Maybe Int32) (Maybe Int32) (Maybe Int32) Int32 Int32 Int32 Text Int32 Int32 Int32 (Maybe Text)

instance ToJSON Jobset where
instance ElmType Jobset where

type JobsetReadColumns = Jobset' (Column PGText) (Column PGText) (Column (Nullable PGText)) (Column PGText) (Column PGText) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column PGInt4) (Column PGInt4) (Column PGInt4) (Column PGText) (Column PGInt4) (Column PGInt4) (Column PGInt4) (Column (Nullable PGText))

type JobsetWriteColumns = Jobset' (Column PGText) (Column PGText) (Maybe (Column (Nullable PGText))) (Column PGText) (Column PGText) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Column PGInt4) (Column PGInt4) (Column PGInt4) (Column PGText) (Column PGInt4) (Column PGInt4) (Column PGInt4) (Maybe (Column (Nullable PGText)))

type JobsetNullableColumns = Jobset' (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText))

type JobsetMaybe = Jobset' (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Text)

sequenceJobset :: JobsetMaybe -> Maybe Jobset
sequenceJobset (Jobset c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17) = pure Jobset <*> c1 <*> c2 <*> pure c3 <*> c4 <*> c5 <*> pure c6 <*> pure c7 <*> pure c8 <*> pure c9 <*> c10 <*> c11 <*> c12 <*> c13 <*> c14 <*> c15 <*> c16 <*> pure c17

$(makeAdaptorAndInstance "pJobset" ''Jobset')

jobsetTable :: Table JobsetWriteColumns JobsetReadColumns
jobsetTable = Table "jobsets" (pJobset
  Jobset
    { jobsetName = required "name"
    , jobsetProject = required "project"
    , jobsetDescription = optional "description"
    , jobsetNixexprinput = required "nixexprinput"
    , jobsetNixexprpath = required "nixexprpath"
    , jobsetErrormsg = optional "errormsg"
    , jobsetErrortime = optional "errortime"
    , jobsetLastcheckedtime = optional "lastcheckedtime"
    , jobsetTriggertime = optional "triggertime"
    , jobsetEnabled = required "enabled"
    , jobsetEnableemail = required "enableemail"
    , jobsetHidden = required "hidden"
    , jobsetEmailoverride = required "emailoverride"
    , jobsetKeepnr = required "keepnr"
    , jobsetCheckinterval = required "checkinterval"
    , jobsetSchedulingshares = required "schedulingshares"
    , jobsetFetcherrormsg = optional "fetcherrormsg"
    }
  )

---- Types for table: newsitems ----

data Newsitem' c1 c2 c3 c4 =
  Newsitem
    { newsitemId         :: c1
    , newsitemContents   :: c2
    , newsitemCreatetime :: c3
    , newsitemAuthor     :: c4
    }

type Newsitem = Newsitem' Int32 Text Int32 Text

type NewsitemReadColumns = Newsitem' (Column PGInt4) (Column PGText) (Column PGInt4) (Column PGText)

type NewsitemWriteColumns = Newsitem' (Maybe (Column PGInt4)) (Column PGText) (Column PGInt4) (Column PGText)

type NewsitemNullableColumns = Newsitem' (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGText))

type NewsitemMaybe = Newsitem' (Maybe Int32) (Maybe Text) (Maybe Int32) (Maybe Text)

sequenceNewsitem :: NewsitemMaybe -> Maybe Newsitem
sequenceNewsitem (Newsitem c1 c2 c3 c4) = pure Newsitem <*> c1 <*> c2 <*> c3 <*> c4

$(makeAdaptorAndInstance "pNewsitem" ''Newsitem')

newsitemTable :: Table NewsitemWriteColumns NewsitemReadColumns
newsitemTable = Table "newsitems" (pNewsitem
  Newsitem
    { newsitemId = optional "id"
    , newsitemContents = required "contents"
    , newsitemCreatetime = required "createtime"
    , newsitemAuthor = required "author"
    }
  )

---- Types for table: nrbuilds ----

data Nrbuild' c1 c2 =
  Nrbuild
    { nrbuildWhat  :: c1
    , nrbuildCount :: c2
    }

type Nrbuild = Nrbuild' Text Int32

type NrbuildReadColumns = Nrbuild' (Column PGText) (Column PGInt4)

type NrbuildWriteColumns = Nrbuild' (Column PGText) (Column PGInt4)

type NrbuildNullableColumns = Nrbuild' (Column (Nullable PGText)) (Column (Nullable PGInt4))

type NrbuildMaybe = Nrbuild' (Maybe Text) (Maybe Int32)

sequenceNrbuild :: NrbuildMaybe -> Maybe Nrbuild
sequenceNrbuild (Nrbuild c1 c2) = pure Nrbuild <*> c1 <*> c2

$(makeAdaptorAndInstance "pNrbuild" ''Nrbuild')

nrbuildTable :: Table NrbuildWriteColumns NrbuildReadColumns
nrbuildTable = Table "nrbuilds" (pNrbuild
  Nrbuild
    { nrbuildWhat = required "what"
    , nrbuildCount = required "count"
    }
  )

---- Types for table: projectmembers ----

data Projectmember' c1 c2 =
  Projectmember
    { projectmemberProject  :: c1
    , projectmemberUsername :: c2
    }

type Projectmember = Projectmember' Text Text

type ProjectmemberReadColumns = Projectmember' (Column PGText) (Column PGText)

type ProjectmemberWriteColumns = Projectmember' (Column PGText) (Column PGText)

type ProjectmemberNullableColumns = Projectmember' (Column (Nullable PGText)) (Column (Nullable PGText))

type ProjectmemberMaybe = Projectmember' (Maybe Text) (Maybe Text)

sequenceProjectmember :: ProjectmemberMaybe -> Maybe Projectmember
sequenceProjectmember (Projectmember c1 c2) = pure Projectmember <*> c1 <*> c2

$(makeAdaptorAndInstance "pProjectmember" ''Projectmember')

projectmemberTable :: Table ProjectmemberWriteColumns ProjectmemberReadColumns
projectmemberTable = Table "projectmembers" (pProjectmember
  Projectmember
    { projectmemberProject = required "project"
    , projectmemberUsername = required "username"
    }
  )

---- Types for table: projects ----

data Project' c1 c2 c3 c4 c5 c6 c7 =
  Project
    { projectName        :: c1
    , projectDisplayname :: c2
    , projectDescription :: c3
    , projectEnabled     :: c4
    , projectHidden      :: c5
    , projectOwner       :: c6
    , projectHomepage    :: c7
    }
  deriving(Generic)

type Project = Project' Text Text (Maybe Text) Int32 Int32 Text (Maybe Text)

instance ToJSON Project where
instance ElmType Project where

type ProjectReadColumns = Project' (Column PGText) (Column PGText) (Column (Nullable PGText)) (Column PGInt4) (Column PGInt4) (Column PGText) (Column (Nullable PGText))

type ProjectWriteColumns = Project' (Column PGText) (Column PGText) (Maybe (Column (Nullable PGText))) (Column PGInt4) (Column PGInt4) (Column PGText) (Maybe (Column (Nullable PGText)))

type ProjectNullableColumns = Project' (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText))

type ProjectMaybe = Project' (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe Text)

sequenceProject :: ProjectMaybe -> Maybe Project
sequenceProject (Project c1 c2 c3 c4 c5 c6 c7) = pure Project <*> c1 <*> c2 <*> pure c3 <*> c4 <*> c5 <*> c6 <*> pure c7

$(makeAdaptorAndInstance "pProject" ''Project')

projectTable :: Table ProjectWriteColumns ProjectReadColumns
projectTable = Table "projects" (pProject
  Project
    { projectName = required "name"
    , projectDisplayname = required "displayname"
    , projectDescription = optional "description"
    , projectEnabled = required "enabled"
    , projectHidden = required "hidden"
    , projectOwner = required "owner"
    , projectHomepage = optional "homepage"
    }
  )

---- Types for table: releasemembers ----

data Releasemember' c1 c2 c3 c4 =
  Releasemember
    { releasememberProject     :: c1
    , releasememberRelease     :: c2
    , releasememberBuild       :: c3
    , releasememberDescription :: c4
    }

type Releasemember = Releasemember' Text Text Int32 (Maybe Text)

type ReleasememberReadColumns = Releasemember' (Column PGText) (Column PGText) (Column PGInt4) (Column (Nullable PGText))

type ReleasememberWriteColumns = Releasemember' (Column PGText) (Column PGText) (Column PGInt4) (Maybe (Column (Nullable PGText)))

type ReleasememberNullableColumns = Releasemember' (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGText))

type ReleasememberMaybe = Releasemember' (Maybe Text) (Maybe Text) (Maybe Int32) (Maybe Text)

sequenceReleasemember :: ReleasememberMaybe -> Maybe Releasemember
sequenceReleasemember (Releasemember c1 c2 c3 c4) = pure Releasemember <*> c1 <*> c2 <*> c3 <*> pure c4

$(makeAdaptorAndInstance "pReleasemember" ''Releasemember')

releasememberTable :: Table ReleasememberWriteColumns ReleasememberReadColumns
releasememberTable = Table "releasemembers" (pReleasemember
  Releasemember
    { releasememberProject = required "project"
    , releasememberRelease = required "release_"
    , releasememberBuild = required "build"
    , releasememberDescription = optional "description"
    }
  )

---- Types for table: releases ----

data Release' c1 c2 c3 c4 =
  Release
    { releaseProject     :: c1
    , releaseName        :: c2
    , releaseTimestamp   :: c3
    , releaseDescription :: c4
    }

type Release = Release' Text Text Int32 (Maybe Text)

type ReleaseReadColumns = Release' (Column PGText) (Column PGText) (Column PGInt4) (Column (Nullable PGText))

type ReleaseWriteColumns = Release' (Column PGText) (Column PGText) (Column PGInt4) (Maybe (Column (Nullable PGText)))

type ReleaseNullableColumns = Release' (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGText))

type ReleaseMaybe = Release' (Maybe Text) (Maybe Text) (Maybe Int32) (Maybe Text)

sequenceRelease :: ReleaseMaybe -> Maybe Release
sequenceRelease (Release c1 c2 c3 c4) = pure Release <*> c1 <*> c2 <*> c3 <*> pure c4

$(makeAdaptorAndInstance "pRelease" ''Release')

releaseTable :: Table ReleaseWriteColumns ReleaseReadColumns
releaseTable = Table "releases" (pRelease
  Release
    { releaseProject = required "project"
    , releaseName = required "name"
    , releaseTimestamp = required "timestamp"
    , releaseDescription = optional "description"
    }
  )

---- Types for table: schemaversion ----

data Schemaversion' c1 =
  Schemaversion
    { schemaversionVersion :: c1
    }

type Schemaversion = Schemaversion' Int32

type SchemaversionReadColumns = Schemaversion' (Column PGInt4)

type SchemaversionWriteColumns = Schemaversion' (Column PGInt4)

type SchemaversionNullableColumns = Schemaversion' (Column (Nullable PGInt4))

type SchemaversionMaybe = Schemaversion' (Maybe Int32)

sequenceSchemaversion :: SchemaversionMaybe -> Maybe Schemaversion
sequenceSchemaversion (Schemaversion c1) = pure Schemaversion <*> c1

$(makeAdaptorAndInstance "pSchemaversion" ''Schemaversion')

schemaversionTable :: Table SchemaversionWriteColumns SchemaversionReadColumns
schemaversionTable = Table "schemaversion" (pSchemaversion
  Schemaversion
    { schemaversionVersion = required "version"
    }
  )

---- Types for table: starredjobs ----

data Starredjob' c1 c2 c3 c4 =
  Starredjob
    { starredjobUsername :: c1
    , starredjobProject  :: c2
    , starredjobJobset   :: c3
    , starredjobJob      :: c4
    }

type Starredjob = Starredjob' Text Text Text Text

type StarredjobReadColumns = Starredjob' (Column PGText) (Column PGText) (Column PGText) (Column PGText)

type StarredjobWriteColumns = Starredjob' (Column PGText) (Column PGText) (Column PGText) (Column PGText)

type StarredjobNullableColumns = Starredjob' (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText))

type StarredjobMaybe = Starredjob' (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text)

sequenceStarredjob :: StarredjobMaybe -> Maybe Starredjob
sequenceStarredjob (Starredjob c1 c2 c3 c4) = pure Starredjob <*> c1 <*> c2 <*> c3 <*> c4

$(makeAdaptorAndInstance "pStarredjob" ''Starredjob')

starredjobTable :: Table StarredjobWriteColumns StarredjobReadColumns
starredjobTable = Table "starredjobs" (pStarredjob
  Starredjob
    { starredjobUsername = required "username"
    , starredjobProject = required "project"
    , starredjobJobset = required "jobset"
    , starredjobJob = required "job"
    }
  )

---- Types for table: systemstatus ----

data Systemstatus' c1 c2 =
  Systemstatus
    { systemstatusWhat   :: c1
    , systemstatusStatus :: c2
    }

type Systemstatus = Systemstatus' Text JSON.Value

type SystemstatusReadColumns = Systemstatus' (Column PGText) (Column PGJson)

type SystemstatusWriteColumns = Systemstatus' (Column PGText) (Column PGJson)

type SystemstatusNullableColumns = Systemstatus' (Column (Nullable PGText)) (Column (Nullable PGJson))

type SystemstatusMaybe = Systemstatus' (Maybe Text) (Maybe JSON.Value)

sequenceSystemstatus :: SystemstatusMaybe -> Maybe Systemstatus
sequenceSystemstatus (Systemstatus c1 c2) = pure Systemstatus <*> c1 <*> c2

$(makeAdaptorAndInstance "pSystemstatus" ''Systemstatus')

systemstatusTable :: Table SystemstatusWriteColumns SystemstatusReadColumns
systemstatusTable = Table "systemstatus" (pSystemstatus
  Systemstatus
    { systemstatusWhat = required "what"
    , systemstatusStatus = required "status"
    }
  )

---- Types for table: systemtypes ----

data Systemtype' c1 c2 =
  Systemtype
    { systemtypeSystem        :: c1
    , systemtypeMaxconcurrent :: c2
    }

type Systemtype = Systemtype' Text Int32

type SystemtypeReadColumns = Systemtype' (Column PGText) (Column PGInt4)

type SystemtypeWriteColumns = Systemtype' (Column PGText) (Column PGInt4)

type SystemtypeNullableColumns = Systemtype' (Column (Nullable PGText)) (Column (Nullable PGInt4))

type SystemtypeMaybe = Systemtype' (Maybe Text) (Maybe Int32)

sequenceSystemtype :: SystemtypeMaybe -> Maybe Systemtype
sequenceSystemtype (Systemtype c1 c2) = pure Systemtype <*> c1 <*> c2

$(makeAdaptorAndInstance "pSystemtype" ''Systemtype')

systemtypeTable :: Table SystemtypeWriteColumns SystemtypeReadColumns
systemtypeTable = Table "systemtypes" (pSystemtype
  Systemtype
    { systemtypeSystem = required "system"
    , systemtypeMaxconcurrent = required "maxconcurrent"
    }
  )

---- Types for table: urirevmapper ----

data Urirevmapper' c1 c2 =
  Urirevmapper
    { urirevmapperBaseuri :: c1
    , urirevmapperUri     :: c2
    }

type Urirevmapper = Urirevmapper' Text Text

type UrirevmapperReadColumns = Urirevmapper' (Column PGText) (Column PGText)

type UrirevmapperWriteColumns = Urirevmapper' (Column PGText) (Column PGText)

type UrirevmapperNullableColumns = Urirevmapper' (Column (Nullable PGText)) (Column (Nullable PGText))

type UrirevmapperMaybe = Urirevmapper' (Maybe Text) (Maybe Text)

sequenceUrirevmapper :: UrirevmapperMaybe -> Maybe Urirevmapper
sequenceUrirevmapper (Urirevmapper c1 c2) = pure Urirevmapper <*> c1 <*> c2

$(makeAdaptorAndInstance "pUrirevmapper" ''Urirevmapper')

urirevmapperTable :: Table UrirevmapperWriteColumns UrirevmapperReadColumns
urirevmapperTable = Table "urirevmapper" (pUrirevmapper
  Urirevmapper
    { urirevmapperBaseuri = required "baseuri"
    , urirevmapperUri = required "uri"
    }
  )

---- Types for table: userroles ----

data Userrole' c1 c2 =
  Userrole
    { userroleUsername :: c1
    , userroleRole     :: c2
    }

type Userrole = Userrole' Text Text

type UserroleReadColumns = Userrole' (Column PGText) (Column PGText)

type UserroleWriteColumns = Userrole' (Column PGText) (Column PGText)

type UserroleNullableColumns = Userrole' (Column (Nullable PGText)) (Column (Nullable PGText))

type UserroleMaybe = Userrole' (Maybe Text) (Maybe Text)

sequenceUserrole :: UserroleMaybe -> Maybe Userrole
sequenceUserrole (Userrole c1 c2) = pure Userrole <*> c1 <*> c2

$(makeAdaptorAndInstance "pUserrole" ''Userrole')

userroleTable :: Table UserroleWriteColumns UserroleReadColumns
userroleTable = Table "userroles" (pUserrole
  Userrole
    { userroleUsername = required "username"
    , userroleRole = required "role"
    }
  )

---- Types for table: users ----

data User' c1 c2 c3 c4 c5 c6 =
  User
    { userUsername     :: c1
    , userFullname     :: c2
    , userEmailaddress :: c3
    , userPassword     :: c4
    , userEmailonerror :: c5
    , userType         :: c6
    }

type User = User' Text (Maybe Text) Text Text Int32 Text

type UserReadColumns = User' (Column PGText) (Column (Nullable PGText)) (Column PGText) (Column PGText) (Column PGInt4) (Column PGText)

type UserWriteColumns = User' (Column PGText) (Maybe (Column (Nullable PGText))) (Column PGText) (Column PGText) (Column PGInt4) (Column PGText)

type UserNullableColumns = User' (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGText))

type UserMaybe = User' (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Int32) (Maybe Text)

sequenceUser :: UserMaybe -> Maybe User
sequenceUser (User c1 c2 c3 c4 c5 c6) = pure User <*> c1 <*> pure c2 <*> c3 <*> c4 <*> c5 <*> c6

$(makeAdaptorAndInstance "pUser" ''User')

userTable :: Table UserWriteColumns UserReadColumns
userTable = Table "users" (pUser
  User
    { userUsername = required "username"
    , userFullname = optional "fullname"
    , userEmailaddress = required "emailaddress"
    , userPassword = required "password"
    , userEmailonerror = required "emailonerror"
    , userType = required "type"
    }
  )

