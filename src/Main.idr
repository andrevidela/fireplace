module Main

import System.Clock
import Pack.Runner
import Pack.Runner.Install
import Pack.Runner.Develop
import Pack.Config.Types
import Pack.Config.Environment
import Pack.CmdLn.Types
import Pack.Database.Types
import Pack.Core
import Pack.Core.IO

import Data.IORef
import Data.Vect
import Data.String
import Data.List.Quantifiers

PIO : Type -> Type
PIO = EitherT PackErr IO

Show PkgName where
  show = value
||| Flag to check if we care about runtime startup or not
data RTOptions
  = WithStartup
  | WithoutStartup

||| Compile stages we are measuring
data CTOptions = AllStages | OnlyParsing | OnlyTypecheck

||| Flag to benchmarking compilation of packages or runtime of packages
data BenchmarkMode
  = RuntimeBenchmarks RTOptions
  | CompileBenchmarks CTOptions
  | Both RTOptions CTOptions

resultCount : BenchmarkMode -> Nat
resultCount (RuntimeBenchmarks x) = 1
resultCount (CompileBenchmarks x) = 1
resultCount (Both x y) = 2


record BenchmarkArgs where
  constructor MkArgs
  fileName : Maybe String
  idrisCommit : String
  packCollection : String
  mode : BenchmarkMode

data ITable : (keys : List key) -> (n : Nat) -> (a : Type) -> Type where
  Nil : ITable [] n a
  (::) : {k : key} -> (content : Vect n a) -> ITable ks n a -> ITable (k :: ks) n a

asTable : {0 keys : List key} -> ITable keys n a -> List (key, Vect n a)
asTable [] = []
asTable ((::) {k} content x) = (k, content) :: asTable x

concat : {keys : _} -> ITable keys n a -> ITable keys m a -> ITable keys (n + m) a
concat {keys = []} [] [] = []
concat {keys = (z :: xs)} (c :: cs) (d :: ds) = (c ++ d) :: concat cs ds


namespace Benchmark
  public export
  record Info where
    constructor MkInfo
    compilerCommit : String

  public export
  record Results (pkgs : List PkgName) (n : Nat) where
    constructor MkResults
    results : ITable pkgs n (Clock Duration)


toCSV : Show key => Show a => List (key, Vect n a) -> String
toCSV t = let asLists = map (\(x, y) => show x :: map show (toList y)) t
              rows = transpose asLists
          in unlines (map (joinBy ", ") rows)

writeCSV : Show key => Show val => {0 keys : List key} -> ITable keys n val -> IO ()
writeCSV = putStrLn . toCSV . asTable

time : PIO a -> PIO (a, Clock Duration)
time io = do
  start <- liftIO $ clockTime Process
  v <- io
  end <- liftIO $ clockTime Process
  let diff = end `timeDifference` start
  pure (v, diff)


obtainPackages : IdrisEnv -> (List PkgName)
obtainPackages x = keys x.env.db.packages

runTests : {auto env : IdrisEnv} -> (pkgs : List PkgName) -> PIO (Results pkgs 1)
runTests [] = pure (MkResults [])
runTests (package :: rest) = do
  val <- time $ runTest package [] env
  rec <- runTests rest
  pure (MkResults ([snd val] :: results rec))

runTypecheck : {auto env : IdrisEnv} -> (pkgs : List PkgName) -> PIO (Results pkgs 1)
runTypecheck [] = pure (MkResults [])
runTypecheck (package :: rest) = do
  val <- time $ typecheck (Pkg package) env
  rec <- runTypecheck rest
  pure (MkResults ([snd val] :: results rec))

runMain : {auto env : IdrisEnv} -> String -> (pkgs : List PkgName) -> (m : BenchmarkMode) -> PIO (Results pkgs (resultCount m))
runMain file pkgs (RuntimeBenchmarks x) = runTests pkgs
runMain file pkgs (CompileBenchmarks x) = runTypecheck pkgs
runMain file pkgs (Both x y) = do
  typecheckingResults <- runTypecheck pkgs
  runtimeResults <- runTests pkgs
  pure $ MkResults $ concat (results typecheckingResults) (results runtimeResults)

getFileName : Maybe String -> IO String
getFileName Nothing = map (showTime 2 0) $ clockTime UTC
getFileName (Just x) = pure x

readOptions : IO BenchmarkArgs
readOptions = pure (MkArgs Nothing "388d21775705f1b9d045081b29129bf0ebf937aa" "latest" (Both WithStartup AllStages))

withIdrisEnv :
     {auto _ : HasIO io}
  -> (CurDir -> MetaConfig)
  -> (IdrisEnv -> EitherT PackErr io a)
  -> EitherT PackErr io a
withIdrisEnv mc f = do
  pd    <- getPackDir
  withTmpDir $ do
    cd      <- CD <$> curDir
    cache   <- emptyCache
    linebuf <- getLineBufferingCmd
    idrisEnv (mc cd) True >>= f

getConfig : String -> CurDir -> MetaConfig
getConfig collection x = { logLevel := Debug } (init (MkDBName "nightly-230831"))


main : IO ()
main = readOptions >>= \opts => run $ withIdrisEnv (getConfig opts.packCollection) $ \env => do
  let packages = obtainPackages env
  resultsFileName <- liftIO $ getFileName opts.fileName
  (MkResults resultTable) <- runMain resultsFileName packages opts.mode
  liftIO $ writeCSV {key = PkgName} resultTable
  pure ()
