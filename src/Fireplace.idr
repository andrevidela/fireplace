module Fireplace

import Data.IORef
import Data.SortedMap

import System
import System.Clock

import Pack.Core
import Pack.Runner.Install
import Pack.Runner.Database
import Pack.Config
import Pack.Database.Types

record BenchmarkResult where
  constructor MkResult
  start : Clock Monotonic
  end   : Clock Monotonic

(.interval) : BenchmarkResult -> Clock Duration
(.interval) r = r.end `timeDifference` r.start

-- ouputs the compiler environement once it's been entirely bootstrapped from a given commit
bootstrapCompiler : HasIO io => (commit : String) -> EitherT PackErr io IdrisEnv
bootstrapCompiler commit = do
  packDir <- getPackDir
  tmpDir <- mkTmpDir
  libCache <- newIORef (the (SortedMap PkgName (ResolvedLib U)) empty)
  lineBuffer <- getLineBufferingCmd
  cd <- CD <$> curDir
  latestCollection <- defaultColl
  e <- env { pd = packDir
           , td  = tmpDir
           , ch  = libCache
           , lbf = lineBuffer}
           (init {cur = cd, coll = latestCollection}) True

  mkIdris

export covering
libPkgTiming :  HasIO io
       => (e : IdrisEnv)
       => (env        : List (String,String))
       -> (logLevel   : LogLevel)
       -> (cleanBuild : Bool)
       -> (cmd        : CmdArgList)
       -> (desc       : Desc Safe)
       -> EitherT PackErr io BenchmarkResult
libPkgTiming env lvl cleanBuild cmd desc =
  let exe := idrisWithCG
      s   := exe ++ cmd ++ [desc.path.file]
   in do
     pre <- (env ++) <$> buildEnv
     debug "About to run: \{escapeCmd s}"
     when cleanBuild (checkBuildDir desc)

     inDir (desc.path.parent) (\_ => do
           start <- liftIO $ clockTime Monotonic
           val <- sysWithEnvAndLog lvl s pre
           end <- liftIO $ clockTime Monotonic
           pure (MkResult start end)
           )

-- `library` is the library to compile and measure the time it takes to compile
timeCompiling : HasIO io => (library : String) -> (env : IdrisEnv) -> io BenchmarkResult
timeCompiling library env = ?timeCompiling_rhs

timeRunningTests : HasIO io => (library : String) -> (env : IdrisEnv) -> io BenchmarkResult
timeRunningTests library env = ?timeRunning

partial
main : IO ()
main = do Right res <- the (IO (Either PackErr Unit)) (runEitherT (do
                           compilerEnv <- (bootstrapCompiler "lol") 
            s <- safe ?descT
            result <- (libPkgTiming [] Build True [] ?desc)
            pure ()
            ))
            | Left err => putStrLn "error"
          putStrLn "finished"
