module Fireplace

import Data.IORef
import Data.SortedMap

import System
import System.Clock

import Pack.Core
import Pack.Runner.Install
import Pack.Runner.Database
import Pack.Runner.Query
import Pack.Config
import Pack.Database.Types
import Pack.CmdLn.Opts

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
           ({safetyPrompt := False} (init {cur = cd, coll = latestCollection}) ) True

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
     liftIO (putStrLn "timing soon")
     pre <- (env ++) <$> buildEnv
     debug "About to run: \{escapeCmd s}"
     when cleanBuild (checkBuildDir desc)

     inDir (desc.path.parent) (\_ => do
           liftIO (putStrLn "starting the timing")
           start <- liftIO $ clockTime Monotonic
           val <- sysWithEnv s pre
           end <- liftIO $ clockTime Monotonic
           liftIO (putStrLn "timer end")
           pure (MkResult start end)
           )

-- `library` is the library to compile and measure the time it takes to compile
timeCompiling : HasIO io => (library : String) -> (env : IdrisEnv) -> io BenchmarkResult
timeCompiling library env = ?timeCompiling_rhs

timeRunningTests : HasIO io => (library : String) -> (env : IdrisEnv) -> io BenchmarkResult
timeRunningTests library env = ?timeRunning

partial
main : IO ()
main = putStrLn "starting" *> the (IO (Either PackErr BenchmarkResult)) (runEitherT (do
            liftIO (putStrLn "bootstrapping the compiler")
            compilerEnv <- (bootstrapCompiler "lol")
            allPackages <- resolveAll
            -- get the list of all packages so that we can benchmark them all
            -- putStrLn (unlines $ map (show . nameStr) allPackages)
            let libs@(head :: tail) = map name allPackages
            liftIO (putStrLn "getting the library")
            lib <- resolveLib head >>= safe . desc
            liftIO (putStrLn "installing everything")
            installDeps lib
            liftIO (putStrLn "timing the library")
            libPkgTiming [] Build True ["--typecheck"] lib
            ))
        >>= (\case (Left err) => putStrLn "error"
                   (Right time) => putStrLn "finished in: \{show time.interval}")
