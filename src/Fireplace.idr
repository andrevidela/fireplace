module Fireplace

import System

import Pack.Core
import Pack.Runner.Install
import Pack.Config

record BenchmarkResult where
  constructor MkResult
  start : Int
  end   : Int

(.interval) : BenchmarkResult -> Int
(.interval) r = r.end - r.start

-- ouputs the filepath of the compiler once it's been entirely bootstrapped from a given commit
bootstrapCompiler : HasIO io => (commit : String) -> EitherT PackErr io IdrisEnv
bootstrapCompiler commit = do
  e <- env { pd = ?packDir
           , td  = ?tmpDir
           , ch  = ?libCache
           , lbf = ?lineBuf}
           (init {cur = ?curDif, coll = ?collDir}) True

  mkIdris

-- `library` is the library to compile and measure the time it takes to compile
timeCompiling : HasIO io => (library : String) -> (env : IdrisEnv) -> io BenchmarkResult
timeCompiling library env = ?timeCompiling_rhs

timeRunningTests : HasIO io => (library : String) -> (env : IdrisEnv) -> io BenchmarkResult
timeRunningTests library env = ?timeRunning

partial
main : IO ()
main = do [_, compilerCommit] <- getArgs
          pure ()
