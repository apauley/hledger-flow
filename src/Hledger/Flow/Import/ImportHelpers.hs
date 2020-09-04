{-# LANGUAGE QuasiQuotes #-}

module Hledger.Flow.Import.ImportHelpers (pathFindFiles) where

import Path
import Data.Char (isDigit)

import Hledger.Flow.PathHelpers (AbsDir, AbsFile, findFilesIn)

pathFindFiles :: AbsDir -> IO [AbsFile]
pathFindFiles = do
  let excludeDirs = [[Path.reldir|2-preprocessed|], [Path.reldir|3-journal|], [Path.reldir|_manual_|], [Path.reldir|__pycache__|]]
  findFilesIn includePred excludeDirs
    -- Include only files directly underneath 1-in/yearDir, e.g. 1-in/2020/*
    where includePred d = (dirname . parent) d == [Path.reldir|1-in|]
                            && length shortDirName == 4
                            && all isDigit shortDirName
            where shortDirName = (init . Path.toFilePath . Path.dirname) d
