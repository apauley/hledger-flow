{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Incremental.Platform (tests) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Data.Time (addUTCTime)
import Hledger.Flow.Import.Fingerprint
import Hledger.Flow.Import.PrivateDirectory
import Path
import qualified Path.IO as PathIO
import qualified System.Directory as Dir
import System.IO.Error (tryIOError)
import Test.HUnit hiding (Path)
import TestHelpers (withTestDir)
#ifdef mingw32_HOST_OS
import System.Process (callProcess)
import System.Environment (setEnv, unsetEnv)
import Control.Exception (bracket_)
#else
import Control.Exception (bracket)
import Data.Bits ((.&.))
import qualified System.Posix.Files as Posix
#endif
#ifdef darwin_HOST_OS
import Data.List (isInfixOf)
import qualified System.Posix.Directory as Posix
import System.Process (callProcess, readProcess)
#endif

tests :: Test
tests = TestLabel "Incremental platform primitives" $ TestList
  [ TestLabel "SHA-256 over exact file bytes" $ TestCase $ withTestDir $ \root -> do
      let file = root </> [relfile|input|]
      forM_ [(0, "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"),
             (1000000, "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0")] $
        \(len, expected) -> do
          BS.writeFile (toFilePath file) (BS.replicate len 97)
          assertEqual "in-memory digest" expected (digestHex (digestBytes (BS.replicate len 97)))
          digest <- digestFile file
          assertEqual "fixed SHA-256 vector" expected (digestHex digest)
          let copy = root </> [relfile|copied|]
          writeFile (toFilePath copy) "old contents"
          copied <- copyDigest file copy
          assertEqual "hash of copied bytes" digest copied
          bytes <- BS.readFile (toFilePath copy)
          assertEqual "copied all bytes" (BS.replicate len 97) bytes
          observation <- observeFile file
          stamp <- statFile file
          assertEqual "same file metadata" stamp (fileStamp observation)
          assertEqual "observed bytes" digest (observationDigest observation)
          assertEqual "observed size" (fromIntegral len) (stampSize (fileStamp observation))
  , TestLabel "same-byte rewrite changes transient observation" $ TestCase $ withTestDir $ \root -> do
      let file = root </> [relfile|input|]
          name = toFilePath file
      writeFile name "same bytes"
      first <- observeFile file
      originalTime <- Dir.getModificationTime name
      writeFile name "same bytes"
      Dir.setModificationTime name (addUTCTime 10 originalTime)
      second <- observeFile file
      assertBool "observed rewrite" (first /= second)
      assertEqual "unchanged content key" (observationDigest first) (observationDigest second)
  , TestLabel "file identity distinguishes copies" $ TestCase $ withTestDir $ \root -> do
      let file = root </> [relfile|input|]
          copy = root </> [relfile|copy|]
      writeFile (toFilePath file) "shared bytes"
      Dir.copyFile (toFilePath file) (toFilePath copy)
      a <- observeFile file
      b <- observeFile copy
      assertBool "different files" (stampIdentity (fileStamp a) /= stampIdentity (fileStamp b))
  , TestLabel "private staging under a shared directory" $ TestCase $ withTestDir $ \root -> do
      let run = root </> [reldir|run|]
#ifdef mingw32_HOST_OS
      withAclPaths root run $ do
        powershell $ unlines
          [ "$acl = [IO.Directory]::GetAccessControl($env:HF_ACL_PARENT)"
          , "$everyone = [System.Security.Principal.SecurityIdentifier]'S-1-1-0'"
          , "$acl.AddAccessRule([System.Security.AccessControl.FileSystemAccessRule]::new($everyone,'ReadAndExecute','ContainerInherit,ObjectInherit','None','Allow'))"
          , "[IO.Directory]::SetAccessControl($env:HF_ACL_PARENT, $acl)"
          ]
        createPrivateDirectory run
        let nested = run </> [reldir|nested|]
            copied = nested </> [relfile|statement|]
            source = root </> [relfile|source|]
        PathIO.createDir nested
        writeFile (toFilePath source) "private"
        _ <- copyDigest source copied
        powershell $ unlines
          [ "$sid = [System.Security.Principal.WindowsIdentity]::GetCurrent().User.Value"
          , "$dir = [IO.Directory]::GetAccessControl($env:HF_ACL_RUN)"
          , "if (-not $dir.AreAccessRulesProtected) { throw 'inheriting cache parent DACL' }"
          , "$file = [IO.File]::GetAccessControl([IO.Path]::Combine($env:HF_ACL_RUN, 'nested/statement'))"
          , "foreach ($acl in @($dir, $file)) {"
          , "  $rules = $acl.GetAccessRules($true,$true,[System.Security.Principal.SecurityIdentifier])"
          , "  if ($rules.Count -ne 1 -or $rules[0].IdentityReference.Value -ne $sid -or $rules[0].AccessControlType -ne 'Allow' -or $rules[0].FileSystemRights -ne 'FullControl') { throw 'expected only current user with full access' }"
          , "  if ($acl -ne $dir -and -not $rules[0].IsInherited) { throw 'file lacks inherited access' }"
          , "}"
          ]
#else
      Posix.setFileMode (toFilePath root) 0o777
      createPrivateDirectory run
      mode <- Posix.fileMode <$> Posix.getFileStatus (toFilePath run)
      assertEqual "owner-only directory" 0o700 (mode .&. 0o777)
#endif
#ifndef mingw32_HOST_OS
  , TestLabel "unusable creation mode rejects the empty directory" $ TestCase $ withTestDir $ \root -> do
      let run = root </> [reldir|run|]
      result <- bracket (Posix.setFileCreationMask 0o177) Posix.setFileCreationMask $
        \_ -> tryIOError (createPrivateDirectory run)
      assertRejectedDirectory run result
#endif
#ifdef darwin_HOST_OS
  , TestLabel "inherited macOS ACL rejects staging" $ TestCase $ withTestDir $ \root -> do
      let probe = root </> [reldir|probe|]
          run = root </> [reldir|run|]
      callProcess "/bin/chmod" ["+a", "everyone allow list,search,directory_inherit", toFilePath root]
      Posix.createDirectory (toFilePath probe) 0o700
      acl <- readProcess "/bin/ls" ["-lde", toFilePath probe] ""
      assertBool "the filesystem inherited the test grant" ("inherited allow" `isInfixOf` acl)
      result <- tryIOError (createPrivateDirectory run)
      assertRejectedDirectory run result
#endif
  , TestLabel "occupied path stops staging before copying" $ TestCase $ withTestDir $ \root -> do
      let run = root </> [reldir|occupied|]
      PathIO.createDir run
      result <- tryIOError $ do
        createPrivateDirectory run
        writeFile (toFilePath (run </> [relfile|statement|])) "private"
      case result of
        Left _ -> return ()
        Right () -> assertFailure "used a pre-existing directory"
      entries <- Dir.listDirectory (toFilePath run)
      assertEqual "no copies made" [] entries
  ]

#ifndef mingw32_HOST_OS
assertRejectedDirectory :: Path Abs Dir -> Either IOError () -> Assertion
assertRejectedDirectory run result = do
  case result of
    Left _ -> pure ()
    Right () -> assertFailure "accepted unverified staging permissions"
  exists <- PathIO.doesDirExist run
  assertBool "removed the rejected empty directory" (not exists)
#endif

#ifdef mingw32_HOST_OS
withAclPaths :: Path Abs Dir -> Path Abs Dir -> IO a -> IO a
withAclPaths root run =
  bracket_ (setEnv "HF_ACL_PARENT" (toFilePath root) >> setEnv "HF_ACL_RUN" (toFilePath run))
           (unsetEnv "HF_ACL_PARENT" >> unsetEnv "HF_ACL_RUN")

powershell :: String -> IO ()
powershell command = callProcess "powershell.exe" ["-NoProfile", "-NonInteractive", "-Command", "$ErrorActionPreference = 'Stop'; " ++ command]
#endif
