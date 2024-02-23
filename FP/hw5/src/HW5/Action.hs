{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
module HW5.Action(
  HiPermission (..),
  PermissionException (..),
  HIO (..)
) where
import Control.Exception
import Data.Set
import Control.Monad.Trans.Reader
import System.Directory
import Data.Text as T
import Data.ByteString as B
import Data.Sequence as S
import Data.Text.Encoding
import Data.Time.Clock
import System.Random

import HW5.Base

data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Ord, Eq)

data PermissionException =
  PermissionRequired HiPermission deriving (Show, Ord, Eq)

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a } deriving (Functor, Applicative, Monad) via (ReaderT (Set HiPermission) IO)

instance HiMonad HIO where
  runAction :: HiAction -> HIO HiValue
  runAction HiActionCwd = HIO (\p -> do
      if member AllowRead p then do
        currDir <- getCurrentDirectory
        return (HiValueString (T.pack currDir))
      else throwIO (PermissionRequired AllowRead))

  runAction (HiActionMkDir f) = HIO (\p -> do
      if member AllowWrite p then do
        createDirectory f
        return HiValueNull
      else throwIO (PermissionRequired AllowWrite))

  runAction (HiActionChDir f) = HIO (\p -> do
      if member AllowRead p then do
        setCurrentDirectory f
        return HiValueNull
      else throwIO (PermissionRequired AllowRead))

  runAction (HiActionRead f) = HIO (\p -> do
      if member AllowRead p then do
        file <- doesFileExist f
        if file then do
          bytes <- B.readFile f
          let decodedBytes = either (const HiValueNull) HiValueString (decodeUtf8' bytes)
          if decodedBytes == HiValueNull then return (HiValueBytes bytes) else return decodedBytes
        else do
          l <- listDirectory f
          return (HiValueList (S.fromList (HiValueString . T.pack <$> l)))
      else throwIO (PermissionRequired AllowRead))
    
  runAction (HiActionWrite f bs) = HIO (\p -> do
      if member AllowWrite p then do
        B.writeFile f bs
        return HiValueNull
      else throwIO (PermissionRequired AllowWrite))

  runAction (HiActionEcho e) = HIO (\p -> do
      if member AllowWrite p then do
        print e
        return HiValueNull
      else throwIO (PermissionRequired AllowWrite))    

  runAction HiActionNow = HIO (\p -> do
      if member AllowTime p then do
        time <- getCurrentTime
        return (HiValueTime time)
      else throwIO (PermissionRequired AllowTime))
  
  runAction (HiActionRand l r) = HIO (\_ -> do
        rand <- getStdRandom (uniformR (l, r))
        return (HiValueNumber (toRational rand)))
