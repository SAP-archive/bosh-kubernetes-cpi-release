{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module CPI.Kubernetes.Resource.Stub.PersistentVolumeClaim() where

import           Kubernetes.Model.V1.DeleteOptions             (mkDeleteOptions)
import           Kubernetes.Model.V1.ObjectMeta                (ObjectMeta)
import qualified Kubernetes.Model.V1.ObjectMeta                as ObjectMeta

import           Data.HashMap.Strict                           (HashMap)
import qualified Data.HashMap.Strict                           as HashMap
import           Data.HashSet                                  (HashSet)
import qualified Data.HashSet                                  as HashSet
import           Data.Hourglass
import           Data.Maybe
import           Data.Monoid
import           Data.Text                                     (Text)
import qualified Data.Text                                     as Text

import           Control.Exception.Safe
import           Control.Lens
import           Network.HTTP.Types.Status
import           Servant.Client
import           System.Random

import           CPI.Kubernetes.Resource.Metadata
import           CPI.Kubernetes.Resource.PersistentVolumeClaim

import           Control.Monad.Stub.Console
import           Control.Monad.Stub.StubMonad
import           Control.Monad.Stub.Time
import           Control.Monad.Stub.Wait
import           CPI.Kubernetes.Resource.Stub.State            (HasPVCs (..))

import           Control.Monad
import qualified Control.Monad.State                           as State
import           Control.Monad.Time
import           Control.Monad.Wait


import           Control.Monad.IO.Class
import qualified GHC.Int                                       as GHC

checkName :: (MonadIO m, State.MonadState a m, MonadThrow m, HasPVCs a, HasMetadata n) => Text -> n -> m n
checkName namespace named =
  if isJust $ named ^. maybeName
    then do
      pvcs <- State.gets asPVCs
      if isJust $ HashMap.lookup (namespace, named ^. name) pvcs
        then throwM FailureResponse {
          responseStatus = Status {
              statusCode = 409
          }
        }
        else pure named
    else if isJust $ named ^. maybeGenerateName
      then do
        gen <- liftIO newStdGen
        let chars = take 5 $ randomRs ('a','z') gen
        pure $ named & name .~ (named ^. generateName <> (Text.pack chars))
      else
        throwM FailureResponse {
          responseStatus = Status {
              statusCode = 400
          }
        }

initialStatus newStatus pvc = pvc & status.phase .~ Just newStatus

instance (MonadIO m, MonadThrow m, Monoid w, HasPVCs s, HasWaitCount w, HasTime s, HasTimeline s) => MonadPVC (StubT r s w m) where

  createPersistentVolumeClaim namespace pvc = do
    pvc' <- pvc
             & (initialStatus "Pending")
             & (checkName namespace)
    let pvcName = pvc' ^. name

    State.modify $ withPVCs $ HashMap.insert (namespace, pvcName) pvc'
    timestamp <- currentTime
    State.modify $ withTimeline
                 (\events ->
                   let runningConditions = All True
                     in
                       if getAll runningConditions
                         then
                           HashMap.insert (timestamp + (Elapsed $ Seconds 1))
                           [withPVCs $ HashMap.adjust (status.phase ?~ "Bound") (namespace, pvcName)]
                           events
                         else
                           events)
    pure pvc'

  listPersistentVolumeClaim namespace = do
    kube <- State.get
    pure undefined

  getPersistentVolumeClaim namespace name = do
    pods <- State.gets asPVCs
    pure $ HashMap.lookup (namespace, name) pods

  updatePersistentVolumeClaim namespace pod = do
    pods <- State.gets asPVCs
    State.put undefined
    pure undefined

  deletePersistentVolumeClaim namespace name = do
    timestamp <- currentTime
    State.modify $ withTimeline
                 (\events -> --events)
                   let
                     terminating :: [s -> s]
                     terminating = [withPVCs $ HashMap.adjust (\pod -> pod & status.phase ?~ "Terminating") (namespace, name)]
                     deleted :: [s -> s]
                     deleted = [withPVCs $ HashMap.delete (namespace, name)]
                     after :: GHC.Int64 -> Elapsed
                     after n = timestamp + (Elapsed $ Seconds n)
                     in
                       HashMap.insert (after 2) deleted (HashMap.insert (after 1) terminating events)
                        )

    -- pods <- State.gets $ withPVCs id
    pure undefined

  waitForPersistentVolumeClaim namespace name predicate = waitFor (WaitConfig (Retry 20) (Seconds 1)) (getPersistentVolumeClaim namespace name) predicate
