module HauntedHouse.Game.Model.Interface where 
import Text.Show (Show(show))
import HauntedHouse.Game.Model.GID (GID)

newtype Exit destination = 
          Exit { _toDestination' :: GID destination} deriving stock Show

newtype Interface action 
  = ContainerInterface' (ContainerInterface action action action action)

data ContainerInterface toClose toOpen unlock lock
  = Open toClose 
  | Closed (ClosedContainer toOpen unlock lock)   

data ClosedContainer toOpen unlock lock = ClosedContainer {
  _toOpen :: toOpen
, _lockability :: Maybe (LockState unlock lock)
}

data LockState unlock lock = Locked unlock | UnLocked lock

data Lockability unlock lock 
  = Lockable (LockState unlock lock) 
  | UnLockable
    
instance Show (LockState unlock lock) where 
  show (Locked _) = "Locked"
  show (UnLocked _) = "Unlocked"

newtype Portal location = Portal { _portalExit' :: GID (Exit location)}