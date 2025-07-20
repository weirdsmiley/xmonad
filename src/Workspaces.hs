module Workspaces
  ( myWorkspaces
  ) where

import XMonad

myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1 .. 10]
