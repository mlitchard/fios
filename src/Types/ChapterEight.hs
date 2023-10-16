{-# LANGUAGE RoleAnnotations     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Types.ChapterEight where 

import Data.Coerce (Coercible(..), coerce) 
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Monoid (Sum (..), Product (..))


