
-- | Definition copied from TypeCompose-0.9.14: https://hackage.haskell.org/package/TypeCompose-0.9.14
module GLL.Types.TypeCompose where

import Prelude hiding ((.),id)

import Control.Arrow (Arrow(..))
import Control.Category (Category(..))
import Control.Applicative (liftA, liftA2)

-- | Composition of type constructors: unary with binary.  Called
-- "StaticArrow" in [1].
newtype OO f j a b = OO { unOO :: f (a `j` b) }

instance (Applicative f, Category cat) => Category (OO f cat) where
  id          = OO (pure id)
  OO g . OO h = OO (liftA2 (.) g h)

instance (Applicative f, Arrow arr) => Arrow (OO f arr) where
  arr           = OO . pure . arr
  first (OO g)  = OO (liftA first g)
