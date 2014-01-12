{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Switcher where

-- The associativity order does not matter,
-- emphasizing it by omitting parantheses.
infixr 5 :&
data (a :& b) = a :& b

data Switcher a = Switcher {
        switch :: a -> Int, -- returns index of a branch
        range  :: Int
    }

-- Switcher composition. Mnemonics for the symbol chosen
-- is that it represents a layer in a resulting context
-- switcher's tree.
infixr 5 <|< -- for the composition operator to be agreed with ':&'
(<|<) :: Switcher a -> Switcher b -> Switcher (a :& b)
(Switcher f r1) <|< (Switcher g r2) = Switcher fg $ r1*r2
    where fg (a :& b) = f a * r2 + g b

nilSwitcher :: Switcher a
nilSwitcher =
    Switcher {
        switch = const 0,
        range = 1 }

-- Switchers are monoids with (<|<) as (<>) and 'nilSwitcher'
-- as zero element in a sence that composition of two switchers
-- gives a switcher for which all monoid laws holds:
--     a) identity: s <|< nil === nil <|< s === s
--     b) associativity: (f <|< g) <|< h === f <|< (g <|< h)
-- Unfortunately, for a switcher to be an instance of Haskell's
-- monoid class, 'mconcat' operation should preserve the type
-- of its operands, which is not the case for switchers.
-- Can that consept of 'almost-monoid', or 'parametrized-monoid'
-- be expressed in Haskell? Let's invent a typeclass for now;

-- where is a 'canonical' class for such structures?
class ParaMonoid p where
    parazero :: p a
    paraconcat :: p a -> p b -> p (a :& b)
    -- XXX: the nesting of types shall not be of importance.

    -- Just an alias. '<>' is for monoid, '<|>' is for alternative
    (<||>) :: p a -> p b -> p (a :& b)
    a <||> b = a `paraconcat` b -- monomorpism restriction, lol

    -- LAWS: (similar to monoidal's)
    -- a) Id: a <||> parazero == parazero <||> a == a
    -- b) Assoc: (a <||> b) <||> c == a <||> (b <||> c)

instance ParaMonoid Switcher where
    parazero = nilSwitcher
    paraconcat = (<|<)

-- some useful switchers
enumSwitcher :: forall a. (Enum a, Bounded a) => Switcher a
enumSwitcher = Switcher fromEnum range
    where range = length [(minBound :: a) ..]

-- XXX: Range is INCLUSIVE.
-- TODO: What is a meaningful generalization of that?
-- Let it be non-generalized for now.
rangeSwitcher :: (Int, Int) -> Switcher Int
rangeSwitcher (a, b) = Switcher cropper r
    where r = b - a + 1
          cropper s | s <= a = a
                    | s >= b = b
                    | otherwise = s
