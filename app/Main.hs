{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Data.List
import Data.Maybe

data Plan p r
  = End p
  | Leg p r (Plan p r)
  deriving stock (Show)

type ICAO p r = Plan p r

type ADEXP p = Plan p [p]

type Combined p r = Plan p (Via p r)

data Via p r = Via
  { route :: r,
    through :: [p]
  }
  deriving stock (Show)

projectICAO :: Combined p r -> ICAO p r
projectICAO = mapRoutes (.route)

projectADEXP :: Combined p r -> ADEXP p
projectADEXP = mapRoutes (.through)

mapRoutes :: (r -> r') -> Plan p r -> Plan p r'
mapRoutes _ (End p) = End p
mapRoutes f (Leg p r rest) = Leg p (f r) (mapRoutes f rest)

reconcile :: (Eq p) => ICAO p r -> [p] -> [Combined p r]
reconcile (End p) [p'] | p == p' = pure (End p)
reconcile (Leg p r rest) (p' : ps) | p == p' = do
  (through, restAdexp) <- splits ps
  recoRest <- reconcile rest restAdexp
  pure (Leg p Via {route = r, through} recoRest)
reconcile _ _ = []

-- This version ignores the duplicate waypoint issue. But still works!
reconcile' :: (Eq p) => ICAO p r -> [p] -> Maybe (Combined p r)
reconcile' (End p) [p'] | p == p' = pure (End p)
reconcile' (Leg p r rest) (p' : ps) | p == p' = do
  let (skipped, restAdexp) = span (/= start rest) ps
  recoRest <- reconcile' rest restAdexp
  pure (Leg p Via {route = r, through = skipped} recoRest)
reconcile' _ _ = Nothing

-- Extract the UK part of the flight.
ukSegment :: (p -> Bool) -> Combined p r -> Either Err (Combined p r)
ukSegment uk (End p)
  | nonUkPlan uk (End p) = Left NonUkPlan
  | otherwise = pure (End p)
ukSegment uk plan@(Leg _ _ rest) =
  if nonUkLeg uk plan
    then ukSegment uk rest
    else pure (flyUK uk plan)

-- Fly the UK part of the flight.
flyUK :: (p -> Bool) -> Combined p r -> Combined p r
flyUK _ (End end) = End end
flyUK uk (Leg p v rest)
  | nonUkPlan uk rest = Leg p v (afterUK rest)
  | otherwise = Leg p v (flyUK uk rest)

-- Skip the rest of the flight.
afterUK :: Combined p r -> Combined p r
afterUK plan = End (start plan)

-- The next leg of the journey doesn't fly through the UK.
nonUkLeg :: (p -> Bool) -> Combined p r -> Bool
nonUkLeg uk (End p) = not (uk p)
nonUkLeg uk (Leg p v _) = not (uk p) && not (any uk v.through)

-- The whole plan isn't in the UK.
nonUkPlan :: (a -> Bool) -> Combined a r -> Bool
nonUkPlan uk plan = all (nonUkLeg uk) (legs plan)

legs :: Plan p r -> [Plan p r]
legs (End p) = [End p]
legs plan@(Leg _ _ rest) = plan : legs rest

start :: Plan p r -> p
start (End p) = p
start (Leg p _ _) = p

ukPartOfICAO :: (Eq p) => (p -> Bool) -> ICAO p r -> [p] -> Either Err (ICAO p r)
ukPartOfICAO uk icao adexp = case reconcile icao adexp of
  [plan] -> projectICAO <$> ukSegment uk plan
  [] -> Left CannotReconcileIcaoAdexp
  _ -> Left AmbiguousReconciliationsOfIcaoAdexp

ukPartOfICAO' :: (Eq p) => (p -> Bool) -> ICAO p r -> [p] -> Either Err (ICAO p r)
ukPartOfICAO' uk icao adexp = case reconcile' icao adexp of
  Just plan -> projectICAO <$> ukSegment uk plan
  _ -> Left CannotReconcileIcaoAdexp

data Err
  = NonUkPlan
  | CannotReconcileIcaoAdexp
  | AmbiguousReconciliationsOfIcaoAdexp
  deriving stock (Show)

-- All the ways to snap a list in two. That is, 'split xs' returns all pairs
-- '(ys, zs)' where xs == ys ++ zs.
splits :: [a] -> [([a], [a])]
splits xs = zip (inits xs) (tails xs)

-- Example:

infixr 6 ~>

(~>) :: (p, r) -> Plan p r -> Plan p r
(~>) (p, r) = Leg p r

{-
           4       2        8         5              1           9
ICAO:  F------Q--------T--------O-----------P---------------Y--------U

ADEXP: F   S  Q    C   T   A    O  E  X     P   W   B   Q   Y        U
                       UK  UK   UK UK UK    UK  UK
-}
example :: Either Err (ICAO String Integer)
example = ukPartOfICAO' inUK icao adexp
  where
    inUK = (`elem` ["T", "A", "O", "E", "X", "P", "W"])
    icao = ("F", 4) ~> ("Q", 2) ~> ("T", 8) ~> ("O", 5) ~> ("P", 1) ~> ("Y", 9) ~> End "U"
    adexp = ["F", "S", "Q", "C", "T", "A", "O", "E", "X", "P", "W", "B", "Q", "Y", "U"]

main :: IO ()
main = print example
