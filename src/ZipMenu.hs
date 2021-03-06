module ZipMenu
( ZipMenu
, Menu (..)
, contentOf
, fromMenu
, toMenu
--
, up
, upmost
, down
, left
, leftMost
, right
, rightMost
, modify
, zmap
, downTo
--
, isTop
--
, cursor
, path
, siblings
--
, MenuStatus (..)
, WithStatus
, toMenuBy
, toMenuWithInfo
--
, drawMenu
) where

import Data.List (splitAt, findIndex)

data Menu a = Item a | Sub a [Menu a] deriving (Eq, Show)

data Crumb a = Crumb a [Menu a] [Menu a] deriving (Eq, Show)

type ZipMenu a = (Menu a, [Crumb a])

--

instance Functor Menu where
  fmap f (Item a)   = Item (f a)
  fmap f (Sub a is) = Sub (f a) (map (fmap f) is)

instance Functor Crumb where
  fmap f (Crumb a ls rs) = Crumb (f a) (map (fmap f) ls) (map (fmap f) rs)

--

contentOf :: Menu a -> a
contentOf (Item a)  = a
contentOf (Sub a _) = a

fromMenu :: Menu a -> ZipMenu a
fromMenu item = (item, [])

toMenu :: ZipMenu a -> Menu a
toMenu = fst . upmost

--

left :: ZipMenu a -> Maybe (ZipMenu a)
left (_, []              ) = Nothing
left (_, Crumb _ [] _ :bs) = Nothing
left (t, Crumb a ls rs:bs) = Just (l, Crumb a ls' (t:rs):bs)
  where
    ls' = init ls
    l   = last ls

leftMost :: ZipMenu a -> ZipMenu a
leftMost z = maybe z leftMost $ left z

right :: ZipMenu a -> Maybe (ZipMenu a)
right (_, []                  ) = Nothing
right (_, Crumb _ _  []    :bs) = Nothing
right (t, Crumb a ls (r:rs):bs) = Just (r, Crumb a (ls ++ [t]) rs:bs)

rightMost :: ZipMenu a -> ZipMenu a
rightMost z = maybe z rightMost $ right z

up :: ZipMenu a -> Maybe (ZipMenu a)
up (_, [])               = Nothing
up (t, Crumb a ls rs:bs) = Just (Sub a (ls ++ [t] ++ rs), bs)

upmost :: ZipMenu a -> ZipMenu a
upmost z = maybe z upmost $ up z

down :: ZipMenu a -> Maybe (ZipMenu a)
down (Item _, _)        = Nothing
down (Sub a (t:ts), bs) = Just (t, Crumb a [] ts:bs)
down (Sub _ _, _)       = Nothing

modify :: (a -> a) -> ZipMenu a -> ZipMenu a
modify f (item, bs) = (modifyTop f item, bs)

zmap :: (a -> a) -> ZipMenu a -> ZipMenu a
zmap f (item, bs) = (f <$> item, map (f <$>) bs)

--

downTo :: (a -> Bool) -> ZipMenu a -> Maybe (ZipMenu a)
downTo _ (Item _, _)    = Nothing
downTo f (Sub a ts, bs) =
  work <$> findIndex (f . contentOf) ts
  where
    work i = let (ls, t:rs) = splitAt i ts
             in (t, Crumb a ls rs:bs)

--

isTop :: ZipMenu a -> Bool
isTop = null . snd

--

cursor :: ZipMenu a -> a
cursor = contentOf . fst

path :: ZipMenu a -> [a]
path (item, bs0) = fromItem item : fromCrumb bs0
  where
    fromItem (Item a)  = a
    fromItem (Sub a _) = a

    fromCrumb [] = []
    fromCrumb (Crumb a _ _:bs) = a : fromCrumb bs

siblings :: ZipMenu a -> ([a], a, [a])
siblings (t, [])              = ([], contentOf t, [])
siblings (t, Crumb _ ls rs:_) = (ls', contentOf t, rs')
  where
    ls' = map contentOf ls
    rs' = map contentOf rs

-- Helper

modifyTop :: (a -> a) -> Menu a -> Menu a
modifyTop f (Item a)   = Item (f a)
modifyTop f (Sub a ts) = Sub (f a) ts

--

data MenuStatus = OpenSub | Focus deriving (Eq, Show)

type WithStatus a = (Maybe MenuStatus, a)

toMenuBy :: (WithStatus a -> b) -> ZipMenu a -> Menu b
toMenuBy f = fmap f . toMenuWithInfo

toMenuWithInfo :: ZipMenu a -> Menu (WithStatus a)
toMenuWithInfo (t0, bs0) = fst . top True $ z0
  where
    setI a = (,) (Just a) . snd
    z0 = (t', bs')
      where
        setNoInfo = (,) Nothing
        t' = modifyTop (setI Focus) $ setNoInfo <$> t0
        bs' = map (fmap setNoInfo) bs0

    top :: Bool -> ZipMenu (WithStatus a) -> ZipMenu (WithStatus a)
    top p z = maybe z (top False) $ up' z
      where
        up' :: ZipMenu (WithStatus a) -> Maybe (ZipMenu (WithStatus a))
        up' (_, [])                 = Nothing
        up' (t, Crumb a ls rs:bs) =
          Just (Sub (setI OpenSub a) (ls ++ [t'] ++ rs), bs)
          where
            setter = if p then setI Focus else setI OpenSub
            t' = modifyTop setter t

--

drawMenu :: Show a => Menu a -> String
drawMenu = unlines . go 0
  where
    go :: Show a => Int -> Menu a -> [String]
    go n (Item a)   = [spaceN n ++ show a]
    go n (Sub a ts) = (spaceN n ++ show a) : ts'
      where
        ts' = concatMap (go (n + 1)) ts

spaceN :: Int -> String
spaceN n = replicate (2 * n) ' '
