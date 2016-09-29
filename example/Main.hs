{-# LANGUAGE MultiWayIf #-}

module Main where

import Control.Monad (when, unless)
import Data.Char (toUpper)
import Data.Maybe (fromMaybe)

import qualified ZipMenu as M
import ZipMenu (Menu (..), WithStatus, MenuStatus (..))

main :: IO ()
main = do
  let loop z = do
        print z
        -- putStrLn $ M.drawMenu . M.toMenu $ z
        putChar '\n'
        putStrLn $ filter (/= '"') . M.drawMenu . M.toMenuWithInfo $ z
        putChar '\n'
        putStrLn "# Menu Tree"
        renderTest . M.toMenuWithInfo $ z

        putChar '\n'
        putStrLn "# Cursor"
        print $ M.cursor z
        putChar '\n'
        putStrLn "# Path"
        print $ M.path z

        putChar '\n'
        putStrLn "== Move cursor by =="
        putStrLn " [w] : prev"
        putStrLn " [s] : next"
        putStrLn " [a] : up"
        putStrLn " [d] : down, action"
        putChar '\n'
        putStrLn " [ ] : top"
        putStrLn "===================="
        putStrLn " "

        a <- getChar
        putChar '\n'
        let z' = manipulate a z
            quit = case M.path z' of
                     (Yes True:Exit:_) -> True
                     _                 -> False
        unless quit $
          loop $ adjust z'

  loop $ fromMaybe z0 $ M.down z0
  where
    z0 = M.fromMenu myMenu

    manipulate a z = func z
      where
        safe = fromMaybe z
        func = case a of
          'w' -> fromMaybe (M.rightMost z) . M.left
          's' -> fromMaybe (M.leftMost z) . M.right
          'd' -> fromMaybe (action z) . down'
          'a' -> safe . M.up
          ' ' -> M.upmost
          _   -> id

    adjust z = unselect $
      case M.cursor z of
        No True -> fromMaybe z $ M.up z
        _       -> z

    unselect = M.zmap work
      where
        work (Yes _) = Yes False
        work (No _)  = No False
        work t       = t

    action = M.modify work
      where
        work (Switch a)  = Switch $ not a
        work (Counter a) = Counter $ a + 1
        work (Yes _)     = Yes True
        work (No _)      = No True
        work item        = item

    down' z
      | M.cursor z == Exit = M.downTo isNo z
      | otherwise          = M.down z
      where
        isNo (No _) = True
        isNo _      = False

data MyItem
  = Root
  | Start
  | Option
  | OptA
  | OptB
  | Switch Bool
  | Counter Int
  | OptC
  | Credit
  | Exit
  --
  | Yes Bool
  | No Bool
  deriving (Show, Eq)

myMenu :: Menu MyItem
myMenu =
  Sub Root
    [ Item Start
    , Sub Option
      [ Item OptA
      , Sub OptB
        [ Item (Switch False)
        , Item (Counter 0)
        ]
      , Item OptC
      ]
    , Item Credit
    , Sub Exit
      [ Item (Yes False)
      , Item (No False)
      ]
    ]

renderTest :: Menu (WithStatus MyItem) -> IO ()
renderTest = go 0
  where
    go depth m = do
      putStrLn $ indent ++ cursor ++ mark:' ':show a
      case m of
        (Item _)   -> return ()
        (Sub _ ts) -> when (mInfo == Just OpenSub) $ mapM_ (go (depth + 1)) ts
      where
        isSub = case m of
                  (Sub _ _) -> True
                  _         -> False
        (mInfo,a) = M.contentOf m
        indent = replicate (2*depth) ' '
        cursor = if mInfo == Just Focus
                   then "> "
                   else "  "
        mark = if | mInfo == Just OpenSub -> '-'
                  | isSub                 -> '+'
                  | otherwise             -> '.'
