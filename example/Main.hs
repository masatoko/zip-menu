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
        putStrLn " [d] : down"
        putChar '\n'
        putStrLn " [J] : action"
        putStrLn "===================="
        putStrLn " "

        a <- getChar
        putChar '\n'
        let z' = manipulate a z
            focusExitYes = case M.path z' of
                             (Yes:Exit:_) -> True
                             _            -> False
            quit = a == 'j' && focusExitYes
        unless quit $
          loop z'

  loop $ fromMaybe z0 $ M.down z0
  where
    z0 = M.fromMenu myMenu

    manipulate a z = func z
      where
        safe = fromMaybe z
        func = case a of
          'w' -> safe . M.prev
          's' -> safe . M.next
          'd' -> safe . down'
          'a' -> safe . M.up
          ' ' -> M.upmost
          'j' -> adjust . action
          _   -> id

    adjust z = case M.cursor z of
                 No -> fromMaybe z $ M.up z
                 _  -> z

    action = M.modify work
      where
        work (Switch a)  = Switch $ not a
        work (Counter a) = Counter $ a + 1
        work item        = item

    down' z
      | M.cursor z == Exit = M.downTo (== No) z
      | otherwise          = M.down z

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
  | Yes
  | No
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
      [ Item Yes
      , Item No
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
