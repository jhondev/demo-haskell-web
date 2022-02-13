{-# LANGUAGE DuplicateRecordFields #-}

module Lib
  ( someFunc,
  )
where

import ClassyPrelude

-- newtype User = User
--   { name :: Text
--   }

-- newtype User2 = User2
--   { name :: Text
--   }

someFunc :: IO ()
someFunc = do
  putStrLn "User name: "
