{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Text.Regex.Posix

foo :: ()
foo = $(do
    let mapFile = "C:   000009A7  _initial_step"
    runIO $ print (getAllTextSubmatches (mapFile =~ "^C: +([0-9ABCDEF]{8}) +_initial_step") :: [String])
    return $ TupE []
    )

main = print foo
