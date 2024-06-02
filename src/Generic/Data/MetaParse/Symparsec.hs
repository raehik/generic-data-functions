{-# LANGUAGE UndecidableInstances #-} -- due to type families

module Generic.Data.MetaParse.Symparsec where

import Symparsec.Parser ( PParser )
import Symparsec.Run ( Run', PrettyERun )
import TypeLevelShow.Doc ( RenderPDoc )

{- Symparsec stuff TODO
Note that we can't provide a handy Symparsec wrapper:
* can't do instances on parsers (type families inside due to parser unwrapping)
* can't cheat and do `Symparsec tag` because the type algebra doesn't work out
It's not a big loss, we just provide some helpers instead.

I don't want to include Symparsec as a dependency if I don't need to, and I
think I've designed this in such a way that I don't, so this might need to go in
another package :(
-}
type ResultOf (p :: PParser s r) = r

type family Idk e where
    Idk (Right '(a, b)) = Right a
    Idk (Left e) = Left (RenderPDoc (PrettyERun e))

type SymIt p str = Idk (Run' p str)
