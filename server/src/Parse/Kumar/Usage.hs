
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}




module Parse.Kumar.Usage where
import Parse.Kumar.Parser
import Language.LBNF.Compiletime
import Language.LBNF.Runtime
import Language.LBNF (lbnf, bnfc)

-- e = [expr| let rec f x = x and rec g y = y in z |]