module Control.Monad.ProofT where

newtype ProofT s1 s2 m a = ProofT { runProofT :: a }


