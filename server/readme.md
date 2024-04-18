```haskell
data BranchGroup s a = BranchGroup (s -> [[(a,s)]])
```