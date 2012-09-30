import qualified Data.Map as M
       
main = do
    let m :: M.Map Int Int
        m = M.fromList [ (1,1), (2,2) ]
    print $ M.findWithDefault 6 1 m
    print $ M.findWithDefault 6 2 m

    print $ M.lookup 1 m
    print $ M.lookup 2 m
    