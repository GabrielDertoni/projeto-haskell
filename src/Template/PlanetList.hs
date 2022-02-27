module Template.PlanetList (mkPlanetWordList) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

mkPlanetWordList :: String -> FilePath -> Q [Dec]
mkPlanetWordList name path = do
    ls <- lines <$> (qRunIO $ readFile path)
    let name' = mkName name
    let expq = listE $ map (litE . StringL) ls
    [d| $(pure $ VarP name') = $(expq) |]

