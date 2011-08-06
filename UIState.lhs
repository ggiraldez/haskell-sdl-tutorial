> module UIState where

> import qualified Data.Map as DMap
> import qualified Graphics.UI.SDL as SDL

> data TerrainType = TTh_Blue | TTh_Green | TTh_White | TTh_Brown
>      deriving (Bounded, Eq, Enum, Ord, Show)

> terrainMaxBound :: Int
> terrainMaxBound = fromEnum (maxBound :: TerrainType)

> terrainTypes :: [TerrainType]
> terrainTypes = [minBound..maxBound]

> type Point = (Int, Int)
> type TerrainSurfaces = [(TerrainType, SDL.Surface)]
> type TerrainMap = DMap.Map Point TerrainType

> data ViewPort = ViewPort {
>                          vpX :: Int,
>                          vpY :: Int,
>                          vpWidth :: Int,
>                          vpHeight :: Int
>                          }

> data UIState = UIState {
>                        uiViewPort :: ViewPort,
>                        uiMouseButtonsDown :: [SDL.MouseButton],
>                        uiMainSurface :: SDL.Surface,
>                        uiTerrainSurfaces :: TerrainSurfaces,
>                        uiTerrainMap :: TerrainMap
>                        }

