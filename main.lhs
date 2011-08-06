First, some qualified imports

> import qualified Data.Maybe as DM
> import qualified Data.Map as DMap
> import qualified System.Random as R
> import qualified Control.Monad as CM
> import Graphics.UI.SDL as SDL
> import Graphics.UI.SDL.Image as SDLi

These are the assets for the terrain

> artFilePaths = [ "art/64x74_blue.png",
>                  "art/64x74_green.png",
>                  "art/64x74_white.png",
>                  "art/64x74_brown.png" ]

Data types for the definition of the terrain

> data TerrainType = TTh_Blue | TTh_Green | TTh_White | TTh_Brown
>      deriving (Bounded, Eq, Enum, Ord, Show)
>
> type Point = (Int, Int)
> type TerrainSurfaces = [(TerrainType, SDL.Surface)]
> type TerrainMap = DMap.Map Point TerrainType

How many terrain types do we have?

> terrainMaxBound :: Int
> terrainMaxBound = fromEnum (maxBound :: TerrainType)

Enumeration of the terrain types, not sure what for yet...

> terrainTypes :: [TerrainType]
> terrainTypes = enumFrom TTh_Blue

Generate an array of random terrain types

> getRandomTerrain :: Int -> IO [TerrainType]
> getRandomTerrain l = do
>     randomNumbers <- CM.replicateM l $ R.randomRIO (0, terrainMaxBound)
>     return $ map toEnum randomNumbers

Create a 2D random map

> makeRandomMap :: Int -> Int -> IO TerrainMap
> makeRandomMap w h = do
>     CM.foldM (\m y -> makeRow w y m) DMap.empty [1..h]
>   where
>     makeRow :: Int -> Int -> TerrainMap -> IO TerrainMap
>     makeRow w y tileMap = do
>         rt <- getRandomTerrain w
>         let tp = zip [1..w] rt
>         return $ foldr (\(x,t) m -> DMap.insert (x,y) t m) tileMap tp

Load the assets

> loadArt :: [String] -> IO TerrainSurfaces
> loadArt paths = do
>     tileSurfs <- mapM SDLi.load paths
>     return $ zip terrainTypes tileSurfs

Blit the surface of the terrain tile to another surface (e.g. the main screen)

> drawTile :: SDL.Surface -> TerrainSurfaces -> TerrainMap -> Point -> IO ()
> drawTile mainSurf terrainSurfs tm (x,y) = do
>     let sr = Just (SDL.Rect 0 0 64 74)
>     let dr = Just $ getHexmapOffset 64 74 x y
>     let tt = DM.fromJust $ DMap.lookup (x,y) tm
>     let terrainSurf = DM.fromJust $ lookup tt terrainSurfs
>     SDL.blitSurface terrainSurf sr mainSurf dr
>     return ()

Ooops... missing function here: calculate the rectangle of a tile given its
map coordinates

> getHexmapOffset :: Int -> Int -> Int -> Int -> SDL.Rect
> getHexmapOffset tileW tileH x y =
>     SDL.Rect adjX adjY 0 0
>   where
>     baseAdjX = (tileW * (x-1))
>     baseAdjY = (tileH * (y-1))
>     quarterH = tileH `div` 4
>     halfW = tileW `div` 2
>     adjX = if odd y
>               then baseAdjX + halfW
>               else baseAdjX
>     adjY = baseAdjY - ((y-1) * quarterH)


===============================================================================
The main thingy

> main :: IO ()
> main = do
>     SDL.init [SDL.InitEverything]
>     SDL.setVideoMode 640 480 32 []
>     SDL.setCaption "Video Test!" "video test"
>
>     mainSurf <- getVideoSurface
>     tileSurfs <- loadArt artFilePaths
>
>     randomMap <- makeRandomMap 9 8
>
>     mapM_ (drawTile mainSurf tileSurfs randomMap) $ DMap.keys randomMap
>     SDL.flip mainSurf
>
>     eventLoop
>     mapM_ freeSurf tileSurfs
>     SDL.quit
>     print "done"
>  where
>     freeSurf (_, s) = SDL.freeSurface s
>     eventLoop = SDL.waitEventBlocking >>= checkEvent
>     checkEvent (KeyUp _) = return ()
>     checkEvent _         = eventLoop

