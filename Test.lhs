First, some qualified imports

> import qualified Data.Maybe as DM
> import qualified Data.Map as DMap
> import qualified System.Random as R
> import qualified Control.Monad as CM
> import qualified Graphics.UI.SDL as SDL
> import qualified Graphics.UI.SDL.Image as SDLi

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

All the terrain types

> terrainTypes :: [TerrainType]
> terrainTypes = [minBound..maxBound]

Terrain types can be random too...

> instance R.Random TerrainType where
>   randomR (a,b) = (onFst toEnum) . (R.randomR (fromEnum a, fromEnum b))
>     where onFst f (a,b) = (f a,b)
>   random = R.randomR (minBound, maxBound)

Create a 2D random map

> makeRandomMap :: Int -> Int -> IO TerrainMap
> makeRandomMap w h = do
>     tiles <- CM.replicateM (w*h) R.randomIO
>     return $ DMap.fromAscList (zip [(x,y) | x <- [1..w], y <- [1..h]] tiles)

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
>     mainSurf <- SDL.getVideoSurface
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
>     checkEvent (SDL.KeyUp _) = return ()
>     checkEvent (SDL.MouseMotion x y xr yr) = 
>         putStrLn ("MouseMotion X:" ++ show x ++ "  Y:" ++ show y ++
>                   "  RX:" ++ show xr ++ "  RY:" ++ show yr)
>                   >> eventLoop
>     checkEvent (SDL.MouseButtonDown x y b) = 
>         putStrLn ("MouseBDown X:" ++ show x ++ "  Y:" ++ show y ++ 
>                   "  B:" ++ show b)
>                   >> eventLoop
>     checkEvent (SDL.MouseButtonUp x y b) = 
>         putStrLn ("MouseBUp X:" ++ show x ++ "  Y:" ++ show y ++ 
>                   "  B:" ++ show b)
>                   >> eventLoop
>     checkEvent _ = eventLoop

