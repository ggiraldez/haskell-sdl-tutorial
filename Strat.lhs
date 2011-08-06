> import qualified Data.Maybe as DM
> import qualified Data.Map as DMap
> import qualified System.Random as R
> import qualified Control.Monad as CM
> import qualified Graphics.UI.SDL as SDL
> import qualified Graphics.UI.SDL.Image as SDLi

> import UIState

> artFilePaths = [ "art/64x74_blue.png",
>                  "art/64x74_green.png",
>                  "art/64x74_white.png",
>                  "art/64x74_brown.png" ]

> tileWidth = 64
> tileHeight = 74
> windowWidth = 640
> windowHeight = 480
> mapRows = 100
> mapColumns = 100

> getRandomTerrain :: Int -> IO [TerrainType]
> getRandomTerrain l = do
>      randomNumbers <- CM.replicateM l $ R.randomRIO (0,terrainMaxBound)
>      return $ map toEnum randomNumbers

> makeRandomMap :: Int -> Int -> IO TerrainMap
> makeRandomMap w h = do
>      CM.foldM (\m y -> makeRow w y m) DMap.empty [1..h]
>  where
>      makeRow :: Int -> Int -> TerrainMap -> IO (TerrainMap)
>      makeRow w y tileMap = do
>          rt <- getRandomTerrain w
>          let tp = zip [1..w] rt
>          return $ foldr (\(x,t) m -> DMap.insert (x,y) t m) tileMap tp

> loadArt :: [String] -> IO TerrainSurfaces
> loadArt paths = do
>      tileSurfs <- mapM SDLi.load paths
>      return $ zip terrainTypes tileSurfs

> mapCoordinates :: [Point]
> mapCoordinates = [(x,y) | x <- [1..mapColumns], y <- [1..mapRows]]

> redrawScreen ::  UIState -> IO ()
> redrawScreen ui@(UIState vp _ mainSurf terrainSurfs terrainMap) = do
>     SDL.fillRect mainSurf Nothing (SDL.Pixel 0)
>     mapM_ (drawTile ui) mapCoordinates
>     SDL.flip mainSurf
>     return ()

> drawTile :: UIState -> Point -> IO ()
> drawTile (UIState vp _ mainSurf terrainSurfs tm) (x,y) = do
>      let sr = {-# SCC "dt-sr" #-} Just (SDL.Rect 0 0 tileWidth tileHeight)
>          (tX, tY) = {-# SCC "dT-gP2V" #-} gamePoint2View vp $ {-# SCC "dT-gHO" #-} getHexmapOffset tileWidth tileHeight x y
>          dr = {-# SCC "dT-dr" #-} Just $ SDL.Rect tX tY 0 0
>          tt = DM.fromJust $ {-# SCC "dT-tmLookup" #-} DMap.lookup (x,y) tm
>          terrainSurf = DM.fromJust $ {-# SCC "dT-tsLookup" #-} lookup tt terrainSurfs
>      if {-# SCC "dT-tIVP" #-} tileInViewPort vp tileWidth tileHeight (tX,tY)
>          then do
>                {-# SCC "dT-blit" #-} SDL.blitSurface terrainSurf sr mainSurf dr
>                return ()
>          else
>               return ()

> getHexmapOffset :: Int -> Int -> Int -> Int -> Point
> getHexmapOffset tileW tileH x y =
>      (adjX , adjY)
>   where
>      baseAdjX = (tileW * (x-1))
>      baseAdjY = (tileH * (y-1))
>      quarterH = tileH `div` 4
>      halfW = tileW `div` 2
>      adjX = if odd y
>                then baseAdjX + halfW
>                else baseAdjX
>      adjY = baseAdjY - ((y-1) * quarterH)

> tileInViewPort :: ViewPort -> Int -> Int -> Point -> Bool
> tileInViewPort (ViewPort _ _ vpW vpH) tileW tileH (pX , pY) =
>     let pX' = pX + tileW
>         pY' = pY + tileH
>     in
>         if ((pX' < 0) || (pX > vpW) || (pY' < 0) || pY > vpH)
>             then False else True

> gamePoint2View :: ViewPort -> Point -> Point
> gamePoint2View (ViewPort vpx vpy _ _) (gx , gy) =
>     ((gx + vpx) , (gy + vpy))

> main :: IO ()
> main = do
>      SDL.init [SDL.InitEverything]
>      SDL.setVideoMode windowWidth windowHeight 32 []    
>      SDL.setCaption "Video Test!" "video test"
>
>      mainSurf <- SDL.getVideoSurface
>      tileSurfs <- loadArt artFilePaths
>      randomMap <- makeRandomMap mapColumns mapRows
>
>      let initialUI = UIState (ViewPort 0 0 windowWidth windowHeight) [] mainSurf tileSurfs randomMap
>      eventLoop initialUI
>
>      mapM_ freeSurf tileSurfs
>      SDL.quit
>      putStrLn "done"
>   where
>      freeSurf (_ , s) = SDL.freeSurface s
>      eventLoop ui = do
>          e <- SDL.pollEvent
>          checkEvent ui e
>      checkEvent ui (SDL.NoEvent) = do
>          redrawScreen ui
>          e <- SDL.waitEvent
>          checkEvent ui e
>      checkEvent ui (SDL.KeyUp _) = return ()
>      checkEvent ui (SDL.MouseMotion _ _ xr yr ) = do
>          if elem SDL.ButtonRight $ uiMouseButtonsDown ui
>              then eventLoop ui'
>              else eventLoop ui
>        where
>          ui' = ui { uiViewPort = updatedVP }
>          updatedVP = vp { vpX = x', vpY = y' }
>          vp = uiViewPort ui
>          x' = (vpX vp) + fromIntegral xr
>          y' = (vpY vp) + fromIntegral yr
>      checkEvent ui (SDL.MouseButtonDown _ _ b) = do
>          let mbs = uiMouseButtonsDown ui
>          eventLoop $ ui { uiMouseButtonsDown = mbs ++ [b] }
>      checkEvent ui (SDL.MouseButtonUp _ _ b) = do
>          let mbs = uiMouseButtonsDown ui
>          let mbs' = filter (/= b) mbs
>          eventLoop $ ui { uiMouseButtonsDown = mbs' }
>      checkEvent ui _ = eventLoop ui

