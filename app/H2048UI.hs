{-# LANGUAGE OverloadedStrings #-}

module H2048UI where

import           Brick
import           Brick.BChan
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Monad              (forever, void)
import           Control.Monad.IO.Class
import           Data.List                  (find)
import           Data.Maybe
import qualified Graphics.Vty               as V
import           H2048
import           H2048AI


-- | Named resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
type Name = ()

data Tick = Tick

data Game = Game
  { _board :: Board
  , _players :: [Player]
  , _failed :: Bool
  , _selected :: Maybe Player
  }

app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

continueMove :: Move -> Game -> EventM Name (Next Game)
-- in AI mode, do not respond to move event
continueMove _ g@Game{_selected=Just _} = continue g
continueMove m g                        = continueMove' (Just m) g

continueMove' :: Maybe Move -> Game -> EventM Name (Next Game)
continueMove' Nothing g = continue g{_failed=True}
continueMove' (Just m) g@Game{_board=b} = do
  let b' = move m b
  if b' == b
    then continue g
    else liftIO (addRandom b') >>= (continue .(\nb-> g{_board=nb}). fromJust)


-- Handling events
handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent b (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt b
handleEvent b (VtyEvent (V.EvKey V.KEsc []))        = halt b
handleEvent _ (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO initGame >>= continue
handleEvent g@Game{_failed=True} _ = continue g
handleEvent g@Game {_selected = Just p, _board = b} (AppEvent Tick) =
  liftIO (getMove p b) >>= \m -> continueMove' m g
handleEvent b (VtyEvent (V.EvKey (V.KChar 'k') [])) = continueMove UP b
handleEvent b (VtyEvent (V.EvKey (V.KChar 'j') [])) = continueMove DOWN b
handleEvent b (VtyEvent (V.EvKey (V.KChar 'l') [])) = continueMove RIGHT b
handleEvent b (VtyEvent (V.EvKey (V.KChar 'h') [])) = continueMove LEFT b
handleEvent g (VtyEvent (V.EvKey (V.KChar 'H') [])) = selectPlayer g Nothing
handleEvent g@Game{_players=players} (VtyEvent (V.EvKey (V.KChar c) [])) |
  isJust matchedPlayer = selectPlayer g matchedPlayer
  where matchedPlayer = find ((c ==) . head . playerName) players
handleEvent b _                                     = continue b

selectPlayer :: Game -> Maybe Player -> EventM Name (Next Game)
selectPlayer g@Game{_selected = p} mp
  | p == mp = continue g
  | otherwise = do
      b <- liftIO initBoard
      continue g{_board=b,_selected=mp}

-- Drawing
newtype Cell = Cell Int

drawUI :: Game -> [Widget Name]
drawUI g =
    [ C.vCenter $
      vLimit 66 $
      vBox [C.hCenter $ withAttr "insight" (drawBoardStat g),
      hBox
          [ padLeft Max $
            padRight (Pad 2) $ drawStatus g
          , drawCells (_board g)
          , padRight Max $ padLeft (Pad 2) $ drawPlayers (_players g)]]]

drawPlayers :: [Player] -> Widget Name
drawPlayers players = hLimit 22 $ withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "AI")
  $ vBox rows
  where rows = drawStat "Human" "H":fmap draw' players
        draw' p = let name = playerName p in drawStat name (take 1 name)

score :: Board -> String
score = show . sum . fmap sum . _cells

drawStatus :: Game -> Widget Name
drawStatus Game {_failed = failed, _board = b, _selected = p} =
  hLimit 22 $
  withBorderStyle BS.unicodeBold $
  B.borderWithLabel (str "Stats") $
  vBox $ if failed
  then failHint : status
  else status
  where
    name = maybe "Human" playerName p
    status = [
      drawStat "Score" (score b),
      padTop (Pad 1) $ drawStat "Player" name
      ]
    failHint = padTop (Pad 1) (withAttr "fail" $ drawStat "FAILED" "!!!")

drawBoardStat :: Game -> Widget Name
drawBoardStat Game{_board = b} =
    hLimit 66 $
    withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str "BoardInsight") $
    vBox
        [ drawStat "empty" (show . emptyNum $ stat)
        , drawStat "order" (show . ordered $ stat)
        , drawStat "maxAtEnd" (show . maxAtEnd $ stat)
        , drawStat "diverge" (show . diverge $ stat)
        , drawStat "closePairs" (show . closePairs $ stat)]
  where
    stat = toStat b


drawStat :: String -> String -> Widget Name
drawStat s n = padLeftRight 1
  $ str s <+> padLeft Max (str n)

drawCells :: Board -> Widget Name
drawCells b = hLimit 24 $ withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "2048")
  $ vBox rows
  where
    (ColSize c)  = _colSize b
    (RowSize r)  = _rowSize b
    cells :: [[Int]]
    cells  = _cells b
    rows         = [hBox $ cellsInRow y | y <- [0..r-1]]
    cellsInRow :: Int -> [Widget Name]
    cellsInRow y = [drawCoord (Cell $ cells !! y !! x) | x <- [0..c-1]]
    drawCoord    = drawCell


drawCell :: Cell -> Widget Name
drawCell (Cell n) = withAttr (attr n) $ cw n

cw :: Int -> Widget Name
cw 0 = str "     "
cw i = str $ formatNum i
  where
    formatNum n =
      let ns = show n
          l = length ns
          toFill
            | l <= 5 = 5 - l
            | otherwise = 0
      in ns ++ replicate toFill ' '

attr :: Int -> AttrName
attr = attrName . ("cell" ++) . show

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ ("cell0", rgbfg 238 228 218)
    , ("cell2", rgbfg 238 228 218)
    , ("cell4", rgbfg 237 224 200)
    , ("cell8", rgbfg 242 177 121)
    , ("cell16", rgbfg 245 149 99)
    , ("cell32", rgbfg 246 124 95)
    , ("cell64", rgbfg 246 94 59)
    , ("cell128", rgbfg 237 207 114)
    , ("cell256", rgbfg 237 204 97)
    , ("cell512", rgbfg 237 200 80)
    , ("cell1024", rgbfg 237 197 63)
    , ("cell2048", rgbfg 237 194 46)
    , ("fail", fg V.red)
    , ("insight", rgbfg 237 194 46)
    ]
  where
    rgbfg :: Integer -> Integer -> Integer -> V.Attr
    rgbfg r g b= fg $ V.rgbColor r g b

initBoard :: IO Board
initBoard = fmap fromJust $ addRandom $ makeBoard (ColSize 4) (RowSize 4)

initGame :: IO Game
initGame = fmap genGame initBoard
  where
    genGame :: Board -> Game
    genGame b =
      Game {_board = b,
            _players = [ randomPlayer
                       , minmaxPlayer
                       , alphaBetaPlayer
                       -- , exptPlayer
                       ],
            _failed = False,
            _selected = Nothing}

h2048UI :: IO ()
h2048UI = do
  chan <- newBChan 10
  _ <- forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000
  g <- initGame
  void $ customMain (V.mkVty V.defaultConfig) (Just chan) app g
