module Lib () where

import Data.Complex

import GHC.Float

import Data.Maybe

import Data.List

import Graphics.Gloss

import Graphics.Gloss.Interface.IO.Game

--complex--

intersectionMPlayer :: [Maybe (Complex Float, [Char])] -> ((Player -> Player) -> Player) -> [Maybe (Complex Float, [Char])]
intersectionMPlayer cs p = map (`intersectionMPlayer'` p) cs

intersectionMPlayer' :: Maybe (Complex Float, [Char]) -> ((Player -> Player) -> Player) -> Maybe (Complex Float, [Char])
intersectionMPlayer' m p = do
    (com, col) <- m
    return (com - point, col)
    where (point, pa, vl) = getPlayerData p

minComplex :: [(Complex Float, [Char])] -> Maybe (Complex Float, [Char])
minComplex [] = Nothing
minComplex css@(c : cs) = if realPart (abs (fst c)) == mabs
    then Just c
    else minComplex cs
    where mabs = minimum (map (realPart . abs . fst) css)

cosHosei :: [Maybe (Complex Float, [Char])] -> ((Player -> Player) -> Player) -> [Maybe (Complex Float, [Char])]
cosHosei mcfc p = fun pa zipd
    where
        (point, pa, pl) = getPlayerData p
        zipd = zip mcfc [1 .. honsuu]
        fun :: Float -> [(Maybe (Complex Float, [Char]), Int)] -> [Maybe (Complex Float, [Char])]
        fun pa [] = []
        fun pa (m:may) = if isNothing (fst m)
            then Nothing : fun pa may
            else do
                let
                    (maycf, int) = m
                    (cf, cl) = fromJust maycf
                Just (cf * (cos (pa / 2 + pa * int2Float (snd m - 1) / int2Float honsuu) :+ 0), cl) : fun pa may

--wall--

type Wall = (Complex Float, Complex Float, [Char])

makeWall :: Wall -> (Wall -> Wall) -> Wall
makeWall (s, e, c) f = f (s, e + (0.000001 :+ 0.000001), c)

getWallData :: ((Wall -> Wall) -> Wall) -> Wall
getWallData a = a id

wallToFun :: ((Wall -> Wall) -> Wall) -> (Float -> Complex Float)
wallToFun w = \t -> s + (e - s) * (t :+ 0)
    where (s, e, c) = getWallData w

intersection :: ((Wall -> Wall) -> Wall) -> ((Wall -> Wall) -> Wall) -> Maybe (Complex Float, [Char])
intersection w1 w2 = if ((a1 <= realPart ans) && (c1 <= realPart ans) && (realPart ans <= (a1 + b1)) && (realPart ans <= (c1 + d1))) || ((a2 <= imagPart ans) && (c2 <= imagPart ans) && (imagPart ans <= (a2 + b2)) && imagPart ans <= (c2 + d2))
    then Just (ans, co2)
    else Nothing
    where
        (s1, e1, co1) = getWallData w1
        (s2, e2, co2) = getWallData w2
        (a1, a2) = complexToPoint s1
        (c1, c2) = complexToPoint s2
        (b1, b2) = complexToPoint (e1 - s1)
        (d1, d2) = complexToPoint (e2 - s2)
        t = (d2 * (a1 - c1) - d1 * (a2 - c2)) / (d1 * b2 - d2 * b1)
        ans = s1 + (t :+ 0) * (e1 - s1)

complexToPoint :: Complex Float -> (Float, Float)
complexToPoint c = (realPart c, imagPart c)


wallsToIntersections :: [(Wall -> Wall) -> Wall] -> [(Wall -> Wall) -> Wall] -> [Maybe (Complex Float, [Char])]
wallsToIntersections w1 w2 = reverse (map (minComplex . (\ w -> mapMaybe (intersection w) w2)) w1)

--player--

type Player = (Complex Float, Float, Float)

honsuu :: Int
honsuu = 64

vangle :: Float
vangle = pi / 2

makePlayer :: Player -> (Player -> Player) -> Player
makePlayer (point, angle, vlong) f = f (point, angle, vlong)

getPlayerData :: ((Player -> Player) -> Player) -> Player
getPlayerData p = p id

--graphics--

maybeFloatToPicPicture :: (Maybe (Float, [Char]), Float) -> Picture
maybeFloatToPicPicture (Nothing, _) = blank
maybeFloatToPicPicture (mftp, angle) = color (strToColor col (1 / (1 + rectabs) ^ 2)) (rectangleSolid 20 (500 / (rectabs * cos angle)))
    where
        rectabs = fst (fromJust mftp)
        col = snd (fromJust mftp)

worldToPicture :: ((World -> World) -> World) -> Picture
worldToPicture w = pictures (zipWith (\ p i -> translate ((int2Float i - int2Float honsuu / 2) * 20) 0 p) (zipWith (curry maybeFloatToPicPicture) (byouga w) ([vangle / 2- vangle * int2Float a / int2Float honsuu | a <- [0 .. honsuu - 1]])) [1 .. honsuu])
    where
        (player, walls, str) = getWorldData w
        (point, va, vl) = getPlayerData player


playerToWall :: ((Player -> Player) -> Player) -> [(Wall -> Wall) -> Wall]
playerToWall p = [makeWall (point, point + h, "") | h <- wps]
    where
        (point, angle, vlong) = getPlayerData p
        wps = [(vlong :+ 0) * (cos a :+ sin a) | a <- [angle - vangle / 2 + int2Float x * vangle / int2Float (honsuu - 1) | x <- [0 .. (honsuu - 1)]]]

byouga :: ((World -> World) -> World) -> [Maybe (Float, [Char])]
byouga w = [if isNothing x then Nothing else Just (realPart (abs (fst (fromJust x))), snd (fromJust x)) | x <- intersectionMPlayer (wallsToIntersections (playerToWall p) ws) p]
    where (p, ws, str) = getWorldData w

strToDisplay :: [Char] -> [Char]
strToDisplay str = if odd l
    then [' ' | a <- [1 .. (div (21 - l) 2)]] ++ str ++ [' ' | a <- [1 .. (div (21 - l) 2 - 1)]]
    else [' ' | a <- [1 .. (div (20 - l) 2)]] ++ str ++ [' ' | a <- [1 .. (div (20 - l) 2)]]
    where l = length str

--16->10--


strToColor :: [Char] -> Float -> Color
strToColor (s:str) = makeColor (16 * head ints + ints !! 1) (16 * ints !! 2 + ints !! 3) (16 * ints !! 4 + ints !! 5)
    where ints = map strToNum str

strToNum :: Char -> Float
strToNum 'a' = 10
strToNum 'b' = 11
strToNum 'c' = 12
strToNum 'd' = 13
strToNum 'e' = 14
strToNum 'f' = 15
strToNum x = (read :: [Char] -> Float) [x]

--world--

type World = ((Player -> Player) -> Player, [(Wall -> Wall) -> Wall], ([Char], Int))

makeWorld :: World -> (World -> World) -> World
makeWorld (p, ws, str) f = f (p, ws, str)

getWorldData :: ((World -> World) -> World) -> World
getWorldData w = w id

updateWorld :: Event -> ((World -> World) -> World) -> (World -> World) -> World
updateWorld (EventKey key ks _ _) box = updateWorldWithKey key ks box
updateWorld (EventMotion _)       box = box
updateWorld (EventResize _)       box = box

up, down, right, left, turnright, turnleft :: ((World -> World) -> World) -> ((World -> World) -> World)
up    w = makeWorld (makePlayer (point + (0.5 :+ 0) * exp ((0 :+ 1) * (pv :+ 0)), pv, pl), ws, ("None", 0))
    where
        (p, ws, str) = getWorldData w
        (point, pv, pl) = getPlayerData p
down  w = makeWorld (makePlayer (point - (0.5 :+ 0) * exp ((0 :+ 1) * (pv :+ 0)), pv, pl), ws, ("None", 0))
    where
        (p, ws, str) = getWorldData w
        (point, pv, pl) = getPlayerData p
right w = makeWorld (makePlayer (point + (0.5 :+ 0), pv, pl), ws, ("None", 0))
    where
        (p, ws, str) = getWorldData w
        (point, pv, pl) = getPlayerData p
left  w = makeWorld (makePlayer (point + ((- 0.5) :+ 0), pv, pl), ws, ("None", 0))
    where
        (p, ws, str) = getWorldData w
        (point, pv, pl) = getPlayerData p
turnright w = makeWorld (makePlayer (point, pv - pi / 8, pl), ws, ("None", 0))
    where
        (p, ws, str) = getWorldData w
        (point, pv, pl) = getPlayerData p
turnleft  w = makeWorld (makePlayer (point, pv + pi / 8, pl), ws, ("None", 0))
    where
        (p, ws, str) = getWorldData w
        (point, pv, pl) = getPlayerData p

updateWorldWithKey :: Key -> KeyState -> ((World -> World) -> World) -> (World -> World) -> World
updateWorldWithKey (SpecialKey KeyUp)    ks = if ks == Down then up    else id
updateWorldWithKey (SpecialKey KeyDown)  ks = if ks == Down then down  else id
updateWorldWithKey (SpecialKey KeyRight) ks = if ks == Down then right else id
updateWorldWithKey (SpecialKey KeyLeft)  ks = if ks == Down then left  else id
updateWorldWithKey (Char 'd')            ks = if ks == Down then turnright else id
updateWorldWithKey (Char 'a')            ks = if ks == Down then turnleft  else id
updateWorldWithKey _ _ = id


{-
next :: Float -> ((World -> World) -> World) -> ((World -> World) -> World)
next f w
    | fst atate == "None" = w
    | 
    |
    where (p, wo, state) = getWorldData w
-}

next f w = w


--main--

main :: IO ()
main = do
    let
        p = makePlayer (0, pi / 2, 5)
        a = makeWall (1.5, 0 :+ 1.5, "#0000ff")
        b = makeWall (0 :+ 1, 10 :+ 1, "#ffffff")
        c = makeWall (0 :+ 2, 10 :+ 2, "#00ff00")
        w = makeWorld (p, [b, c], ("None", 0))

    play FullScreen black 24 w worldToPicture updateWorld next
    return ()