-- | Her orders, on screen: the saves as three delivery tickets taped over the title's
-- lighthouse, and the slip the hall prints when an order is saved.
--
-- A ticket is thermal paper printed in the pixel font of her talk, so it sits with
-- her; the screen's own words are in the stations' stroke font, as in the hall.
-- On a ticket, from the top: whose order and how it stands, the order number (the
-- largest thing, since a player first looks for theirs), the route from the wick
-- through the three rooms to the lamp, the next stop by name, then the time on the
-- road, the era shifts and when it was saved. A delivered order is stamped.
--
-- Everything is drawn in canvas units; the shapes that slant step in whole pixels
-- three units square, like the talk box.
module OrdersScreen
  ( ordersScreen
  , hallOverlay
  ) where

import Data.Char (toUpper)
import Draw
import Font
import Game.Orders
import Game.Run (lastVisible)
import Game.Types
import Graphics.Gloss
import PixelFont
import TalkBox (askLayer, pixelPoly)
import Title (titleBackground)

ink, rim, sun, muted, night, paper, faint, stampRed, tapeYellow :: Int
ink = 0x0D0B12        -- printing, and the talk box
rim = 0xF4F1EA        -- the talk box's edge
sun = 0xF6B31C        -- her jacket: the chosen ticket's shadow, what matters
muted = 0x8C8796
night = 0x05080A
paper = 0xF2EEE4      -- thermal paper
faint = 0x6B6575      -- labels printed on it
stampRed = 0xC63D2F
tapeYellow = 0xF6D27A

-- Pixel text in a colour, hanging from (x, y).
ptext :: Float -> Int -> String -> Float -> Float -> Picture
ptext p c s x y = Color (hex c) (Translate x y (pixelText p s))

ptextR, ptextC :: Float -> Int -> String -> Float -> Float -> Picture
ptextR p c s x = ptext p c s (x - fromIntegral (pixelW s) * p)
ptextC p c s x = ptext p c s (x - fromIntegral (pixelW s) * p / 2)

-- The screen --------------------------------------------------------------------

slotX :: Int -> Float
slotX i = [-330, 0, 330] !! i

slotTop :: Float
slotTop = 234

-- | Where a ticket in question comes to: clear of her face and over her box.
askX, askTop :: Float
askX = 140
askTop = 291

ordersScreen :: [Level] -> Book -> Desk -> Int -> Picture
ordersScreen levels book desk clock = Pictures $
  [ titleBackground (fromIntegral clock)
  , Color (hexA night (0.6 + 0.22 * f)) (rectC canvasW canvasH)
  , fadePicture (headerK * (1 - f)) header
  ]
  ++ [ ticketAt i | i <- [0 .. slotCount - 1], i /= sel ]
  ++ [ ticketAt sel ]
  ++ [ arrowAt (slotX sel) (slotTop + 15 * liftOf desk sel + 30 + bob + rise sel) | dkStep desk == Choosing, f < 0.01 ]
  ++ [ fadePicture (headerK * (1 - f)) (keyRow keys (-358)) | not printingNow ]
  ++ [ note n left | Just (n, left) <- [dkNote desk] ]
  ++ [ ask | Just ask <- [question] ]
  where
    sel = dkSel desk
    -- While a new order prints, no key does anything, so none is offered.
    printingNow = case dkStep desk of
      Printing _ -> True
      _ -> False
    f = focusOf desk
    age = dkAge desk
    headerK = clamp01 (fromIntegral age / 12)
    bob = if (clock `div` 15) `mod` 2 == 0 then 0 else 6
    -- The tickets rise into place as the desk opens, one after another.
    rise i = let k = clamp01 (fromIntegral (age - 5 * i) / 18) in negate 560 * (1 - smooth k) ** 2
    header = Pictures
      [ Color (hex 0xFFF0CE) (Translate 0 316 (textC 30 "YOUR ORDERS"))
      , Color (hex 0x98ADB1) (Translate 0 286 (textC 13 "Three saves. Each one is kept every time a room is cleared."))
      ]
    keys = case orderAt sel book of
      Just _ -> [("A / D", "Choose"), ("ENTER", "Continue"), ("DEL", "Cancel order"), ("ESC", "Title")]
      Nothing -> [("A / D", "Choose"), ("ENTER", "New order"), ("ESC", "Title")]
    ticketAt i
      | i == sel = chosen
      | otherwise = fadePicture (1 - 0.96 * f) (Translate 0 (rise i) (plain i))
      where
        top = slotTop + 15 * liftOf desk i
        plain k = case orderAt k book of
          Just o -> ticket levels (k + 1) o (slotX k) top False 1 Nothing
          Nothing -> emptyTicket (k + 1) (slotX k) top False
        chosen = case (dkStep desk, orderAt i book) of
          (Printing t, _) -> printing levels (i + 1) (slotX i) top t
          (Tearing t, Just o) -> tearing levels (i + 1) o askX askTop t
          (step, Just o) ->
            let tear = case step of
                  Asking _ held -> Just (fromIntegral held / fromIntegral holdTicks)
                  _ -> Nothing
            in Translate 0 (rise i) (ticket levels (i + 1) o (lerp f (slotX i) askX) (lerp f top askTop) True 0 tear)
          (_, Nothing) -> Translate 0 (rise i) (emptyTicket (i + 1) (slotX i) top True)
    question = case (dkStep desk, orderAt sel book) of
      (Asking a held, Just o) ->
        let said = cancelLine levels (sel + 1) o
        in Just (askLayer f said (min (length said) (askTyped a)) a (fromIntegral held / fromIntegral holdTicks) clock)
      (Tearing _, Just o) ->
        let said = cancelLine levels (sel + 1) o
        in Just (askLayer f said (length said) 9999 1 clock)
      (Choosing, Just o) | f > 0.01 ->
        let said = cancelLine levels (sel + 1) o
        in Just (askLayer f said (length said) 9999 0 clock)
      _ -> Nothing
    note (Cancelled n) left =
      fadePicture (clamp01 (fromIntegral left / 12)) (toast ("ORDER " ++ show n ++ " CANCELLED") (-270) stampRed)

-- | A tag under the tickets: paper-white letters on ink, over a coloured shadow.
toast :: String -> Float -> Int -> Picture
toast msg y shadow = Pictures
  [ Color (hex shadow) (rect (negate w / 2 + 6) (y - h - 6) w h)
  , Color (hex ink) (rect (negate w / 2) (y - h) w h)
  , ptextC 2 rim msg 0 (y - 11)
  ]
  where
    w = fromIntegral (pixelW msg) * 2 + 30
    h = 36

-- | The marker arrow of her talk, pointing down at (x, y).
arrowAt :: Float -> Float -> Picture
arrowAt x y = Translate x y (Pictures
  [ Pictures [ Color (hex night) (rect (fromIntegral (x0 - 1) * 3 - 10.5) (fromIntegral (9 - i) * 3 - 3) (fromIntegral (n + 2) * 3) 9)
             | (i, (x0, n)) <- rows ]
  , Pictures [ Color (hex sun) (rect (fromIntegral x0 * 3 - 10.5) (fromIntegral (9 - i) * 3) (fromIntegral n * 3) 3)
             | (i, (x0, n)) <- rows ]
  ])
  where
    rows = zip [0 :: Int ..] [ (0, 7), (0, 7), (0, 7), (-3, 13), (-2, 11), (-1, 9), (0, 7), (1, 5), (2, 3), (3, 1) ] :: [(Int, (Int, Int))]

-- | The keys under the screen, centred, in the title's keycap style.
keyRow :: [(String, String)] -> Float -> Picture
keyRow items y = Pictures (go startX items)
  where
    gap = 34
    width (k, l) = textW 12 k + 16 + 9 + textW 12 l
    total = sum (map width items) + gap * fromIntegral (length items - 1)
    startX = negate total / 2
    go _ [] = []
    go x (it@(key, label) : rest) =
      let w = textW 12 key + 16
      in Translate x y (Pictures
           [ Color (hexA 0xFFFFFF 0.6) (rect 0 (-7) w 24)
           , Color (hex 0x0B1014) (rect 1.5 (-5.5) (w - 3) 21)
           , Color (hex 0xF4EEE2) (Translate (w / 2) 0 (textC 12 key))
           , Color (hexA 0xF4EEE2 0.85) (Translate (w + 9) 0 (textL 12 label))
           ])
         : go (x + width it + gap) rest

-- A ticket ---------------------------------------------------------------------------

ticketW, ticketH :: Float
ticketW = 288
ticketH = 450

-- | Where a ticket tears: along the perforation under its order number.
tearBelow :: Float
tearBelow = 114

-- | Paper from @hi@ down to @lo@, with torn teeth on the edges asked for.
paperPart :: Float -> Float -> Float -> Bool -> Bool -> Picture
paperPart x hi lo teethTop teethBottom = Pictures $
  [ rect left (lo + (if teethBottom then t else 0)) ticketW (hi - lo - (if teethTop then t else 0) - (if teethBottom then t else 0)) ]
  ++ [ pixelPoly [(l, hi - t), (l + tw, hi - t), (l + tw / 2, hi)] | teethTop, l <- teeth ]
  ++ [ pixelPoly [(l, lo + t), (l + tw / 2, lo), (l + tw, lo + t)] | teethBottom, l <- teeth ]
  where
    left = x - ticketW / 2
    n = 24 :: Int
    tw = ticketW / fromIntegral n
    t = 6
    teeth = [ left + fromIntegral i * tw | i <- [0 .. n - 1] ]

-- | A piece of printing, and the heights it runs between.
data Bit = Bit !Float !Float Picture

-- | Everything printed on an order's ticket.
printed :: [Level] -> Int -> Order -> Float -> Float -> Bool -> [Bit]
printed levels n o x top torn =
  [ Bit (top - 24) (top - 38) (ptext 2 faint "RIDER: MEI" left (top - 24))
  , Bit (top - 19) (top - 43) (if done then chip "DONE" ink rim else chip "EN ROUTE" sun ink)
  , Bit (top - 50) (top - 78) (ptext 4 ink ("ORDER " ++ show n) left (top - 50))
  , Bit (top - 90) (top - 104) (ptext 2 faint "1x LAMP WICK > TOP FLOOR" left (top - 90))
  ]
  ++ [ Bit (top - 114) (top - 116) (dashes (top - 114)) | not torn ]
  ++ [ Bit (top - 128) (top - 184) (route levels x (top - 152) (odCompleted o)) ]
  ++ nextStop
  ++ [ Bit (top - 260) (top - 262) (dashes (top - 260))
     , Bit (top - 276) (top - 290) (stat (top - 276) "ON THE ROAD" (clockOf (odTicks o)))
     , Bit (top - 302) (top - 316) (stat (top - 302) "ERA SHIFTS" (show (odShifts o)))
     , Bit (top - 328) (top - 342) (stat (top - 328) "SAVED" (maybe "-" stampText (odSaved o)))
     , Bit (top - 352) (top - 354) (dashes (top - 352))
     , Bit (top - 364) (top - 398) (barcode x (top - 364) (n * 7 + odCompleted o))
     , Bit (top - 408) (top - 422) (ptextC 2 faint ("No. 000" ++ show n ++ "  " ++ show (min visible (odCompleted o)) ++ "/" ++ show visible) x (top - 408))
     ]
  ++ [ Bit (top - 353) (top - 415) (stamp (x + 38) (top - 384) (maybe "" dayText (odSaved o)) n) | done ]
  where
    left = x - 126
    right = x + 126
    visible = lastVisible levels + 1
    done = delivered levels o
    chip msg bg fg =
      let w = fromIntegral (pixelW msg) * 2 + 14
      in Pictures [ Color (hex bg) (rect (right - w) (top - 43) w 24), ptext 2 fg msg (right - w + 7) (top - 24) ]
    dashes y = Color (hexA faint 0.7) (Pictures [ rect dx y (min 6 (right + 2 - dx)) 2 | dx <- [left - 2, left + 8 .. right] ])
    stat y label value =
      let a = left + fromIntegral (pixelW label) * 2 + 8
          b = right - fromIntegral (pixelW value) * 2 - 8
      in Pictures
           [ ptext 2 faint label left y
           , ptextR 2 ink value right y
           , Color (hexA faint 0.7) (Pictures [ rect dx (y - 14) 2 2 | dx <- takeWhile (< b - 1) [a, a + 6 ..] ])
           ]
    nextStop
      | done =
          [ Bit (top - 196) (top - 210) (ptext 2 faint "DELIVERED TO" left (top - 196))
          , Bit (top - 218) (top - 232) (ptext 2 ink "THE LAMP, TOP FLOOR" left (top - 218)) ]
          ++ [ Bit (top - 240) (top - 254) (ptext 2 stampRed "+ HIDDEN STOP FOUND" left (top - 240)) | odCompleted o > visible ]
      | otherwise =
          let c = odCompleted o
          in [ Bit (top - 196) (top - 210) (ptext 2 faint "NEXT STOP" left (top - 196))
             , Bit (top - 216) (top - 238) (Pictures
                 [ Color (hex sun) (rect left (top - 238) 22 22)
                 , ptextC 2 ink (show (c + 1)) (left + 11) (top - 220)
                 , ptext 2 ink (map toUpper (lvName (levels !! c))) (left + 30) (top - 220)
                 ])
             ]

-- | Wick, the rooms, the lamp: black for cleared, yellow for the next stop.
route :: [Level] -> Float -> Float -> Int -> Picture
route levels cx cy cleared = Pictures $
  [ link i | i <- [0 .. stops] ]
  ++ [ wick (head xs) cy ]
  ++ [ room k | k <- [1 .. stops] ]
  ++ [ lighthouse (last xs) cy done
     , ptextC 2 faint "WICK" (head xs) (cy - 18)
     , ptextC 2 faint "LAMP" (last xs) (cy - 18) ]
  where
    stops = lastVisible levels + 1
    done = cleared >= stops
    left = cx - 126
    xs = [ left + 12 + 228 * fromIntegral i / fromIntegral (stops + 1) | i <- [0 .. stops + 1] ]
    link i =
      let a = xs !! i + 13
          b = xs !! (i + 1) - 13
      in if i < cleared || done
           then Color (hex ink) (rect a (cy - 1.5) (b - a) 3)
           else Color (if i == cleared then hex ink else hexA faint 0.6)
                  (Pictures [ rect dx (cy - 1.5) 3 3 | dx <- takeWhile (< b - 1) [a, a + 6 ..] ])
    room k =
      let x = xs !! k
          box
            | k <= cleared = Pictures [ Color (hex ink) (rect (x - 10.5) (cy - 10.5) 21 21), tick x cy rim ]
            | k == cleared + 1 = Pictures [ Color (hex ink) (rect (x - 12) (cy - 12) 24 24), Color (hex sun) (rect (x - 9) (cy - 9) 18 18) ]
            | otherwise = Pictures [ Color (hex faint) (rect (x - 10.5) (cy - 10.5) 21 21), Color (hex paper) (rect (x - 7.5) (cy - 7.5) 15 15) ]
      in Pictures [ box, ptextC 2 (if k <= cleared + 1 then ink else faint) (show k) x (cy - 18) ]

tick :: Float -> Float -> Int -> Picture
tick x y c = Color (hex c) (Pictures
  [ rect (x + i * 3 - 1.5) (y + j * 3 - 1.5) 3 3 | (i, j) <- [(-2, 0), (-1, -1), (0, -2), (1, -1), (2, 0), (3, 1)] ])

-- | The power cell she carries: the wick.
wick :: Float -> Float -> Picture
wick x y = Pictures
  [ Color (hex ink) (rect (x - 6) (y - 12) 12 21)
  , Color (hex ink) (rect (x - 3) (y + 9) 6 3)
  , Color (hex sun) (rect (x - 3) (y - 9) 6 15)
  , Color (hex 0xFFF1C2) (rect (x - 3) y 3 6)
  ]

-- | A little lighthouse, its lamp lit once the order is delivered.
lighthouse :: Float -> Float -> Bool -> Picture
lighthouse x y lit = Pictures $
  [ Color (hex (band i)) (rect (x - w / 2) (y - 15 + fromIntegral i * 3) w 3) | (i, w) <- zip [0 :: Int ..] [15, 15, 12, 12, 12, 9, 9] ]
  ++ [ Color (hex ink) (rect (x - 9) (y + 6) 18 3)
     , Color (hex (if lit then sun else ink)) (rect (x - 4.5) (y + 9) 9 6)
     , Color (hex (if lit then 0xA8362E else ink)) (Pictures [rect (x - 6) (y + 15) 12 3, rect (x - 3) (y + 18) 6 3]) ]
  ++ [ Color (hex sun) (Pictures [rect (x - 24) (y + 10.5) 12 3, rect (x + 12) (y + 10.5) 12 3, rect (x - 21) (y + 18) 9 3, rect (x + 12) (y + 18) 9 3]) | lit ]
  where
    band i
      | lit = if even (i `div` 2) then 0xD8493B else 0xE9DFCF
      | otherwise = if even (i `div` 2) then 0x9F98A6 else 0xC9C3CE

barcode :: Float -> Float -> Int -> Picture
barcode cx top seed = Color (hex ink) (Pictures [ rect (start + x) (top - 34) w 34 | (x, w) <- placed ])
  where
    bars = takeUntil 196 [ ( [2, 2, 4, 2, 6] !! floor (rnd seed i 7 * 5)
                           , [2, 4, 2, 4] !! floor (rnd seed i 9 * 4) ) | i <- [0 ..] ]
    takeUntil limit = go 0
      where
        go acc ((w, g) : rest) | acc < limit = (w, g) : go (acc + w + g) rest
        go _ _ = []
    total = sum [ w + g | (w, g) <- bars ]
    start = cx - total / 2
    placed = snd (foldl (\(x, acc) (w, g) -> (x + w + g, acc ++ [(x, w)])) (0, []) bars)

-- | The red stamp on a delivered order: worn rubber, a few specks of paper showing.
stamp :: Float -> Float -> String -> Int -> Picture
stamp x y date seed = Translate x y (Rotate (-8) (Pictures
  [ Color (hexA stampRed 0.88) (Pictures
      [ rect (negate w / 2) (negate h / 2) w 3, rect (negate w / 2) (h / 2 - 3) w 3
      , rect (negate w / 2) (negate h / 2) 3 h, rect (w / 2 - 3) (negate h / 2) 3 h
      , rect (negate w / 2 + 6) (negate h / 2 + 6) (w - 12) 2, rect (negate w / 2 + 6) (h / 2 - 8) (w - 12) 2 ])
  , Color (hexA stampRed 0.88) (Translate (negate (fromIntegral (pixelW "DELIVERED")) * 1.5) (h / 2 - 13) (pixelText 3 "DELIVERED"))
  , Color (hexA stampRed 0.88) (Translate (negate (fromIntegral (pixelW date))) (negate h / 2 + 24) (pixelText 2 date))
  , Color (hexA paper 0.75) (Pictures
      [ rect (negate w / 2 + w * rnd seed i 31) (negate h / 2 + h * rnd seed i 32) 3 3 | i <- [0 .. 25] ])
  ]))
  where
    w = fromIntegral (pixelW "DELIVERED") * 3 + 30
    h = 62

tape :: Float -> Float -> Picture
tape x top = Color (hexA tapeYellow 0.8) (Pictures $
  rect (x - 39) (top - 12) 78 24
  : concat [ [rect (x - 42) (top - 12 + i * 6) 3 3, rect (x + 39) (top - 9 + i * 6) 3 3] | i <- [0 .. 3] ])

-- | Holes along the line it tears on, the tear itself so far (0 to 1), and the
-- scissors at its start.
perforation :: Float -> Float -> Float -> Picture
perforation x y k = Pictures
  [ Color (hexA night 0.45) (Pictures [ rect dx (y - 1.5) 3 3 | dx <- [left + 6, left + 15 .. left + ticketW - 3] ])
  , Color (hex night) (Pictures
      [ rect dx (y - down) 3 (up + down)
      | (i, dx) <- zip [0 :: Int ..] (takeWhile (< left + ticketW * k) [left, left + 3 ..])
      , let up = 3 + 3 * fromIntegral (floor (rnd i 1 5 * 2) :: Int)
            down = 3 + 3 * fromIntegral (floor (rnd i 2 5 * 2) :: Int) ])
  , Pictures [ Color (hex night) (rect (sx + fromIntegral i * 3 - 1.5) (y + 9 - fromIntegral j * 3 - 1.5) 6 6) | (i, j) <- scissors ]
  , Color (hex (if k > 0 then sun else rim)) (Pictures [ rect (sx + fromIntegral i * 3) (y + 9 - fromIntegral j * 3) 3 3 | (i, j) <- scissors ])
  ]
  where
    left = x - ticketW / 2
    sx = left - 42
    scissors = [ (i, j) | (j, row) <- zip [0 :: Int ..] [".##.......", "#..#....##", ".##.#.##..", ".....#....", ".##.#.##..", "#..#....##", ".##......."]
                        , (i, ch) <- zip [0 :: Int ..] row, ch == '#' ]

-- | An order's ticket, its top edge at @top@: chosen or not, darkened by @darken@, and
-- torn so far along its perforation if it is being cancelled.
ticket :: [Level] -> Int -> Order -> Float -> Float -> Bool -> Float -> Maybe Float -> Picture
ticket levels n o x top chosen darken tear = Pictures $
  [ shadow (paperPart (x + d) (top - d) (top - ticketH - d) True True)
  , Color (hex paper) (paperPart x top (top - ticketH) True True) ]
  ++ [ p | Bit _ _ p <- printed levels n o x top (tear /= Nothing) ]
  ++ [ perforation x (top - tearBelow) k | Just k <- [tear] ]
  ++ [ Color (hexA night (0.55 * darken)) (paperPart x top (top - ticketH) True True) | darken > 0.01 ]
  ++ [ fadePicture (1 - 0.45 * darken) (tape x top) ]
  where
    d = if chosen then 12 else 6
    shadow = if chosen then Color (hex sun) else Color (hexA night 0.6)

-- | An empty slot: a dashed outline where a ticket would be, offering a new order.
emptyTicket :: Int -> Float -> Float -> Bool -> Picture
emptyTicket n x top chosen = Pictures
  [ Color (hexA night 0.45) (rect left bottom ticketW ticketH)
  , Color (fade a (hex edge)) (Pictures $
      concat [ [rect dx (top - 3) 6 3, rect dx bottom 6 3] | dx <- [left, left + 12 .. left + ticketW - 1] ]
      ++ concat [ [rect left dy 3 6, rect (left + ticketW - 3) dy 3 6] | dy <- [bottom, bottom + 12 .. top - 1] ])
  , fadePicture a (Pictures
      [ ptext 2 muted ("ORDER " ++ show n) (left + 18) (top - 22)
      , ptextR 2 muted "EMPTY" (left + ticketW - 18) (top - 22)
      , Color (hex (if chosen then sun else muted)) (Pictures [rect (x - 21) (top - 174) 42 6, rect (x - 3) (top - 192) 6 42])
      , ptextC 2 muted "Starts a new save," x (top - 278)
      , ptextC 2 muted "with the opening film." x (top - 302)
      ])
  , Color (hexA rim (if chosen then 1 else 0.55)) (Translate (x - fromIntegral (pixelW "NEW ORDER") * 2) (top - 226) (pixelText 4 "NEW ORDER"))
  ]
  where
    left = x - ticketW / 2
    bottom = top - ticketH
    edge = if chosen then sun else muted
    a = if chosen then 1 else 0.7

-- | A new order printing: the paper feeds down out of the printer, each row of
-- printing appearing as it passes the head.
printing :: [Level] -> Int -> Float -> Float -> Int -> Picture
printing levels n x top t = Pictures $
  [ Color (hex sun) (paperPart (x + 12) (top - 12) (cut - 12) True True)
  , Color (hex paper) (paperPart x top cut True True) ]
  ++ [ p | Bit _ lo p <- printed levels n newOrder x top False, lo >= cut ]
  ++ [ Color (hex ink) (rect (x - 168) (top + 6) 336 21)
     , Color (hex 0x2A2833) (rect (x - 168) (top + 6) 336 6)
     , Color (hexA rim 0.35) (rect (x - 156) (top + 9) 312 3)
     , Color (hex sun) (rect (x + 138) (top + 15) 9 6) ]
  where
    p' = fromIntegral (floor (clamp01 (fromIntegral t / fromIntegral printTicks) * 30) :: Int) / 30
    cut = top - max 30 (ticketH * p')

-- | An order torn off: the part below the perforation drops away turning, and what
-- is left fades.
tearing :: [Level] -> Int -> Order -> Float -> Float -> Int -> Picture
tearing levels n o x top t = Pictures
  [ fadePicture upperK (Pictures $
      [ Color (hex sun) (paperPart (x + 12) (top - 12) (y - 12) True False)
      , Color (hex paper) (paperPart x top y True False) ]
      ++ [ p | Bit hi _ p <- bits, hi > y ]
      ++ [ perforation x y 1, tape x top ])
  , fadePicture (1 - f) (Translate x (y - 10 * f - 700 * f * f) (Rotate (10 * f) (Translate (negate x) (negate y) (Pictures $
      [ Color (hex sun) (paperPart (x + 12) (y - 12) (top - ticketH - 12) False True)
      , Color (hex paper) (paperPart x y (top - ticketH) False True) ]
      ++ [ p | Bit hi _ p <- bits, hi <= y ]))))
  ]
  where
    y = top - tearBelow
    f = clamp01 (fromIntegral t / fromIntegral tearTicks)
    upperK = 1 - smooth (fromIntegral (t - 30) / 20)
    bits = printed levels n o x top True

-- | The time on the road, from ticks: hours, minutes and seconds.
clockOf :: Int -> String
clockOf ticks = show h ++ ":" ++ two m ++ ":" ++ two s
  where
    total = ticks `div` ticksPerSecond
    (h, rest) = total `divMod` 3600
    (m, s) = rest `divMod` 60
    two v = (if v < 10 then "0" else "") ++ show v

dayText :: Stamp -> String
dayText st = show (smDay st) ++ " " ++ (months !! max 0 (min 11 (smMonth st - 1)))
  where months = ["JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"]

stampText :: Stamp -> String
stampText st = dayText st ++ "  " ++ two (smHour st) ++ ":" ++ two (smMinute st)
  where two v = (if v < 10 then "0" else "") ++ show v

-- The hall ---------------------------------------------------------------------------

-- | Over the hall: which order this is, pinned top left, and the slip it prints
-- when an order is saved after a room is cleared, for @slipAge@ ticks so far. A
-- slip that could not be saved says so, in red.
hallOverlay :: [Level] -> Int -> Maybe (Int, Bool, Order) -> Picture
hallOverlay levels n slip = Pictures $
  [ Color (hex sun) (rect (-604) 340 tagW 32)
  , Color (hex rim) (rect (-610) 346 tagW 32)
  , ptext 2 ink tagText (-598) 369
  , Color (hex 0x98ADB1) (Translate (-610 + tagW + 12) 356 (textL 11 "Mei's delivery"))
  ]
  ++ [ savedSlip levels n o ok (400 + 108 - 108 * k) | Just (age, ok, o) <- [slip], let k = slipK age, k > 0.01 ]
  where
    tagText = "ORDER " ++ show n
    tagW = fromIntegral (pixelW tagText) * 2 + 24
    slipK age
      | age < 14 = smooth (fromIntegral age / 14)
      | age < 170 = 1
      | otherwise = 1 - smooth (fromIntegral (age - 170) / 14)

savedSlip :: [Level] -> Int -> Order -> Bool -> Float -> Picture
savedSlip levels n o ok top = Pictures $
  [ Color (hexA night 0.6) (rect (left + 6) (top - h) w (h - 6))
  , Color (hex (if ok then paper else 0xF3D9D2)) (rect left (top - h + 6) w (h - 6))
  , Color (hex (if ok then paper else 0xF3D9D2)) (Pictures [ pixelPoly [(l, top - h + 6), (l + tw / 2, top - h), (l + tw, top - h + 6)] | l <- teeth ])
  ]
  ++ (if ok
        then [ Color (hex ink) (rect (left + 14) (top - 52) 24 24), tick (left + 26) (top - 40) sun
             , ptext 2 ink ("ORDER " ++ show n ++ " SAVED") (left + 48) (top - 30)
             , ptext 2 faint (show (min visible (odCompleted o)) ++ " OF " ++ show visible ++ " ROOMS  " ++ clockOf (odTicks o)) (left + 48) (top - 54) ]
        else [ Color (hex stampRed) (rect (left + 14) (top - 52) 24 24)
             , Color (hex paper) (Pictures [rect (left + 24) (top - 46) 4 4, rect (left + 24) (top - 36) 4 10])
             , ptext 2 stampRed "COULD NOT SAVE" (left + 48) (top - 30)
             , ptext 2 faint "Will keep trying." (left + 48) (top - 54) ])
  ++ [ Color (hexA faint 0.5) (Pictures [ rect dx (top - 76) 6 2 | dx <- [left + 14, left + 24 .. left + w - 20] ])
     , ptext 2 faint "CAPE COURIERS" (left + 14) (top - 82) ]
  where
    w = 300
    h = 108
    left = 470 - w / 2
    n' = 20 :: Int
    tw = w / fromIntegral n'
    teeth = [ left + fromIntegral i * tw | i <- [0 .. n' - 1] ]
    visible = lastVisible levels + 1
