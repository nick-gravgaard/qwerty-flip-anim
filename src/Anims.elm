module Anims exposing (main)

import Animation exposing (..)
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (..)
import Html.Attributes as HA
import Json.Decode exposing (Value)
import List.Nonempty as NE exposing (Nonempty)
import Random exposing (Generator)
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes as SA

import Ease


type AnimType
  = Flip
  | Spin
  | Twist

type alias Key =
  { char : Char
  , x : Int
  , y : Int
  , colour : String
  }

config =
  { keySize = 48 * 1
  , keyCharSize = 26 * 1
  , borderSize = 4 * 1
  , cornerRadius = 4 * 1
  , paddingSize = 20 * 1
  , yKeyCharFudge = 2.5 * 1
  , yNameTextFudge = 6.0 * 1
  , titleFontSize = 20 * 1
  , titleFont = "\"Linotte-Regular\""
  , keyFont = "\"Open Gorton\""
  , keyCharRows =
    NE.Nonempty
    ( NE.Nonempty 'Q' ['W', 'E', 'R', 'T', 'Y', 'U', 'I', 'O', 'P'] )
    [ NE.Nonempty 'A' ['S', 'D', 'F', 'G', 'H', 'J', 'K', 'L', ';']
    , NE.Nonempty 'Z' ['X', 'C', 'V', 'B', 'N', 'M', ',', '.', '/']
    ]

  , flipGoodKeys = Set.fromList ['E', 'R', 'T', 'U', 'I', 'O', 'P']
  , flipBadKeys  = Set.fromList ['D', 'F', 'G', 'J', 'K', 'L', ';']

  , spinGoodKeys = Set.fromList ['E', 'R', 'T', 'U', 'I', 'O'     ]
  , spinBadKeys  = Set.fromList ['D', 'F', 'G', 'J', 'K',      ';']

  , twistGoodKeys = Set.fromList ['E', 'R', 'T',      'I', 'O', 'P', 'N']
  , twistBadKeys  = Set.fromList ['D', 'F', 'G', 'J', 'K', 'L', ';']

  , rowOffsets =
      NE.Nonempty
      0
      [ 0.25
      , 0.75
      ]

  , goodCol =    hsl 90 50 33
  , badCol =     hsl  0 50 33
  , neutralCol = hsl  0  0 33

  , goodHighlightedCol    = hsl 90 100 75
  , badHighlightedCol     = hsl  0 100 75
  , neutralHighlightedCol = hsl  0   0 75

  }


drawKey : Key -> Float -> Svg Float
drawKey key scale =
  let
    keySize = config.keySize * scale
    halfKeySize = (config.keySize * scale) / 2
    keyCharSize = config.keyCharSize * scale
    borderSize = config.borderSize * scale
    cornerRadius = config.cornerRadius * scale
    yKeyCharFudge = config.yKeyCharFudge * scale
  in
  Svg.g
    [ SA.id (String.fromChar key.char)
    ]
    [ Svg.rect
      [ SA.fill key.colour
      , SA.x (String.fromInt key.x)
      , SA.y (String.fromInt key.y)
      , SA.width (String.fromInt (round keySize))
      , SA.height (String.fromInt (round keySize))
      , SA.stroke "black"
      , SA.strokeWidth (String.fromInt (round borderSize))
      , SA.rx (String.fromInt (round cornerRadius))
      , SA.ry (String.fromInt (round cornerRadius))
      ]
      []
    , Svg.text_
      [ SA.x (String.fromFloat ((toFloat key.x) + halfKeySize))
      , SA.y (String.fromFloat ((toFloat key.y) + halfKeySize + yKeyCharFudge))
      , SA.dominantBaseline "middle"
      , SA.textAnchor "middle"
      , SA.fontFamily config.keyFont
      , SA.fontSize (String.fromInt (round keyCharSize))
      ]
      [ Svg.text (String.fromChar key.char) ]
    ]


makeKey : AnimType -> Clock -> Char -> Int -> Int -> Float -> Int -> Int -> Float -> Key
makeKey animType clock char left top xOffset colNum rowNum scale =
  case animType of
    Flip ->
      makeFlipKey clock char left top xOffset colNum rowNum scale
    Twist ->
      makeTwistKey clock char left top xOffset colNum rowNum scale
    Spin ->
      makeSpinKey clock char left top xOffset colNum rowNum scale


hsl : Int -> Int -> Int -> String
hsl h s l =
  "hsl(" ++ (String.fromInt h) ++ ", " ++ (String.fromInt s) ++ "%, " ++ (String.fromInt l) ++ "%)"

makeFlipKey : Clock -> Char -> Int -> Int -> Float -> Int -> Int -> Float -> Key
makeFlipKey clock char left top xOffset colNum rowNum scale =
  let
    makeAnim dest =
      animation 0
        |> from 0
        |> to dest
        |> duration 2000
        |> delay 1
        |> ease Ease.inOutCubic
    goodXAnim = makeAnim ((NE.get 1 config.rowOffsets) * config.keySize * scale)
    goodYAnim = makeAnim (config.keySize * scale)
    badXAnim = makeAnim -((NE.get 1 config.rowOffsets) * config.keySize * scale)
    badYAnim = makeAnim -(config.keySize * scale)

    (xAnimDelta, yAnimDelta, colour) =
      if Set.member char config.flipGoodKeys then
        (animate clock goodXAnim, animate clock goodYAnim, config.goodHighlightedCol)
      else
        if Set.member char config.flipBadKeys then
          (animate clock badXAnim, animate clock badYAnim, config.badHighlightedCol)
        else
          (0.0, 0.0, config.neutralHighlightedCol)
    x = left + (round (((xOffset + (toFloat colNum)) * config.keySize) + xAnimDelta))
    y = top + (round (((toFloat rowNum) * config.keySize) + yAnimDelta))
  in
  Key char x y colour


makeSpinKey : Clock -> Char -> Int -> Int -> Float -> Int -> Int -> Float -> Key
makeSpinKey clock char left top xOffset colNum rowNum scale =
  let
    makeAnim dest =
      animation 0
        |> from 0
        |> to dest
        |> duration 2000
        |> delay 1
        |> ease Ease.inOutCubic
    row1Offset = NE.get 1 config.rowOffsets
    row2Offset = NE.get 2 config.rowOffsets
    goodXAnim = makeAnim (row1Offset * config.keySize * scale)
    goodYAnim = makeAnim (config.keySize * scale)
    badXAnim = makeAnim -(row1Offset * config.keySize * scale)
    badYAnim = makeAnim -(config.keySize * scale)

    lXAnim = makeAnim  (config.keySize * scale)
    pXAnim = makeAnim -(config.keySize * scale)

    (xAnimDelta, yAnimDelta, colour) =
      if char == 'L' then
        (animate clock lXAnim, 0.0, config.goodHighlightedCol)
      else
        if char == 'P' then
          (animate clock pXAnim, 0.0, config.badHighlightedCol)
        else
          if Set.member char config.spinGoodKeys then
            (animate clock goodXAnim, animate clock goodYAnim, config.goodCol)
          else
            if Set.member char config.spinBadKeys then
              (animate clock badXAnim, animate clock badYAnim, config.badCol)
            else
              (0.0, 0.0, config.neutralCol)
    x = left + (round (((xOffset + (toFloat colNum)) * config.keySize * scale) + xAnimDelta))
    y = top + (round (((toFloat rowNum) * config.keySize * scale) + yAnimDelta))
  in
  Key char x y colour


makeTwistKey : Clock -> Char -> Int -> Int -> Float -> Int -> Int -> Float -> Key
makeTwistKey clock char left top xOffset colNum rowNum scale =
  let
    makeAnim dest =
      animation 0
        |> from 0
        |> to dest
        |> duration 2000
        |> delay 1
        |> ease Ease.inOutCubic
    row1Offset = NE.get 1 config.rowOffsets
    row2Offset = NE.get 2 config.rowOffsets
    goodXAnim = makeAnim (row1Offset * config.keySize * scale)
    goodYAnim = makeAnim (config.keySize * scale)
    badXAnim = makeAnim -(row1Offset * config.keySize * scale)
    badYAnim = makeAnim -(config.keySize * scale)

    nXAnim = makeAnim ((row2Offset - row1Offset) * config.keySize * scale)
    nYAnim = makeAnim -(config.keySize * scale)
    jXAnim = makeAnim -((row2Offset - row1Offset) * config.keySize * scale)
    jYAnim = makeAnim (config.keySize  * scale)

    (xAnimDelta, yAnimDelta, colour) =
      if Set.member char (Set.remove 'N' config.twistGoodKeys) then
        (animate clock goodXAnim, animate clock goodYAnim, config.goodCol)
      else
        if char == 'N' then
          (animate clock nXAnim, animate clock nYAnim, config.goodHighlightedCol)
        else
          if Set.member char (Set.remove 'J' config.twistBadKeys) then
            (animate clock badXAnim, animate clock badYAnim, config.badCol)
          else
            if char == 'J' then
              (animate clock jXAnim, animate clock jYAnim, config.badHighlightedCol)
            else
              (0.0, 0.0, config.neutralCol)
    x = left + (round (((xOffset + (toFloat colNum)) * config.keySize * scale) + xAnimDelta))
    y = top + (round (((toFloat rowNum) * config.keySize * scale) + yAnimDelta))
  in
  Key char x y colour


drawRow : AnimType -> Clock -> Int -> Int -> Int -> Nonempty Char -> Float -> Nonempty (Char, Svg Float)
drawRow animType clock left top rowNum keyCharRow scale =
  let
    rowOffset = NE.get rowNum config.rowOffsets
  in
  NE.indexedMap (\colNum keyChar -> (keyChar, drawKey (makeKey animType clock keyChar left top rowOffset colNum rowNum scale) scale)) keyCharRow


drawKeyboard : AnimType -> Clock -> Int -> Int -> Set Char -> Set Char -> Float -> List (Svg Float)
drawKeyboard animType clock left top goodKeys badKeys scale =
  let
    keyboardWidth = 10
    keyboardHeight = 3
    row2Offset = NE.get 2 config.rowOffsets
    goodAndBadKeys = Set.union goodKeys badKeys
    charSvgs = NE.toList (NE.concat (NE.indexedMap (\rowNum keyCharRow -> drawRow animType clock left top rowNum keyCharRow scale) config.keyCharRows))
    topKeys = List.map (\(char, svg) -> svg) (List.filter (\(char, svg) -> Set.member char goodKeys) charSvgs)
    bottomKeys = List.map (\(char, svg) -> svg) (List.filter (\(char, svg) -> Set.member char badKeys) charSvgs)
    middleKeys = List.map (\(char, svg) -> svg) (List.filter (\(char, svg) -> not (Set.member char goodAndBadKeys)) charSvgs)
    orderedKeys = bottomKeys ++ middleKeys ++ topKeys
    name =
      case animType of
        Flip -> "FLIP"
        Twist -> "TWIST"
        Spin -> "SPIN"
    nameText =
      Svg.text_
        [ SA.x (String.fromFloat ((toFloat left) + ((((toFloat keyboardWidth) + row2Offset) * config.keySize) - config.borderSize) * scale))
        , SA.y (String.fromFloat (((toFloat top) + (keyboardHeight * config.keySize) * scale  ) + (config.yNameTextFudge * scale)))
        , SA.dominantBaseline "hanging"
        , SA.textAnchor "end"
        , SA.fontFamily config.titleFont
        , SA.fontSize (String.fromInt config.titleFontSize)
        , SA.fill "#fff"
        ]
        [ Svg.text name ]
  in
  orderedKeys ++ [ nameText ]


view : Clock -> Html Float
view clock =
  let
    keyboardWidth = 10
    width =
      (keyboardWidth + 1) * config.keySize
    paddingSize = (String.fromInt config.paddingSize) ++ "px"
    padding = paddingSize ++ " " ++ paddingSize ++ " 0 " ++ paddingSize
  in
  Html.div
    []
    [ div
      [ HA.id "svg-main-container"
      , HA.style "background-color" "#2a2a2a"
      , HA.style "padding" padding
      , HA.style "color" "#fff"
      , HA.style "width" ((String.fromInt width) ++ "px")
      ]
      [ viewMainSvg clock
      ]
    , div
      [ HA.id "svg-extras-container"
      , HA.style "background-color" "#2a2a2a"
      , HA.style "padding" padding
      , HA.style "color" "#fff"
      , HA.style "width" ((String.fromInt width) ++ "px")
      ]
      [ viewExtrasSvg clock
      ]
    ]


viewMainSvg : Clock -> Svg Float
viewMainSvg clock =
  let
    keyboardWidth = 10
    width =
      (keyboardWidth + 3) * config.keySize
    height =
      config.borderSize + (3 * config.keySize) + config.titleFontSize
  in
  Svg.svg
    [ SA.width (String.fromInt width)
    , SA.height (String.fromInt height)
    ]
    (
      (drawKeyboard Flip clock config.borderSize config.borderSize config.flipGoodKeys config.flipBadKeys 1.0)
    )


viewExtrasSvg : Clock -> Svg Float
viewExtrasSvg clock =
  let
    keyboardWidth = 10
    keyboardHeight = 3
    width =
      (keyboardWidth + 3) * config.keySize
    height =
      round ((config.borderSize * 0.5) + (3 * (config.keySize * 0.5))) + config.titleFontSize
  in
  Svg.svg
    [ SA.width (String.fromInt width)
    , SA.height (String.fromInt height)
    ]
    (
      (drawKeyboard Spin clock (round (config.borderSize * 0.5)) (round (config.borderSize * 0.5)) config.spinGoodKeys config.spinBadKeys 0.5)
      ++ (drawKeyboard Twist clock ((round ((keyboardWidth + 1) * config.keySize * 0.5)) + config.borderSize) (round (config.borderSize * 0.5)) config.twistGoodKeys config.twistBadKeys 0.5)
    )


main : Program Value Clock Float
main =
  Browser.element
    { init = always ( 0, Cmd.none )
    , update = \dt clock -> ( clock + dt, Cmd.none )
    , subscriptions = \clock -> onAnimationFrameDelta identity
    , view = view
    }
