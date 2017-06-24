module View exposing (view)

import Helpers exposing (..)
import Model exposing (..)
import ModelHelpers exposing (..)
import Line exposing (..)
import ModelHelpers exposing (lineFromBall)
import Html exposing (Html, div, p, text, a, button, h3, label, input, ul, fieldset, section, span)
import Html.Attributes exposing (style, href, target, type_, value, checked)
import Html.Events exposing (onClick)
import Svg exposing (svg, rect, image, g, circle, line, text_, marker, path, defs)
import Svg.Attributes exposing (id, x, y, viewBox, fill, width, height,
    cx, cy, r, color, stroke, strokeWidth, fontSize, opacity)
import Set exposing (toList)

----------
-- VIEW --
----------


viewSettings = {
    paddles = {
        width = toString (paddleWidth)
        , height = toString (paddleHeight)
        , fill = "#050500"
        , margin = 10
        }

    -- Game box, built out of SVG nodes.
    , viewBoxSize = (fieldWidth, fieldHeight)
    , viewBoxStr = "0 0 "
        ++ (toString fieldWidth) ++ " "
        ++ (toString fieldHeight) --1000 1000"
    , svgWidth = "600px"
    , bgfill = "grey"
    , paddlefill = "#FFFFFF"
    , ballfill = "#FFFFFF"
    , containerStyle = [ ( "margin", "50px 50px" ), ( "text-align", "center" ) ]
    }

svgCentered =
    [
        ("width", viewSettings.svgWidth),
        ("margin", "0 auto")
    ]



view : Model -> Html Msg
view model =
    let
        w = (toString <| fst viewSettings.viewBoxSize) ++ "px"
        h = (toString <| snd viewSettings.viewBoxSize) ++ "px"
    in
        div [
            Svg.Attributes.width w
            , Svg.Attributes.height h
            , style viewSettings.containerStyle
        ] [
            scoreboardView model
            , gameView model
            , playersChooseView model
            , debugView model
            , settingsview model
        ]


keyToStr : Char -> String
keyToStr char =
    let
        s = toString char
    in
    if char == playerCmdKeys.p1Up then
        s ++ " " ++ "P1 Up"
    else if char == playerCmdKeys.p1Down then
        s ++ " " ++ "P1 Down"
    else if char == playerCmdKeys.p2Up then
        s ++ " " ++ "P2 Up"
    else if char == playerCmdKeys.p2Down then
        s ++ " " ++ "P2 Down"
    else
        s ++ " " ++ "Unknown"


debugView : Model -> Html Msg
debugView model =
    let
        vx = round model.ball.velocity.vx
        vy = round model.ball.velocity.vy
        x = round model.ball.pos.x
        y = round model.ball.pos.y
        intercepts = (round <| fst model.intercepts, round <| snd model.intercepts)
        lp = round2 3 model.lpaddle.y
        rp = round2 3 model.rpaddle.y
    in
    div [
        style <| svgCentered ++ [("text-align", "left")]
        ]
    [
        simpleTextDiv "Pos: " <| "{ x=" ++ (toString x) ++ " y=" ++ (toString y) ++ "}"
        , simpleTextDiv "Velocity: " <| "{ vx=" ++ (toString vx) ++ " vy=" ++ (toString vy) ++ "}"
        , ballDetailsView model.ball
        , div [] [
            text <| "Raw Keys: " ++
                if Set.size model.keys > 0 then
                    (toString <| Set.toList model.keys)
                else
                    ""
        ]
        , simpleTextDiv "Intercepts: " (toString intercepts)
        , simpleTextDiv "Paddles: (" <| (toString lp) ++ "," ++ (toString rp) ++ ")"
    ]

simpleTextDiv : String -> String -> Html Msg
simpleTextDiv prefix str =
    div [] [ text <| prefix ++ str]


round2 : Float-> Float -> Float
round2 places val =
    (toFloat <| round <| val * (10^places)) / (10^places)


ballDetailsView : Ball -> Html Msg
ballDetailsView ball =
    let
        x = toString <| round ball.pos.x
        y = toString <| round ball.pos.y
        vx = toString <| round ball.velocity.vx
        vy = toString <| round ball.velocity.vy
        ballline = lineFromBall ball
        origdc = case ballline.dc of
            DC slope -> slope
            DCVer _ -> 0

        bl = Line (DC (round2 3 origdc)) (round2 3 ballline.yintercept)
    in
        simpleTextDiv "Ball Line: " (toString bl)

gameView : Model -> Html Msg
gameView model =
    div [ style [("background-color", "beige")] ]
    [
        svg [
                viewBox viewSettings.viewBoxStr
                , Svg.Attributes.width viewSettings.svgWidth
            ] [
                backgroundView
                , paddlesView model.lpaddle model.rpaddle
                , ballView model.ball
                --, scoreboardView model -- model.score
            ]
    ]

ballView : Ball -> Html Msg
ballView {pos} =
    circle [
        cx <| toString pos.x
        , cy <| toString pos.y
        , r <| toString ballRadius
        , fill viewSettings.ballfill
        ] []


paddlesView : Paddle -> Paddle -> Html msg
paddlesView lpaddle rpaddle =
    g [] [
        paddleView viewSettings.paddles.margin lpaddle
        , paddleView rpX rpaddle
        ]


paddleView : Int-> Paddle -> Html msg
paddleView xcoord paddle =
    rect [
        x <| toString xcoord
        , y <| toString (paddle.y - paddleHalfHeight)
        , Svg.Attributes.width viewSettings.paddles.width
        , Svg.Attributes.height viewSettings.paddles.height
        , fill viewSettings.paddles.fill
        ] []


styles = {
    scoreboardContainer=[("background-color","green")]
    ,scoreboardHeaders=[("display","inline"),("color","white"),("margin","20px")]
    }

scoreboardView : Model -> Html msg
scoreboardView model =
    let
        score = model.score
        (p1score, p2score) = case score of (Score s1 s2) -> (s1, s2)
    in
        div
            [ style styles.scoreboardContainer ]
            [
                h3
                    [ style styles.scoreboardHeaders ]
                    [ text ( "Player1 (" ++ (toString model.player1type) ++ "): " ++ toString p1score ) ]
                , h3
                    [ style styles.scoreboardHeaders ]
                    [ text ( "Player2 (" ++ (toString model.player2type) ++ "): " ++ toString p2score ) ]
            ]


backgroundView : Html msg
backgroundView =
    let
        w = toString fieldWidth
        h = toString fieldHeight
    in
    rect [
        x "0"
        , y "0"
        , Svg.Attributes.width w
        , Svg.Attributes.height h
        , fill viewSettings.bgfill
        ] []



settingsview : Model -> Html Msg
settingsview model =
    case model.showSettings of
        True ->
            let
                reflectionMode = model.reflectionMode
            in
                div [ style [ ("border", "3px solid black") ] ]
                [
                    viewPicker
                    [ ( "Simple Reflection",
                        SetReflectionMode Simple,
                        reflectionMode == Simple)
                    , ( "Reflect relative to paddle center",
                        SetReflectionMode PaddleCenterRelative,
                        reflectionMode == PaddleCenterRelative)
                    ]
                  , button [ onClick ToggleShowSettings ] [ text "hide settings" ]
                    -- , section [] [ text model.content ]
                ]
        False ->
            div [] [
                button [ onClick ToggleShowSettings ] [ text "settings" ]
            ]

playerChooseButtonStyle =
    [("margin","10px")]

playersChooseView : Model -> Html Msg
playersChooseView model =
    div [] [
        button [ onClick (ToggleActivePlayer Player1), style playerChooseButtonStyle  ] [
            text <| "Switch Player 1 to " ++ (toString <| togglePlayerType model.player1type)
        ]
        ,
        button [ onClick (ToggleActivePlayer Player2), style playerChooseButtonStyle  ] [
            text <| "Switch Player 2 to " ++ (toString <| togglePlayerType model.player2type)
        ]
    ]

viewPicker : List (String, msg, Bool) -> Html msg
viewPicker options =
    fieldset [ style svgCentered ] (List.map radio options)


radio : (String, msg, Bool) -> Html msg
radio (name, msg, ischkd) =
  label []
    [ input [ type_ "radio", onClick msg, checked ischkd ] []
    , text name
    ]


