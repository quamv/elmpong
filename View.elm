module View exposing (view)

import Helpers exposing (..)
import Model exposing (..)
import Html exposing (Html, div, p, text, a, button, h3, label, input, ul, fieldset, section)
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
            , settingsview model
            , debugView model
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
    in
    div [ style [("text-align", "left"), ("justify-content", "left") ] ]
    [
        div [] [
            text <|
                "Velocity: { vx=" ++ (toString vx)
                ++ " vy=" ++ (toString vy) ++ "}"
        ]
        ,
        div [] [
            text <| "Raw Keys: " ++
                if Set.size model.keys > 0 then
                    (toString <| Set.toList model.keys)
                else
                    ""
        ]
    ]


gameView : Model -> Html Msg
gameView model =
    svg [
            viewBox viewSettings.viewBoxStr
            , Svg.Attributes.width viewSettings.svgWidth
        ] [
            backgroundView
            , paddlesView model.lpaddle model.rpaddle
            , ballView model.ball
            --, scoreboardView model -- model.score
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
                    [ text ( "Player2 (" ++ (toString model.player1type) ++ "): " ++ toString p2score ) ]
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
                div []
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


viewPicker : List (String, msg, Bool) -> Html msg
viewPicker options =
    fieldset [] (List.map radio options)


radio : (String, msg, Bool) -> Html msg
radio (name, msg, ischkd) =
  label []
    [ input [ type_ "radio", onClick msg, checked ischkd ] []
    , text name
    ]

