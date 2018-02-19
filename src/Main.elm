module Main exposing (..)

import Html exposing (Html, text, div, h1, img)
-- import Html.Attributes exposing ()

import Svg exposing (Svg, rect, svg)
import Svg.Attributes exposing (width, height, rx, ry, x, y, viewBox, fill)

import Time exposing (Time, millisecond)

import Math.Vector2 as V2 exposing (Vec2, vec2)
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Math.Matrix4 as M exposing (Mat4)

to3 : Vec2 -> Vec3
to3 v = vec3 (V2.getX v) (V2.getY v) 0 

to2 : Vec3 -> Vec2
to2 v = vec2 (V3.getX v) (V3.getY v)

-- ---- MODEL ----


type alias Model =
    { t : Time }


init : ( Model, Cmd Msg )
init =
    ( {t = 0}, Cmd.none )


---- UPDATE ----


type Msg
    = Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick t -> ( { model | t = t }, Cmd.none )

---- VIEW ----
roundedRect : Int -> Int -> Int -> Int -> Svg Msg
roundedRect x_ y_ w h = rect
    [ x (toString x_)
    , y (toString y_)
    , width (toString w)
    , height (toString h)
    , rx "15"
    , ry "15"
    , fill "rgba(0,0,0,0.4)"
    ] []


pts : List Vec2
pts =
    [ vec2 0.2 0.3
    , vec2 0.4 0.9
    , vec2 0.6 -0.7
    , vec2 -0.3 -0.2
    , vec2 0.3 -0.2
    , vec2 0.3 0.2
    , vec2 -0.3 0.2
    , vec2 0.0 0.0
    ]

identity : Mat4
identity = M.identity

scale : Float -> Mat4 -> Mat4
scale s = M.scale (vec3 s s s) 

translate : Vec2 -> Mat4 -> Mat4
translate xy = M.translate3 (V2.getX xy) (V2.getY xy) 0

rotate : Float -> Mat4 -> Mat4
rotate r = M.rotate r (vec3 0 0 1)

transform2 : Mat4 -> Vec2 -> Vec2
transform2 t x = to2 <| M.transform t (to3 x)

mat : Float -> Mat4 -> Mat4
mat t x = let xwobb = (1 + Basics.sin (t * 45)) / 2 in
    rotate t <|
    scale (1.1 + Basics.sin t) <|
    -- translate (vec2 xwobb 0) <|
    x

viewport : Int -> Int -> Mat4 -> Mat4
viewport w_ h_ x =
    let
        w = toFloat w_
        h = toFloat h_
    in
        scale (if w > h then w else h) <|
        x

dot : Vec2 -> Svg Msg
dot p =
    let r = 6
        rr = r + r
    in roundedRect
        (Basics.floor <| V2.getX p - r)
        (Basics.floor <| V2.getY p - r)
        rr rr


f t = viewport 100 100 <| mat t <| identity

view : Model -> Html Msg
view model =
    let k = model.t / (10 ^ 3)
    in 
        div []
            [ h1 [] [ text "Your Elm App is working!" ]
            , svg [ width "300", height "300", viewBox "-100 -100 200 200" ]
                <| List.map (\p -> dot (transform2 (f k) p)) pts
                ++ [ roundedRect -100 -100 200 200 ]
            ]


---- PROGRAM ----
subscriptions : Model -> Sub Msg
subscriptions model = Time.every (10 * millisecond) Tick

main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions -- always Sub.none
        }
