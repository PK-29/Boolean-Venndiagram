module Main exposing (main)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import BoolDiagram




main =
    gameApp Tick
        { model = init -- init is the value in the field model
        , title = "Boolean Diagrams"
        , view = view
        , update = update
        }


type Pages
    = BoolDiagram


init =
    { page = BoolDiagram
    , model1 = BoolDiagram.init

    }



type Msg m1 
    = Tick Float GetKeyState
    | Msg1 (BoolDiagram.Msg m1)


view model =
    collage 600 380 <|
        case model.page of
            BoolDiagram ->
                List.map (map Msg1) (BoolDiagram.view model.model1)

          


update msg model =
    case msg of
        _ ->
            case model.page of
                BoolDiagram ->
                    case msg of
                        Tick f g ->
                            { model | model1 = BoolDiagram.update (BoolDiagram.Tick f g) model.model1 }

                        Msg1 m1 ->
                            { model | model1 = BoolDiagram.update m1 model.model1 }

                        
