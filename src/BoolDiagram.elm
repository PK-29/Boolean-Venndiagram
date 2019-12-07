module BoolDiagram exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)



init =
    { time = 0
    , notify = NotifyTap
    , uInput1 = A
    , uInput2 = B
    , uInput3 = C
    , uInputOp1 = AND
    , uInputOp2 = AND
    }




update msg model =
    case msg of
        Tick t _ ->
                { model
                    | time = t
                }
        
        Notif notif ->
            { model | notify = notif }

        UInput1 ->
            { model | uInput1 = cycleInputs model.uInput1 }

        UInput1Reverse ->
            { model | uInput1 = cycleInputsReverse model.uInput1 }

        UInput2 ->
            { model | uInput2 = cycleInputs model.uInput2 }

        UInput2Reverse ->
            { model | uInput2 = cycleInputsReverse model.uInput2 }

        UInput3 ->
            { model | uInput3 = cycleInputs model.uInput3 }

        UInput3Reverse ->
            { model | uInput3 = cycleInputsReverse model.uInput3 }

        UInputOp1 ->
            { model | uInputOp1 = cycleOperations model.uInputOp1 }
        
        UInputOp1Reverse ->
            { model | uInputOp1 = cycleOperationsReverse model.uInputOp1}

        UInputOp2 ->
            { model | uInputOp2 = cycleOperations model.uInputOp2 }
        
        UInputOp2Reverse ->
            { model | uInputOp2 = cycleOperationsReverse model.uInputOp2}
        



posA = (80, 40)
posB = (135,40)
posC = (110,90)
pPos = (-240,0)

view model =
    let
        everything = 
            group [
                displayPrecedence model
                ,displayResult model 
                , roundedRect 500 220 5
                    |> outlined (solid 2) black
                    |> move (-10,70)
                
                , text "Equation:"
                    |> size 12
                    |> outlined (solid 1) black
                    |> move (-220,-100)
                , text "Boolean Operations"
                    |> size 14
                    |> centered
                    |> outlined (solid 1) orange
                    |> move (-10,160)
                    
            ]

        input1Cycling = 
            --Display all the text and arrows and their correponding fucntions to transition on click
            group
                [ text (inputText model.uInput1) |>centered |> size 10 |> filled black |> move ( -146, -103 )
                , upArrow |> move (-146, -85) |> notifyTap UInput1Reverse  
                , downArrow |> move (-146, -115) |> notifyTap UInput1 
                , text (operationText model.uInputOp1) |>centered |> size 10 |> filled black |> move ( -111, -103 )
                , upArrow |> move (-110, -85) |> notifyTap UInputOp1Reverse
                , downArrow |> move (-110, -115) |> notifyTap  UInputOp1

                , text (inputText model.uInput2) |>centered |> size 10 |> filled black |> move ( -66, -103 )
                , upArrow |> move (-66, -85) |> notifyTap UInput2Reverse
                , downArrow |> move (-66, -115) |> notifyTap  UInput2

                , text (operationText model.uInputOp2) |>centered |> size 10 |> filled black |> move ( -31, -103 )
                , upArrow |> move (-30, -85) |> notifyTap UInputOp2Reverse
                , downArrow |> move (-30, -115) |> notifyTap  UInputOp2

                , text (inputText model.uInput3) |>centered |> size 10 |> filled black |> move ( 13, -103 )
                , upArrow |> move (14, -85) |> notifyTap UInput3Reverse
                , downArrow |> move (14, -115) |> notifyTap UInput3
                ]

    in
        [everything |> move (0,0)
        , input1Cycling 
        ]
    
    


upArrow =
    polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, 12 ) ] |> filled (rgba 155 0 0 1)


downArrow =
    polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, -12 ) ] |> filled (rgba 155 0 0 1)



type Msg m
    = Tick Float GetKeyState 
    | UInput1
    | UInput1Reverse
    | UInput2
    | UInput2Reverse
    | UInput3
    | UInput3Reverse
    | UInputOp1
    | UInputOp1Reverse
    | UInputOp2
    | UInputOp2Reverse
    | Notif Notifications


type UserInputs
    = A
    | B
    | C
    | NotA
    | NotB
    | NotC

type LogicOperations
    = AND
    | OR
    | XOR

type Notifications
    = NotifyTap
  

--function to cycle through all the boolean operations availavle
cycleOperations op = 
    case op of
        AND ->
            OR
        
        OR ->
            XOR
        
        XOR ->
            AND

--function to reverse cycle through all the boolean operations
cycleOperationsReverse op = 
    case op of
        OR ->
            AND
        
        AND ->
            XOR
        
        XOR ->
            OR

--function to cycle through all aavilable inputs
cycleInputs ui =
    case ui of
        A ->
            B

        B ->
            C

        C ->
            NotA

        NotA ->
            NotB

        NotB ->
            NotC

        NotC ->
            A
--function to reverse cycle through all the avalble inputs
cycleInputsReverse ui =
    case ui of
        

        A ->
            NotC
        
        B ->
            A

        C ->
            B

        NotA ->
            C

        NotB ->
            NotA

        NotC ->
            NotB

--String to display the selected inputs 
inputText ui = 
    case ui of 
        A ->
            "A"
        
        B ->
            "B"
        
        C ->
            "C"
        
        NotA ->
            "Not A"
        
        NotB ->
            "Not B"
        
        NotC ->
            "Not C"

operationText op =
    case op of 
        AND ->
            "AND"
        OR ->
            "OR"
        XOR ->
            "XOR"


--function to fill the given circle
drawCircle input =
    (case input of
        A ->
            group [
                circle 45 
                    |> filled red
                    |> makeTransparent 0.75
                    |> move posA
            ]
            
        B ->
            group [
                circle 45
                    |> filled blue
                    |> makeTransparent 0.75
                    |> move posB
            ]
        C ->
            group [
                circle 45
                    |> filled green
                    |> makeTransparent 0.75
                    |> move posC
            ]
        
        NotA -> 
            group [
                roundedRect 260 220 5
                    |> filled yellow
                    |> makeTransparent 0.20
                    |> move (110,70)
                    |> subtract (drawCircle A)

            ]
        
        NotB -> 
            group [
                roundedRect 260 220 5
                    |> filled yellow
                    |> makeTransparent 0.20
                    |> move (110,70)
                    |> subtract (drawCircle B)

            ]
        
        NotC -> 
            group [
                roundedRect 260 220 5
                    |> filled yellow
                    |> makeTransparent 0.20
                    |> move (110,70)
                    |> subtract (drawCircle C)

            ]

    )

--function to draw the outline of the given circle 
drawOutline input =
    (case input of
        A ->
            group [
                 circle 45
                    |> outlined (solid 1.5) black
                    |> move posA
                , text "A"
                    |> outlined (solid 0.5) black
                    |> move (20, 30)
                
            ]
             
        B ->
            group [
                 circle 45
                    |> outlined (solid 1.5) black
                    |> move posB
                , text "B"
                    |> outlined (solid 0.5) black
                    |> move (190,30)
                
            ]
        C ->
            group [
                circle 45
                    |> outlined (solid 1.5) black
                    |> move posC
                , text "C"
                    |> outlined (solid 0.5) black
                    |> move (100,150)
            ]
        
        NotA -> 
            group [drawOutline A
    
            ]
        
        NotB -> 
            group [drawOutline B
 
            ]
        
        NotC -> 
            group [drawOutline C

            ]
    )

--function to cut portions of the circle based on two given inputs and the operation to perform on them
clipCircle  shape1 shape2 op =
    (case op of 
        AND ->
        --simply take the common ground between two given shapes
            group [
                clip (shape1) (shape2)
                
            ]
        
        OR ->
        --simply fill up the two given shape
            group [
                shape1 
                ,shape2
            ]
        -- take the & and | of the two shapes and subtract & results from the | result
        XOR ->
            group [
                subtract (clip (shape1) (shape2)) (group [shape1 ,shape2])
            ]
        )

--perform the selected operations on the selected inputs
displayResult m =
    group [
        clipCircle (clipCircle  (drawCircle m.uInput1) (drawCircle m.uInput2) (m.uInputOp1)) (drawCircle m.uInput3) (m.uInputOp2)
        ,drawOutline m.uInput1
        ,drawOutline m.uInput2
        ,drawOutline m.uInput3
        ,text ((inputText m.uInput1) ++ "   " ++ (operationText m.uInputOp1) ++ "   " ++ (inputText m.uInput2)++ "   "  ++ (operationText m.uInputOp2) ++ "   " ++ (inputText m.uInput3))
                    |> size 9
                    |> outlined (solid 1) red
                    |> move (80,-32)
                    |> makeTransparent 0.65
    ]
 

displayPrecedence m =
    group [
        clipCircle (drawCircle m.uInput1) (drawCircle m.uInput2) (m.uInputOp1)
            |> move pPos
        ,drawOutline m.uInput1
            |> move pPos
        ,drawOutline m.uInput2
            |> move pPos
        ,text ((inputText m.uInput1) ++ "   " ++ (operationText m.uInputOp1) ++ "   " ++ (inputText m.uInput2))
                    |> size 9
                    |> outlined (solid 1) red
                    |> move (-160,-32)
                    |> makeTransparent 0.65

    ]
