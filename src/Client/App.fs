module App

// https://github.com/bengobeil/StonkWatch/blob/master/src/Client/App.fs
// https://github.com/bytesource/MyStonkWatch

open Sutil.DOM.Html
open Sutil.DOM
open Sutil.Attr
open Sutil.Styling
open Sutil.Bulma

[<Measure>] type percent

module Style = 
    let [<Literal>] lightGrey = "#EEEEEE"


module Bulma = 
    let createElement el className =
        fun props -> el <| [ class' className ] @ props

    let navbar = createElement nav "navbar" 
    let level = createElement nav "level"

    
    module Navbar = 
        let brand = createElement nav "navbar-brand"
        let item  = createElement a   "navbar-item"
            
    module Level = 
        let left = createElement div "level-left"
        let right = createElement div "level-right"
        let item = createElement div "level-item"


type SummaryInfo = 
    | Positions
    | Balances


type Model = 
    {
        OpenPnl: decimal<percent>
        DayPnl: decimal<percent>
        SelectedPane: SummaryInfo
    }

// Model helpers

type Message = NoOp

let init () : Model = { 
    OpenPnl = 3.35m<percent>
    DayPnl = -3.32m<percent>
    SelectedPane = Positions
}

let update (msg : Message) (model : Model) : Model =
    model

// View --------------------------------------------
let mainStylesheet = 
    Sutil.Bulma.withBulmaHelpers
        [ rule "nav.navbar" [ Css.borderBottom $"1px {Style.lightGrey} solid" ]
          rule ".body" [ Css.height "100vh"]
          rule ".full-height" [ Css.height "100%"]
          rule ".span.pnl-percent" [ Css.fontSize "1.1em"
                                     Css.fontWeight "500" ]
          rule ".pnl-element.positive" [ Css.color "green" ]
          rule ".pnl-element.negative" [ Css.color "red" ]
        ]


module Navbar = 
    open Bulma

    let section = 
        navbar [ Navbar.brand [ Navbar.item [ h5 [ text "MY STONK"] ]]]


module Main = 
    open Bulma

    let pnlElement title (percentage: decimal<percent>)= 
        let percentageSpan =
            span [ class' "pnl-element"
                   if percentage >= 0.0m<percent>
                   then class' "positive"
                   else class' "negative"
                   text $"""{percentage}{"%"}""" ]
        Level.item
            [ bulma.container 
                [ style [ Css.textAlign "center"] 
                  h5 [ text title
                       class' "mb-2" ]
                  percentageSpan ]]

    let button str = 
        Level.item [ bulma.button [ text str ]]


    let level = 
        Bulma.level
            [ Level.left [ button "Postitions"
                           button "Balances" ]
              Level.right [ pnlElement "Open Pnl" -3.22m<percent> ]
              Level.right [ pnlElement "Day Pnl" 3.34m<percent> ]]



    let contentView = 
        bulma.section [ div [ style [ Css.backgroundColor Style.lightGrey ]

                              bulma.container 
                                  [ class' "p-5"
                                    h3 [ text "Account Summary" ]
                                    bulma.container [ class' "pt-5"
                                                      level]]]]

    let section = 
        div [ 
            class' "full-height"
            bulma.columns
                [ bulma.column [ column.is 2
                                 style [ Css.backgroundColor Style.lightGrey ]]
                  bulma.column [ contentView ] ] ]



// In Sutil, the view() function is called *once*
let view() = 
    div [  
        class' "body"
        Navbar.section 
        Main.section
        ]
    |> withStyle mainStylesheet


// Start the app
view() |> mountElement "sutil-app"