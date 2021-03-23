module App

open System

// https://github.com/bengobeil/StonkWatch/blob/master/src/Client/App.fs
// https://github.com/bytesource/MyStonkWatch
open Sutil
open Sutil.DOM.Html
open Sutil.DOM
open Sutil.Attr
open Sutil.Styling
open Sutil.Bulma

[<Measure>] type percent

module Style = 
    let [<Literal>] lightGrey = "#EEEEEE"


module Bulma = 
    let createElement nodeFn css =
        fun props -> nodeFn <| [ class' css ] @ props

    let navbar = createElement nav "navbar" 
    let level = createElement nav "level"
    let table = createElement table "table"

    
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

module SummaryInfo = 
    let name summaryInfo = 
        match summaryInfo with 
        | Positions -> "Positions"
        | Balances  -> "Balances"


type Model = 
    {
        OpenPnl: decimal<percent>
        DayPnl: decimal<percent>
        SelectedPane: SummaryInfo
    }

// Events
type Message = 
    | SelectedPaneChanged of SummaryInfo


let init () : Model = { 
    OpenPnl = 3.35m<percent>
    DayPnl = -3.32m<percent>
    SelectedPane = Balances
}


let update (msg : Message) (model : Model) : Model =
    match msg with 
    | SelectedPaneChanged summaryInfo ->
        if summaryInfo <> model.SelectedPane then
            { model with SelectedPane = summaryInfo }
        else 
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
          rule "button.button.selected" [ Css.backgroundColor "#6A4287"
                                          Css.color "white" ]
        ]


module Navbar = 
    open Bulma

    let section = 
        navbar [ Navbar.brand [ Navbar.item [ h5 [ text "MY STONK"] ]]]


module SummaryPage = 
    open Bulma

    let header = 
        [ thead [ tr [ th [ text "Symbols" ] 
                       th [ text "Price"]
                       th [ el "abbr" [ attr("title", "Open quantity")]
                            text "QTY"]
                       th [ el "abbr" [ attr("title", "Open profit and loss")]
                            text "Open PnL"]]]]


    let positionsTable = [ header ]


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

    let button dispatch summaryInfo isSelectedStore = 
        Level.item [ bulma.button [ text <| SummaryInfo.name summaryInfo
                                    onClick (fun _ -> SelectedPaneChanged summaryInfo |> dispatch) []
                                    bindClass isSelectedStore "selected" ]]


    let level dispatch (selectedPaneStore: IObservable<SummaryInfo>) = 
        // let isPositionsSelected = selectedPaneStore |> Store.map (function Positions -> true | _ -> false)
        let isPositionsSelected = selectedPaneStore |> Store.map ((=) Positions)
        let isBalancesSelected  = selectedPaneStore |> Store.map ((=) Balances)
        Bulma.level
            [ Level.left [ button dispatch Positions isPositionsSelected
                           button dispatch Balances isBalancesSelected ]
              Level.right [ pnlElement "Open Pnl" -3.22m<percent> ]
              Level.right [ pnlElement "Day Pnl" 3.34m<percent> ]]



    let contentView model dispatch = 

        let selectedPaneStore = 
            model 
            |> Store.map (fun m -> m.SelectedPane)
            |> Store.distinct // New value must be different from last value

        let getViewForSelectedPane = function 
            | Positions -> positionsTable
            | Balances -> text "Not implemented yet"


        bulma.section [ div [ style [ Css.backgroundColor Style.lightGrey ]

                              bulma.container 
                                  [ class' "p-5"
                                    h3 [ text "Account Summary" ]
                                    bulma.container [ class' "pt-5"
                                                      level dispatch selectedPaneStore
                                                      Bind.fragment selectedPaneStore getViewForSelectedPane]]]]


module Main = 
    open Bulma

    let section model dispatch = 
        div [ 
            class' "full-height"
            bulma.columns
                [ bulma.column [ column.is 2
                                 style [ Css.backgroundColor Style.lightGrey ]]
                  bulma.column [ SummaryPage.contentView model dispatch ] ] ]


// In Sutil, the view() function is called *once*
let view() = 
    let model, dispatch = Store.makeElmishSimple init update ignore ()


    div [ disposeOnUnmount [ model ]

          class' "body"
          Navbar.section 
          Main.section model dispatch]
    |> withStyle mainStylesheet


// Start the app
view() |> mountElement "sutil-app"