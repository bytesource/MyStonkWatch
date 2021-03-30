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
open Client.Types


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


// This type is about presentation,
// so we don't put it in Types.fs
type PortfolioTab = 
    | Positions
    | Balances


type Model = 
    {
        // PortfolioOpenPnL: PortfolioOpenPnL // Calculated based on the data we have.
        // PortfolioDayPnL: PortfolioDayPnL
        Portfolio: Portfolio
        CurrentPortfolioTab: PortfolioTab
    }

// Events
type Message = 
    | SelectedPaneChanged of PortfolioTab

// open Client.SeedData

let init () : Model = {
    Portfolio = Client.SeedData.portfolio
    CurrentPortfolioTab = Positions
}


let update (msg : Message) (model : Model) : Model =
    match msg with 
    | SelectedPaneChanged portfolioTab ->
        if portfolioTab <> model.CurrentPortfolioTab then
            { model with CurrentPortfolioTab = portfolioTab }
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

[<RequireQualifiedAccess>]
module PnL = 
    let span (PnL percentage) =
        span [ class' "pnl-element"
               if percentage >= 0.0m<percent>
               then class' "positive"
               else class' "negative"
               text $"""{percentage}{"%"}""" ]


module Navbar = 
    open Bulma

    let section = 
        navbar [ Navbar.brand [ Navbar.item [ h5 [ text "MY STONK"] ]]]


module SummaryPage = 
    open Bulma

    let positionsTable (positionStore: IObservable<PositionInfo list>) = 

        let header = 
            thead [ tr [ th [ text "Symbols" ] 
                         th [ text "Open price"]
                         th [ text "Current price"]
                         th [ el "abbr" [ attr("title", "Open quantity")]
                              text "QTY"]
                         th [ el "abbr" [ attr("title", "Open profit and loss")]
                              text "Open PnL"]]]


        let getRowFromPositionInfo (info: PositionInfo) = 
            let openQtyString = 
                ShareQuantity.get info.OpenQty
                |> string

            let openPriceString = 
                AverageOpenPrice.get info.AverageOpenPrice
                |> string

            let currentPriceString = 
                info.Stock.CurrentPrice
                |> CurrentStockPrice.get 
                |> string

            let (PositionOpenPnL (OpenPnL pnl)) = 
                info 
                |> PositionOpenPnL.calculate

            tr [ td [ text <| Stock.getSymbolString info.Stock ]
                 td [ text openPriceString ]
                 td [ text currentPriceString ]
                 td [ text openQtyString ]
                 td [ PnL.span pnl ]]

        let rows positions = 
            positions
            |> List.map getRowFromPositionInfo


        let getTableFromPositions positions = 
            Bulma.table <| header::rows positions

        Bind.fragment positionStore  getTableFromPositions


    let pnlElement title pnl = 

        Level.item
            [ bulma.container 
                [ style [ Css.textAlign "center"] 
                  h5 [ text title
                       class' "mb-2" ]
                  PnL.span pnl ]]

    let button dispatch portfolioTab isSelectedStore = 
        Level.item [ bulma.button [ text <| string portfolioTab
                                    onClick (fun _ -> dispatch <| SelectedPaneChanged portfolioTab) []
                                    bindClass isSelectedStore "selected" ]]


    let level dispatch (selectedPaneStore: IObservable<PortfolioTab>) = 
        // let isPositionsSelected = selectedPaneStore |> Store.map (function Positions -> true | _ -> false)
        let isPositionsSelected = selectedPaneStore |> Store.map ((=) Positions)
        let isBalancesSelected  = selectedPaneStore |> Store.map ((=) Balances)
        Bulma.level
            [ Level.left [ button dispatch Positions isPositionsSelected
                           button dispatch Balances isBalancesSelected ]
              Level.right [ pnlElement "Open Pnl" (PnL -3.22m<percent>) ]
              Level.right [ pnlElement "Day Pnl" (PnL 3.34m<percent>) ]]



    let contentView model dispatch = 

        let selectedPaneStore = 
            model 
            |> Store.map (fun m -> m.CurrentPortfolioTab)
            |> Store.distinct // New value must be different from last value

        let portfolioStore = 
            model 
            |> Store.map (fun m -> m.Portfolio)
            |> Store.distinct // New value must be different from last value

        let getViewForSelectedPane portfolio = function 
            | Positions -> 
                let positionListStore = 
                    portfolio
                    |> Store.map (fun p -> p.Positions)
                    |> Store.distinct

                positionsTable positionListStore

            | Balances -> text "Not implemented yet"


        bulma.section [ div [ style [ Css.backgroundColor Style.lightGrey ]

                              bulma.container 
                                  [ class' "p-5"
                                    h3 [ text "Account Summary" ]
                                    bulma.container [ class' "pt-5"
                                                      level dispatch selectedPaneStore
                                                      Bind.fragment selectedPaneStore <| getViewForSelectedPane portfolioStore ]]]]


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