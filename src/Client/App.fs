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
[<Measure>] type price

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


type Symbol = Symbol of string


type StockPrice = StockPrice of decimal<price>
type CurrentStockPrice = CurrentStockPrice of StockPrice

[<RequireQualifiedAccess>]
module CurrentStockPrice = 
    let get (CurrentStockPrice (StockPrice price )) = price

type LastClosePrice = LastClosePrice of StockPrice


type Quantity = Quantity of uint // unsigned integer (always positive)
type ShareQuantity = ShareQuantity of Quantity

[<RequireQualifiedAccess>]
module ShareQuantity = 
    let get (ShareQuantity (Quantity num)) = num


type PnL = PnL of decimal<percent>

type OpenPnL = OpenPnL of PnL
type PositionOpenPnL = PositionOpenPnL of OpenPnL
type PortfolioOpenPnL = PortfolioOpenPnL of OpenPnL

type DayPnL = DayPnl of PnL
type PositionDayPnL = PositionDayPnL of DayPnL
type PortfolioDayPnL = PortfolioDayPnL of DayPnL


type Stock = {
    Symbol: Symbol
    CurrentPrice: CurrentStockPrice
    LastClosePrice: LastClosePrice
}

[<RequireQualifiedAccess>]
module Stock = 
    let getSymbolString ({ Symbol = (Symbol ticker) }) = 
        ticker


type AveragePrice = AveragePrice of decimal<price>
type AverageOpenPrice = AverageOpenPrice of AveragePrice

[<RequireQualifiedAccess>]
module AverageOpenPrice = 
    let get (AverageOpenPrice (AveragePrice price)) = price

type PositionInfo = {
    Stock: Stock
    OpenQty: ShareQuantity
    // PnL can be calculated. Only part of the UI, but not the domain.
    // OpenPnL: PositionOpenPnL
    // DayPnL: PositionDayPnL
    AverageOpenPrice: AverageOpenPrice
}

type Balances = DUMMY


type Portfolio = {
    Positions: PositionInfo list
    Balances: Balances
}


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


let init () : Model = 
    let stock = {
        Symbol = Symbol "GME"
        CurrentPrice = CurrentStockPrice (StockPrice (3.5m<price>))
        LastClosePrice = LastClosePrice (StockPrice (4.6m<price>))
    }

    let positionInfo = {
        Stock = stock
        AverageOpenPrice = AverageOpenPrice (AveragePrice (2.567m<price>))
        OpenQty = ShareQuantity (Quantity 34u)
    }

    let portfolio = { Balances = DUMMY; Positions = [ positionInfo ] }
    { 
        Portfolio = portfolio 
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

            tr [ td [ text <| Stock.getSymbolString info.Stock ]
                 td [ text openPriceString ]
                 td [ text currentPriceString ]
                 td [ text openQtyString ]]

        let rows positions = 
            positions
            |> List.map getRowFromPositionInfo


        let getTableFromPositions positions = 
            Bulma.table <| header::rows positions

        Bind.fragment positionStore  getTableFromPositions


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
              Level.right [ pnlElement "Open Pnl" -3.22m<percent> ]
              Level.right [ pnlElement "Day Pnl" 3.34m<percent> ]]



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