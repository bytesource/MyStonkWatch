module App

open System
open Shared.Types

// https://github.com/bengobeil/StonkWatch/blob/master/src/Client/App.fs
// https://github.com/bytesource/MyStonkWatch
open Sutil
open Sutil.DOM
open Sutil.Attr
open Sutil.Styling
open Sutil.Bulma
open Feliz
open type Feliz.length

open Fable.Remoting.Client

[<RequireQualifiedAccess>]
module Sutil = 
    let map f x = 
        Store.map f x 
        |> Store.distinct

    
    let bind f x = Bind.fragment x f


module Style = 
    let [<Literal>] lightGrey = "#EEEEEE"

[<AutoOpen>]
module SutilOperators = 
    let (|>>) x f = 
        x 
        |> Store.map f 
        |> Store.distinct


    let (>>==) x f = Bind.fragment x f



module Bulma = 
    let createElement nodeFn css =
        fun props -> nodeFn <| [ class' css ] @ props

    let navbar = createElement Html.nav "navbar" 
    let level = createElement Html.nav "level"
    let table = createElement Html.table "table"

    
    module Navbar = 
        let brand = createElement Html.nav "navbar-brand"
        let item  = createElement Html.a   "navbar-item"
            
    module Level = 
        let left = createElement Html.div "level-left"
        let right = createElement Html.div "level-right"
        let item = createElement Html.div "level-item"


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
    | FetchPortfolio
    | PortfolioFetched of Portfolio


let portfolioApi = 
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IPortfolioApi>


let init () : Model * Cmd<Message> = 
    {   // We start with an empty portfolio
        Portfolio = { Positions = []; Balances = DUMMY }
        CurrentPortfolioTab = Positions
    }, Cmd.ofMsg FetchPortfolio


let update (msg : Message) (model : Model) : Model * Cmd<Message> =
    match msg with 
    | SelectedPaneChanged portfolioTab ->
        let model = 
            if portfolioTab <> model.CurrentPortfolioTab then
                { model with CurrentPortfolioTab = portfolioTab }
            else 
                model

        model, Cmd.none

    | FetchPortfolio -> 
        let msg = async {
            let! portfolio = portfolioApi.getPortfolio()
            return PortfolioFetched portfolio
        }

        model, Cmd.OfAsync.result msg

    | PortfolioFetched portfolio ->
        { model with Portfolio = portfolio }, Cmd.none
        

// View --------------------------------------------
let mainStylesheet = 
    Sutil.Bulma.withBulmaHelpers
        [ rule "nav.navbar" [ Css.borderBottom (px 1, borderStyle.solid, Style.lightGrey) ]
          rule ".body" [ Css.height (vh 100)]
          rule ".full-height" [ Css.height (percent 100)]
          rule ".span.pnl-percent" [ Css.fontSize (em 1.1) 
                                     Css.fontWeight 500 ]
          rule ".pnl-element.positive" [ Css.color "green" ]
          rule ".pnl-element.negative" [ Css.color "red" ]
          rule "button.button.selected" [ Css.backgroundColor "#6A4287"
                                          Css.color "white" ]
        ]

[<RequireQualifiedAccess>]
module PnL = 
    let span (PnL percentage) =
        Html.span [ class' "pnl-element"
                    if percentage >= 0.0m<percent> then
                        class' "positive"
                    else 
                        class' "negative"

                    Html.text $"""{percentage}{"%"}""" ]


module Navbar = 
    open Bulma

    let section = 
        navbar [ Navbar.brand [ Navbar.item [ Html.h5 [ Html.text "MY STONK"] ]]]


module SummaryPage = 
    open Bulma

    let positionsTable (positionStore: IObservable<PositionInfo list>) = 

        let header = 
            Html.thead [ Html.tr [ Html.th [ Html.text "Symbols" ] 
                                   Html.th [ Html.text "Open price"]
                                   Html.th [ Html.text "Current price"]
                                   Html.th [ Html.abbr [ attr("title", "Open quantity")]
                                             Html.text "QTY"]
                                   Html.th [ Html.abbr [ attr("title", "Open profit and loss")]
                                             Html.text "Open PnL"]]]


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

            Html.tr [ Html.td [ Html.text (Stock.getSymbolString info.Stock) ]
                      Html.td [ Html.text openPriceString ]
                      Html.td [ Html.text currentPriceString ]
                      Html.td [ Html.text openQtyString ]
                      Html.td [ PnL.span pnl ]]

        let rows positions = 
            positions
            |> List.map getRowFromPositionInfo


        let getTableFromPositions positions = 
            Bulma.table <| header::rows positions

        positionStore >>== getTableFromPositions
        // positionStore |> Sutil.bind getTableFromPositions
        // Bind.fragment positionStore  getTableFromPositions


    let pnlElement (title: string) pnl = 

        Level.item
            [ bulma.container 
                [ style [ Css.textAlignCenter] 
                  Html.h5 [ Html.text title; class' "mb-2" ]
                  PnL.span pnl ]]


    let button dispatch portfolioTab isSelectedStore = 
        let tabString = string portfolioTab
        Level.item [ bulma.button.button [ Html.text tabString
                                           onClick (fun _ -> dispatch <| SelectedPaneChanged portfolioTab) []
                                           bindClass isSelectedStore "selected" ]]


    let level dispatch (selectedPaneStore: IObservable<PortfolioTab>) = 
        // let isPositionsSelected = selectedPaneStore |> Store.map (function Positions -> true | _ -> false)
        let isPositionsSelected = selectedPaneStore |>> ((=) Positions)
        // let isPositionsSelected = selectedPaneStore |> Sutil.map ((=) Positions)
        let isBalancesSelected  = selectedPaneStore |>> ((=) Balances)
        Bulma.level
            [ Level.left [ button dispatch Positions isPositionsSelected
                           button dispatch Balances isBalancesSelected ]
              Level.right [ pnlElement "Open Pnl" (PnL -3.22m<percent>) ]
              Level.right [ pnlElement "Day Pnl" (PnL 3.34m<percent>) ]]



    let contentView model dispatch = 

        let currentTabStore = 
            model 
            |> Sutil.map (fun m -> m.CurrentPortfolioTab)
            // model |>> (fun m -> m.CurrentPortfolioTab)

        let portfolioStore = 
            model |>> (fun m -> m.Portfolio)


        let getViewForSelectedPane = function 
            | Positions -> 
                let positionListStore = 
                    portfolioStore |>> (fun p -> p.Positions)

                // Wrap a div to avoid a bug.
                Html.div [ positionsTable positionListStore ]

            | Balances -> Html.text "Not implemented yet"


        bulma.section [ Html.div [ style [ Css.backgroundColor Style.lightGrey ]

                                   bulma.container [ class' "p-5"
                                                     Html.h3 [ Html.text "Account Summary" ]
                                                     bulma.container [ class' "pt-5"
                                                                       level dispatch currentTabStore
                                                                       currentTabStore >>== getViewForSelectedPane ]]]]


module Main = 
    open Bulma

    let section model dispatch = 
        Html.div [ 
            class' "full-height"
            bulma.columns
                [ bulma.column [ column.is2
                                 style [ Css.backgroundColor Style.lightGrey ]]
                  bulma.column [ SummaryPage.contentView model dispatch ] ] ]


// In Sutil, the view() function is called *once*
let view() = 
    let model, dispatch = Store.makeElmish init update ignore ()


    Html.div [ disposeOnUnmount [ model ]
               class' "body"
               Navbar.section 
               Main.section model dispatch]
    |> withStyle mainStylesheet


// Start the app
view() |> mountElement "sutil-app"