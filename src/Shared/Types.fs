module Shared.Types


[<Measure>] type percent
[<Measure>] type price


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


module PositionOpenPnL = 

    let toPositionOpenPnL decimalPercent = 
        decimalPercent
        |> PnL
        |> OpenPnL
        |> PositionOpenPnL


    let calculate (positionInfo: PositionInfo): PositionOpenPnL = 
        // ((current - open) / open) * 100
        let current = 
            CurrentStockPrice.get positionInfo.Stock.CurrentPrice

        let open' = 
            AverageOpenPrice.get positionInfo.AverageOpenPrice
            //AverageOpenPrice.get positionInfo.AverageOpenPrice
        
        // decimal<price>/decimal<price> = decimal
        let pnlRatio = (current - open') / open'
        let pnl = pnlRatio * 100m<percent>

        toPositionOpenPnL pnl


    let get (PositionOpenPnL (OpenPnL (PnL percent))) = percent

type Balances = DUMMY


type Portfolio = {
    Positions: PositionInfo list
    Balances: Balances
}
