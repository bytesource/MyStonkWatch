module Server.SeedData
open Shared.Types

let stock = {
        Symbol = Symbol "GME"
        CurrentPrice = CurrentStockPrice (StockPrice (2.00m<price>))
        LastClosePrice = LastClosePrice (StockPrice (3.00m<price>))
    }

let positionInfo = {
    Stock = stock
    AverageOpenPrice = AverageOpenPrice (AveragePrice (1.00m<price>))
    OpenQty = ShareQuantity (Quantity 34u)
}

let portfolio = { Balances = DUMMY; Positions = [ positionInfo ] }