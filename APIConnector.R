# -- -------------------------------------------------------------------------------------------- #
# -- meXBT Various API CONNECTOR ---------------------------------------------------------------- #
# -- License: PRIVATE and Right Reserved -------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

Pkg <- c("base","digest","downloader","fBasics","forecast","grid","gridExtra","jsonlite",
"lubridate","plyr","quantmod","Quandl","reshape2","RCurl","stats",
"scales","tseries","TTR","TSA","xts","xts","zoo")

inst <- Pkg %in% installed.packages()
if(length(Pkg[!inst]) > 0) install.packages(Pkg[!inst])
instpackages <- lapply(Pkg, library, character.only=TRUE)

EconomicCalendar <- function(Instrument,Period)

# -- -------------------------------------------------------------------------------------------- #
# -- OANDA REST API, Indices, Commodities, Bonds Yields ----------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

{
  auth  <- c(Authorization = paste("Bearer",
  "939b7f322b14828aed5fe3088d091fec-34ee9ab87e1a241ae65cf3e157a5aabf",sep=" "))
  auth1 <- paste("Authorization:",auth,sep=" ")
  
  Queryhttp  <- paste("https://api-fxpractice.oanda.com",
  "/labs/v1/calendar?instrument=",sep="")
  Queryhttp1 <- paste(Queryhttp,Instrument,sep="")  
  Queryhttp2 <- paste(Queryhttp1,"period=",sep="&")  
  Queryhttp3 <- paste(Queryhttp2,Period,sep="")
  
  CalenH <- getURL(Queryhttp3,cainfo=system.file("CurlSSL",
  "cacert.pem",package="RCurl"),httpheader=auth)
  Calend <- fromJSON(CalenH, simplifyDataFrame = TRUE)
  Calend <- Calend[complete.cases(Calend[,]),]
  Calend$timestamp <- as.POSIXct(Calend$timestamp,origin = "1970-01-01")
  return(Calend)
}

ActualPrice <- function(Instrument)
{
  auth      <- c(Authorization = paste("Bearer",
  "939b7f322b14828aed5fe3088d091fec-34ee9ab87e1a241ae65cf3e157a5aabf",sep=" "))
  Queryhttp <- paste("https://api-fxpractice.oanda.com",
  "/v1/prices?instruments=",sep="")
  QueryPrec <- paste(Queryhttp,Instrument,sep="")
  InstPrec  <- getURL(QueryPrec,cainfo=system.file("CurlSSL","cacert.pem",
  package="RCurl"),httpheader=auth)
  InstPrecjson <- fromJSON(InstPrec, simplifyDataFrame = TRUE)
  DateTime     <- as.POSIXct(substr(InstPrecjson[[1]]$time,12,19),format = "%H:%M:%S")
  Date <- as.character(substr(DateTime,1,10))
  Time <- as.character(substr(DateTime,12,19))
  DataJSON  <- data.frame(Date,Time,InstPrecjson[[1]]$bid,InstPrecjson[[1]]$ask)
  colnames(DataJSON) <- c("Date","Time","Bid","Ask")
  return(DataJSON)
}

HisPrices  <- function(Granularity,Instrument,Count)
{
  qcount  <- paste("count=",Count,sep="")
  qcandleFormat <- "candleFormat=midpoint"
  qgranularity  <- paste("granularity=",Granularity,sep="")
  qdailyalignment    <- paste("dailyAlignment=",21,sep="")
  qalignmentTimezone <- paste("alignmentTimezone=","America%2FMexico_City",sep="")
  
  auth           <- c(Authorization = paste("Bearer",
  "939b7f322b14828aed5fe3088d091fec-34ee9ab87e1a241ae65cf3e157a5aabf",sep=" "))
  QueryHistPrec  <- paste("https://api-fxpractice.oanda.com",
  "/v1/candles?instrument=",sep="")
  QueryHistPrec1 <- paste(QueryHistPrec,Instrument,sep="")
  QueryHistPrec2 <- paste(QueryHistPrec1,qcount,qcandleFormat,qgranularity,
  qdailyalignment,qalignmentTimezone,sep="&")
  InstHistP <- getURL(QueryHistPrec2,cainfo=system.file("CurlSSL","cacert.pem",
  package="RCurl"),httpheader=auth)
  InstHistPjson <- fromJSON(InstHistP, simplifyDataFrame = TRUE)
  Prices        <- data.frame(InstHistPjson[[3]])
  Prices$Times  <- substr(Prices$time,12,19)
  Prices$Dates  <- substr(Prices$time,1,10)
  Prices <- cbind(Prices$Dates,Prices$Times,Prices[,2:7])
  colnames(Prices) <- c("Date","Time","Open","High","Low","Close","TickVolume","Complete")
  return(Prices)
}

# -- -------------------------------------------------------------------------------------------- #
# -- Quantmod - Yahoo FX and Stocks ------------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

OYFxPairD1 <- function(FxPair,APISource,InitialDate,FinaDate)
{
FxData <- data.frame(get(getSymbols(FxPair,src=APISource,from=InitialDate,to=FinaDate)))
FxData <- data.frame(as.Date(row.names(FxData)), FxData[,])
row.names(FxData) <- NULL
FxData <- FxData [-1,]
colnames(FxData) <- c("TimeStamp","Price")
return(FxData)
}


OYStockD1 <- function(Symbol,APISource,InitDate,FinDate)
{
StockData1 <- data.frame(get(getSymbols(Symbol,src=APISource,from=InitDate,to=FinDate)))
StockData1 <- data.frame(as.Date(row.names(StockData1)), StockData1[,1:6])
row.names(StockData1) <- NULL
colnames(StockData1) <- c("TimeStamp","Open","High","Low","Close","Volumen","Adj.Close")
return(StockData1)
}

# -- -------------------------------------------------------------------------------------------- #
# -- meXBT DATA API ----------------------------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

meXBTAPI <- "https://raw.githubusercontent.com/FranciscoME/mexbt-data-r/clean-master/meXBTRClient.R"
downloader::source_url(meXBTAPI,prompt=FALSE,quiet=TRUE)

# -- -------------------------------------------------------------------------------------------- #
# -- Bitex.la DATA API -------------------------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

# -- Present OrderBook Bitex.la DATA API -------------------------------------------------------- #

BitexlaOrderBook  <- function(TimeStamp)
{
BitexlaOBQuery    <- "https://bitex.la/api-v1/rest/btc/market/order_book"
BitexlaOBGetQuery <- getURL(BitexlaOBQuery,cainfo=system.file("CurlSSL",
                      "cacert.pem",package="RCurl"))
BitexlaOBBids <- data.frame(fromJSON(BitexlaOBGetQuery)[1])
BitexlaOBBids$Side <- "Long(Bid)"
colnames(BitexlaOBBids) <- c("Price","Amount","Side")
BitexlaOBAsks <- data.frame(fromJSON(BitexlaOBGetQuery)[2])
BitexlaOBAsks$Side <- "Short(Ask)"
colnames(BitexlaOBAsks) <- c("Price","Amount","Side")
BitexlaOBAsks <- BitexlaOBAsks[order(-BitexlaOBAsks$Price), , drop = FALSE]
BitexlaOB <- rbind(BitexlaOBAsks,BitexlaOBBids)
BitexlaOB <- data.frame(TimeStamp,BitexlaOB[,])
return(BitexlaOB)
}

# -- Historical Transactions Bitex.la DATA API -------------------------------------------------- #

BitexlaTrades <- function(TimeStamp)
{
BitexlaTQuery    <- "https://bitex.la/api-v1/rest/btc/market/transactions"
BitexlaTGetQuery <- getURL(BitexlaTQuery,cainfo=system.file("CurlSSL",
                    "cacert.pem",package="RCurl"))
BitexlaT <- data.frame(fromJSON(BitexlaTGetQuery))
colnames(BitexlaT) <- c("TimeStamp","TransactionID","PricePaid","AmountSold")
BitexlaT$TimeStamp <- as.POSIXct(BitexlaT$TimeStamp,origin = "1970-01-01")
return(BitexlaT)
}

# -- Ticker Bitex.la DATA API ------------------------------------------------------------------- #

BitexlaTicker  <- function(TimeStamp)
{
BitexlaTkQuery    <- "https://bitex.la/api-v1/rest/btc/market/ticker"
BitexlaTkGetQuery <- getURL(BitexlaTkQuery,cainfo=system.file("CurlSSL",
                     "cacert.pem",package="RCurl"))
BitexlaTicker     <- data.frame(fromJSON(BitexlaTkGetQuery))
return(BitexlaTicker)
}

# -- Transactions from Last 24 Hours Bitex.la DATA API ------------------------------------------ #

Bitexla24HistTrades  <- function(TimeStamp)
{
BitexlaT24Query    <- "https://bitex.la/api-v1/rest/btc/market/last_24_hours"
BitexlaT24GetQuery <- getURL(BitexlaT24Query,cainfo=system.file("CurlSSL",
                      "cacert.pem",package="RCurl"))
BitexlaT24 <- data.frame(fromJSON(BitexlaT24GetQuery))
colnames(BitexlaT24) <- c("TimeStamp","Low","Open","Close","High","Amount",
                        "Last","Volume-Weigh")
BitexlaT24$TimeStamp <- as.POSIXct(as.numeric(BitexlaT24$TimeStamp),
                        origin = "1970-01-01")
return(BitexlaT24)
}

# -- -------------------------------------------------------------------------------------------- #
# -- BANX.io DATA API --------------------------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

# -- Markets Available BANX.io DATA API --------------------------------------------------------- #

BanxioMarkets  <- function(TimeStamp)
{
BANXioQuery1  <- "https://www.banx.io/SimpleAPI?a=markets"
BANXioQuery1G <- getURL(BANXioQuery1,cainfo=system.file("CurlSSL",
                 "cacert.pem",package="RCurl"))
BANXioMkts <- data.frame(fromJSON(BANXioQuery1G))
colnames(BANXioMkts) <- "Market"
return(BANXioMkts)
}

# -- Markets V2 Available BANX.io DATA API ------------------------------------------------------ #

BanxioMarketsV2  <- function(TimeStamp)
{
BANXioQuery2  <- "https://www.banx.io/SimpleAPI?a=marketsv2"
BANXioQuery2G <- getURL(BANXioQuery2,cainfo=system.file("CurlSSL",
                 "cacert.pem",package="RCurl"))
BANXioMkts2   <- data.frame(fromJSON(BANXioQuery2G))
return(BANXioMkts2)
}

# -- Order Book Available BANX.io DATA API ------------------------------------------------------ #

BanxioOrderBook  <- function(MarketC,MarketP)
{
MarketC <- "BTC"
MarketP <- "USD"
BANXioQuery3a  <- paste("https://www.banx.io/SimpleAPI?a=orderbook&c=",MarketC,sep="")
BANXioQuery3b  <- paste(BANXioQuery3a,"&p=",sep="")
BANXioQuery3c  <- paste(BANXioQuery3b,MarketP,sep="")
BANXioQuery3G <- getURL(BANXioQuery3c,cainfo=system.file("CurlSSL",
                 "cacert.pem",package="RCurl"))
BANXioOB <- fromJSON(BANXioQuery3G, simplifyDataFrame = TRUE)[1]$market$recenttrades
return(BANXioOB)
}

# -- Historical Trades Available BANX.io DATA API ----------------------------------------------- #

BanxioTrades  <- function(MarketC,MarketP,TradeSince)
{
BANXioQuery4a <- paste("https://www.banx.io/SimpleAPI?a=tradessince&c=",MarketC,sep="")
BANXioQuery4b <- paste(BANXioQuery4a,"&p=",sep="")
BANXioQuery4c <- paste(BANXioQuery4b,MarketP,sep="")
BANXioQuery4d <- paste(BANXioQuery4c,"&T=",sep="")
BANXioQuery4e <- paste(BANXioQuery4d,TradeSince,sep="")
BANXioQuery4G <- getURL(BANXioQuery4e,cainfo=system.file("CurlSSL",
                 "cacert.pem",package="RCurl"))
BANXioTrades  <- data.frame(fromJSON(BANXioQuery4G, simplifyDataFrame = TRUE)[1])
return(BANXioTrades)
}

# -- Actual Price Available BANX.io DATA API ---------------------------------------------------- #

BanxioTicker  <- function(MarketC)
{
BANXioQuery5  <- paste("https://www.banx.io/GetPrices?c=",MarketC,sep="")
BANXioQuery5a <- paste(BANXioQuery5,"&p=",sep="")
BANXioQuery5G <- getURL(BANXioQuery5a,cainfo=system.file("CurlSSL",
                 "cacert.pem",package="RCurl"))
BANXioTicker  <- fromJSON(BANXioQuery5G, simplifyDataFrame = TRUE)
return(BANXioTicker)
}

# -- -------------------------------------------------------------------------------------------- #
# -- BaseBit DATA API --------------------------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

# -- Markets/Pairs Available BaseBit DATA API --------------------------------------------------- #

BaseBitPairs  <- function(TimeStamp)
{
BaseBitInstruments <- fromJSON(getURL("http://www.basebit.com.br/listpairs",
cainfo=system.file("CurlSSL","cacert.pem",package="RCurl")), simplifyDataFrame = TRUE)
return(BaseBitInstruments)
}

# -- Actual Ticker BaseBit DATA API ------------------------------------------------------------- #

BaseBitTicker <- function(TimeStamp)
{
BaseBitTicker <- fromJSON(getURL("http://www.basebit.com.br/quote-BTC_BRL",
cainfo=system.file("CurlSSL","cacert.pem",package="RCurl")),simplifyDataFrame = TRUE)
BaseBitTicker <- do.call(rbind.data.frame, BaseBitTicker)
return(BaseBitTicker)
}

# -- Order Book BaseBit DATA API ---------------------------------------------------------------- #

BaseBitOrderBook <- function(TimeStamp)
{
BaseBitOrderBook <- fromJSON(getURL("http://www.basebit.com.br/book-BTC_BRL",
cainfo=system.file("CurlSSL","cacert.pem",package="RCurl")),
simplifyDataFrame = TRUE)$result

BaseBitOBBids <- data.frame(BaseBitOrderBook$bids)
BaseBitOBBids$Side  <- "Long(Bid)"
colnames(BaseBitOBBids) <- c("Price","Amount","Side")
BaseBitOBBids$Price <- as.numeric(BaseBitOBBids$Price)

BaseBitOBAsks <- data.frame(BaseBitOrderBook$asks)
BaseBitOBAsks$Side  <- "Short(Ask)"
colnames(BaseBitOBAsks) <- c("Price","Amount","Side")
BaseBitOBAsks$Price <- as.numeric(BaseBitOBAsks$Price)

BaseBitOBAsks <- BaseBitOBAsks[order(-BaseBitOBAsks$Price), , drop = FALSE]
BaseBitOB <- rbind(BaseBitOBAsks,BaseBitOBBids)
BaseBitOB <- data.frame(TimeStamp,BaseBitOB[,])
return(BaseBitOB)
}

# -- Trades BaseBit DATA API -------------------------------------------------------------------- #

BaseBitTrades <- function(TimeStamp)
{
BaseBitTrades <- data.frame(fromJSON(getURL("http://www.basebit.com.br/trades-BTC_BRL",
cainfo=system.file("CurlSSL","cacert.pem",package="RCurl")),
simplifyDataFrame = TRUE)$result)
BaseBitTrades$time <- as.POSIXct(BaseBitTrades$time/1000, origin = "1970-01-01")
colnames(BaseBitTrades) <- c("TimeStamp","Side","Price","Amount")
return(BaseBitTrades)
}

# -- -------------------------------------------------------------------------------------------- #
# -- 1btcxe public API -------------------------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

# -- 1btcxe Transactions public API ------------------------------------------------------------- #

btcxeTrades <- function(TimeStamp)
{
btcxeTransactions <- fromJSON(getURL("https://1btcxe.com/api/transactions",
cainfo=system.file("CurlSSL","cacert.pem",package="RCurl")))
btcxeTransactions <- do.call(rbind.data.frame, btcxeTransactions$transactions)
btcxeTransactions <- btcxeTransactions[-length(btcxeTransactions[,1]),c(1,2,3,4,6,8)]
colnames(btcxeTransactions) <- c("ID","TimeStamp","Amount","Price","PositionValue","Currency")
return(btcxeTransactions)
}

# -- 1btcxe Stats public API -------------------------------------------------------------------- #

btcxeStats <- function(TimeStamp)
{
btcxeStats <- fromJSON(getURL("https://1btcxe.com/api/stats",
cainfo=system.file("CurlSSL","cacert.pem",package="RCurl")))
btcxeStats <- data.frame(substr(names(unlist(btcxeStats)), 
start = 7, stop = 38),unlist(btcxeStats))
row.names(btcxeStats) <- NULL
colnames(btcxeStats)  <- c("Info","Value")
return(btcxeStats)
}

# -- 1btcxe Historical Prices public API -------------------------------------------------------- #

btcxeHistoricPrices <- function(TimeStamp)
{
btcxeHistoricPrices <- fromJSON(getURL("https://1btcxe.com/api/historical-prices",
cainfo=system.file("CurlSSL","cacert.pem",package="RCurl")),simplifyDataFrame = TRUE)
btcxeHistoricPrices <- do.call(rbind.data.frame, btcxeHistoricPrices$`historical-prices`)
btcxeHistoricPrices <- btcxeHistoricPrices[-length(btcxeHistoricPrices[,1]),]
colnames(btcxeHistoricPrices)  <- c("TimeStamp","Price")
return(btcxeHistoricPrices)
}

# -- 1btcxe OrderBook public API ---------------------------------------------------------------- #

btcxeOrderBook <- function(TimeStamp)
{
btcxeOrderBook <- fromJSON(getURL("https://1btcxe.com/api/order-book",
cainfo=system.file("CurlSSL","cacert.pem",package="RCurl")), simplifyDataFrame = TRUE)
btcxets <- Sys.time()

btcxeOrderBookBID <- do.call(cbind.data.frame, btcxeOrderBook$`order-book`$bid)
btcxeOrderBookBID <- data.frame(btcxets,btcxeOrderBookBID[,1:3])
btcxeOrderBookBID$Side <- "Long(Bid)"
colnames(btcxeOrderBookBID) <- c("TimeStamp","Price","Amount","Value","Side")

btcxeOrderBookASK <- do.call(cbind.data.frame, btcxeOrderBook$`order-book`$ask)
btcxeOrderBookASK <- data.frame(btcxets,btcxeOrderBookASK[,1:3])
btcxeOrderBookASK$Side <- "Short(Ask)"
colnames(btcxeOrderBookASK) <- c("TimeStamp","Price","Amount","Value","Side")

btcxeOrderBookASK <- btcxeOrderBookASK[order(-as.numeric(btcxeOrderBookASK$Price)),
, drop = FALSE]
btcxeOrderBook <- rbind(btcxeOrderBookASK,btcxeOrderBookBID)
btcxeOrderBook <- data.frame(btcxeOrderBook[,])
return(btcxeOrderBook)
}

# -- -------------------------------------------------------------------------------------------- #
# -- Bitso Public API --------------------------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

# -- Bitso OrderBook Public API ----------------------------------------------------------------- #

BitsoOrderBook <- function(TimeStamp)
{
BitsoOrderBookQuery <- fromJSON(getURL("https://api.bitso.com/v2/order_book",
cainfo=system.file("CurlSSL","cacert.pem",package="RCurl")), simplifyDataFrame = TRUE)

BitsoOBBids <- data.frame(as.POSIXct(as.numeric(fromJSON(BitsoOrderBookQuery$timestamp)),
origin = "1970-01-01"), BitsoOrderBookQuery[2])
BitsoOBBids$Side   <- "Long(Bid)"
colnames(BitsoOBBids) <- c("TimeStamp","Price","Amount","Side")
BitsoOBBids$Price  <- as.numeric(paste(BitsoOBBids$Price))
BitsoOBBids$Amount <- as.numeric(paste(BitsoOBBids$Amount))

BitsoOBAsk <- data.frame(as.POSIXct(as.numeric(fromJSON(BitsoOrderBookQuery$timestamp)),
origin = "1970-01-01"),BitsoOrderBookQuery[3])
BitsoOBAsk$ Side <- "Short(Ask)"
colnames(BitsoOBAsk) <- c("TimeStamp","Price","Amount","Side")
BitsoOBAsk$Price  <- as.numeric(paste(BitsoOBAsk$Price))
BitsoOBAsk$Amount <- as.numeric(paste(BitsoOBAsk$Amount))

BitsoOBAsk <- BitsoOBAsk[order(-as.numeric(BitsoOBAsk$Price)), , drop = FALSE]
BitsoOrderBookQuery <- rbind(BitsoOBAsk,BitsoOBBids)
BitsoOrderBookQuery <- data.frame(BitsoOrderBookQuery[,])
return(BitsoOrderBookQuery)
}

# -- Bitso Trades Public API -------------------------------------------------------------------- #

BitsoTrades <- function(TimeStamp)
{
BitsoTradesQ <- fromJSON(getForm("https://api.bitso.com/public/trades?book=btc_mxn",
style = "GET",params=(time="hour"),opts = list(ssl.verifypeer = FALSE)))
BitsoTradesQ <-  data.frame(BitsoTradesQ$datetime,BitsoTradesQ$rate,
BitsoTradesQ$value,BitsoTradesQ$amount)
colnames(BitsoTradesQ) <- c("TimeStamp","Rate","Value","Amount")
BitsoTradesQ$TimeStamp <- as.POSIXct(as.numeric(paste(BitsoTradesQ$TimeStamp))/1000,
origin = "1970-01-01")
BitsoTradesQ$Amount    <- as.numeric(paste(BitsoTradesQ$Amount))
return(BitsoTradesQ)
}

# -- -------------------------------------------------------------------------------------------- #
# -- FoxBit ------------------------------------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

# -- FoxBit Trades Public API ------------------------------------------------------------------- #

FoxBitTrades <- function(TimeStamp)
{
FoxBitTradesQ <- fromJSON(
getURL("https://api.blinktrade.com/api/v1/BRL/trades?crypto_currency=BTC"))
colnames(FoxBitTradesQ) <- c("TransactionID","TimeStamp","Price","AmountBTC")
FoxBitTradesQ$TimeStamp <- as.POSIXct(FoxBitTradesQ$TimeStamp,origin = "1970-01-01")
return(FoxBitTradesQ)
}

# -- FoxBit Order Book Public API --------------------------------------------------------------- #

FoxBitOrderBook <- function(TimeStamp)
{
FoxBitts  <- Sys.time()
FoxBitOBQ <- fromJSON(
readLines("https://api.blinktrade.com/api/v1/BRL/orderbook?crypto_currency=BTC"))
FoxBitOBBids <- data.frame(FoxBitOBQ[2])
FoxBitOBBids <- data.frame(FoxBitts,FoxBitOBBids[,])
FoxBitOBBids$Side <- "Long(Bid)"
colnames(FoxBitOBBids) <- c("TimeStamp","Price","AmountBTC","AmountBRL","Side")
FoxBitOBAsks <- data.frame(FoxBitOBQ[3])
FoxBitOBAsks <- data.frame(FoxBitts,FoxBitOBAsks[,])
FoxBitOBAsks$Side <- "Short(Ask)"
colnames(FoxBitOBAsks) <- c("TimeStamp","Price","AmountBTC","AmountBRL","Side")
FoxBitOBAsks <- FoxBitOBAsks[order(-as.numeric(FoxBitOBAsks$Price)), , drop = FALSE]
FoxBitOrderBookQuery   <- rbind(FoxBitOBAsks,FoxBitOBBids)
return(FoxBitOrderBookQuery)
}

# -- -------------------------------------------------------------------------------------------- #
# -- OKCoin ------------------------------------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

#OKCoinTicker   <- "https://www.okcoin.com/api/ticker.do?ok=1"
#OkCoinLCTicker <- "https://www.okcoin.com/api/ticker.do?symbol=ltc_usd&ok=1"
#OkCoinOB     <- "https://www.okcoin.com/api/depth.do?ok=1"
#OkCoinLCOB   <- "https://www.okcoin.com/api/depth.do?symbol=ltc_usd&ok=1"
#OkCoinTrades <- "https://www.okcoin.com/api/trades.do?since=5000&ok=1"
#OkCoinLCTrades <- "https://www.okcoin.com/api/trades.do?symbol=ltc_usd&since=5000&ok=1"
#OKCoinFutureTicker <- "https://www.okcoin.com/api/future_ticker.do"
#symbol=c("btc_usd","ltc_usd") , contractType= c("this_week","next_week","month","quarter")
#OKCoinFutureOB <- "https://www.okcoin.com/api/future_depth.do"
#OKCoinFutureTrades <- "https://www.okcoin.com/api/future_trades.do"
#OKCoinFutureIndex <- "https://www.okcoin.com/api/future_getIndex.do"

# -- -------------------------------------------------------------------------------------------- #
# -- Bitfinex ----------------------------------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

#BitFinexTicker <- "https://api.bitfinex.com/v1/pubticker/btcusd"
#BitFinexStats  <- "https://api.bitfinex.com/v1/stats/btcusd"
#BitFinexTrades <- "https://api.bitfinex.com/v1/trades/btcusd"
#BitFinexLends  <- "https://api.bitfinex.com/v1/lends/btc"
#BitFinexSym    <- "https://api.bitfinex.com/v1/symbols"
#BitFinexSymD   <- "https://api.bitfinex.com/v1/symbols_details"
#BitFinexLendBook  <- "https://api.bitfinex.com/v1/lendbook/btc"
#BitFinexOrderBook <- "https://api.bitfinex.com/v1/book/btcusd"
