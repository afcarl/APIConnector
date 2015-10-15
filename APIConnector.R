
# -- ------------------------------------------------------------------------------- -- #
# -- Federal Reserve Data API  ----------------------------------------------------- -- #
# -- ------------------------------------------------------------------------------- -- #

# -- Get an economic data series from FRED. ---------------------------------------- -- #

FredSeriesObs <- function(SerieId) {
  Query1 <- "https://api.stlouisfed.org/fred/series/observations?series_id="
  Query2 <- paste(Query1,SerieId,sep="")
  Query3 <- paste(Query2,"2ee50c8c30dcf1653196addef51a4914",sep="&api_key=")
  Query4 <- paste(Query3,"json",sep="&file_type=")
  QueryURL  <- getURLContent(Query4,cainfo=system.file("CurlSSL","cacert.pem",
  package="RCurl"))
  QueryJSON <- data.frame(fromJSON(QueryURL))[,15:16]
  QueryJSON[,1] <- as.POSIXct(QueryJSON[,1], origin = "1970-01-01")
  Queryxts  <- xts(QueryJSON[,2],order.by = QueryJSON[,1])["2010-01-01/2015-10-31"]
return(Queryxts) }

# -- ------------------------------------------------------------------------------- -- #
# -- Quantmod - Yahoo FX and Stocks ------------------------------------------------ -- #
# -- ------------------------------------------------------------------------------- -- #

OYFxPairD1 <- function(FxPair,APISource,InitialDate,FinaDate) {
  FxData <- data.frame(get(getSymbols(FxPair,src=APISource,from=InitialDate,to=FinaDate)))
  FxData <- data.frame(as.Date(row.names(FxData)), FxData[,])
  row.names(FxData) <- NULL
  FxData <- FxData [-1,]
  colnames(FxData) <- c("TimeStamp","Price")
return(FxData) }

OYStockD1 <- function(Symbol,APISource,InitDate,FinDate) {
  StockData1 <- data.frame(get(getSymbols(Symbol,src=APISource,from=InitDate,to=FinDate)))
  StockData1 <- data.frame(as.Date(row.names(StockData1)), StockData1[,1:6])
  row.names(StockData1) <- NULL
  colnames(StockData1) <- c("TimeStamp","Open","High","Low","Close","Volumen","Adj.Close")
return(StockData1) }
