submissions <- function(ticker){
    cik <- getCik(ticker)
    if(is.na(cik))
        stop(paste0('Ticker: "', ticker, '" does not exist.'))
    slink <- paste0("https://data.sec.gov/submissions/CIK", cik, ".json")
    data  <- fromJSON(slink)
    return(data)
}
