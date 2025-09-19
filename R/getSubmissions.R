# Download data from the EDGAR `submissions` API for a specific ticker.
getSubmissions <- function(ticker){
    # Git the CIK of the ticker and make sure it exists.
    cik <- getCik(ticker)
    if(is.na(cik))
        stop(paste0('Ticker: "', ticker, '" does not exist.'))

    # Base URL for list of company filings.
    slink <- paste0(edgar.api.url, "submissions/")

    # Download list of corporate filings.
    data <- fromJSON(paste0(slink, "CIK", cik, ".json"))

    # Append archived filings to the recent list.
    ## TODO: Only do this if necessary.
    filings <- as.data.frame(data$filings$recent)
    for(pastFile in data$filings$files$name){
        dlink <- paste0(slink, pastFile)
        tmp <- as.data.frame(fromJSON(paste0(slink, pastFile)))
        filings <- rbind(filings, tmp)
    }

    # restructure the list so that filings is a dataframe.
    data$filings <- filings
    return(data)
}
