# Download the CIK (central key index) number associated with a stock ticker.
getCik <- function(ticker) {
    # The SEC stores ticker <-> CIK mappings in a JSON file.
    ## TODO: should this be in constants? (It is a one-off url)
    url <- "https://www.sec.gov/files/company_tickers.json"

    # Download the JSON file.
    data <- fromJSON(url)

    # Convert json format to data frame.
    df <- data.frame(do.call(rbind, data))

    # Convert all character of the ticker symbol to uppercase.
    ticker <- toupper(ticker)

    # Find the CIK via ticker.
    entry <- df[df$ticker == ticker,]

    # If there is no match return NA.
    if (nrow(entry) == 0){
        return(NA)
    }

    # Prepend zeros to the cik string to make it 10 digits long.
    return(sprintf("%010d", as.integer(entry$cik_str)))
}
