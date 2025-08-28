get_cik <- function(ticker) {
    url <- "https://www.sec.gov/files/company_tickers.json"
    data <- fromJSON(url)
    df <- data.frame(do.call(rbind, data))
    ticker <- toupper(ticker)
    entry <- df[df$ticker == ticker,]
    if (nrow(entry) == 0){
        return(NA)
    }
    sprintf("%010d", as.integer(entry$cik_str))
}
