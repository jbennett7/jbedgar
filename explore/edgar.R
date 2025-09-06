suppressMessages({
    require(XBRL)
    require(dplyr)
    require(stringr)
    require(jsonlite)
    source('R/getCik.R')
    source('R/submissions.R')
    source('R/getFilings.R')
    source('R/constants.R')
})

ua <- readLines('./.ignore/useragent')
options(HTTPUserAgent = ua)
reports <- getFilings('xyz') %>%
    filter(primaryDocDescription == '10-Q') %>%
    select(link)

inst <-xbrlDoAll(as.data.frame(reports)[1,])
names(inst)
