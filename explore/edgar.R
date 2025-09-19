# This file holds an example of how to parse an XBRL instance for information.
# We will be using Block, Inc. (XYZ) 2020-12-31 10-K report.
#

# First some required packages
suppressMessages({
    require(XBRL)
    require(dplyr)
    require(stringr)
    require(jsonlite)
    require(tidyr)
    require(lubridate)
    require(httr)
    source('R/getCik.R')
    source('R/getSubmissions.R')
    source('R/listFilings.R')
    source('R/constants.R')
})

# EDGAR mandates we set the useragent as a valid email.
ua <- readLines('./.ignore/useragent')
options(HTTPUserAgent = ua)

# An example of how to find non-ascii characters in a string.
is_nonascii <- function(d, v){
    d %>%
        filter(
            is.na(iconv({{ v }}, from="UTF-8", to="ASCII"))
        )
}

# helper function to parse a 10K report if we don't have a local copy
dl.10K.report <- function(sym, rDate){
    # where to store the list of data frames (inst)
    file.path <- paste0('./.ignore/', toupper(sym), '10K',
                        '_', as.character(rDate), '.Rda')
    # if we have a local copy of the data return that and do nothing
    if(file.exists(file.path)){
        load(file.path)
        message('Data cache found, loading...')
        return(inst)
    }
    # we want the 10-K reprot of a certian report date for the ticker
    r10K <- listFilings(sym) %>%
        filter(
            form == '10-K',
            reportDate == rDate,
        )
    # the link to the xbrl instance data
    hlink <- as.character(r10K$link)
    # parse the instance data
    inst <- xbrlDoAll(hlink)
    # save the list of tables as `inst` in the file path and return `inst`
    save(inst, file=file.path)
    return(inst)
}

# Sometimes we need to see the raw XML for comparison to the parsed out 
# data tables
get_10K_xml <- function(sym, rDate){
    r10K <- listFilings(sym) %>%
        filter(
            form == '10-K',
            reportDate == rDate,
        )
    hlink <- as.character(r10K$link)
#hlink <- "https://www.sec.gov/Archives/edgar/data/1512673/000162828025007376/sq-20241231.htm"
    resp <- GET(hlink, user_agent(ua))
    text_content <- content(resp, "text")
    file.path <- paste0('./.ignore/', r10K$primaryDocument)
    writeLines(text_content, file.path)
}

# The report instance data
inst <- suppressMessages({dl.10K.report('xyz', '2020-12-31')})

#str(inst, max.level=1)

# these are the revenue numbers for each of the end dates.
revenues <- unique(inst$fact %>%
    filter(elementId == 'us-gaap_Revenues') %>%
    left_join(inst$context, by = 'contextId') %>%
    filter(is.na(dimension1)) %>%
    select(startDate, endDate, fact, unitId, elementId))

# XBRL has several different types of reports
#table(inst$role$type)

# We are interested in statement reports and we want the
# consolidated statement of operations
role.id <-
    (inst$role %>% filter(type == 'Statement') %>% select(roleId))[3,]


# To find the context elements that are reported on the consolidated
# statement of operations, we need to search the presentation tree from
# the top element

# filter by roleId and convert order to numeric
pres <- 
    inst$presentation %>%
    filter(roleId %in% role.id) %>%
    mutate(order = as.numeric(order))

# start with the top element of the presentation tree
pres.df <-
    pres %>%
    anti_join(pres, by = c("fromElementId" = "toElementId")) %>%
    select(elementId = fromElementId)

# breath-first search
while({
    df1 <- pres.df %>%
        na.omit() %>%
        left_join(pres, by = c("elementId" = "fromElementId")) %>%
        arrange(elementId, order) %>%
        select(elementId, child = toElementId);
    nrow(df1) > 0
}){
    # add each new level to the data frame
    pres.df <- pres.df %>% left_join(df1, by = "elementId")
    names(pres.df) <- c(sprintf("level%d", 1:(ncol(pres.df)-1)), "elementId")
}

# add last level as special column (the hierarchy may not be uniformly deep)
pres.df["elementId"] <-
    apply(t(pres.df), 2, function(x){tail(x[!is.na(x)], 1)})
pres.df["elOrder"] <- 1:nrow(pres.df)

# the final data frame structure
#str(pres.df, vec.len=1)

pres.df.num <-
    pres.df %>%
    left_join(inst$fact, by = "elementId") %>%
    left_join(inst$context, by = "contextId") %>%
    #filter(is.na(dimension1)) %>%
    filter(!is.na(endDate)) %>%
    select(elOrder, contains("level"), contains("dimension"), contains("value"), elementId, fact, scale, endDate) %>%
    #mutate(fact = as.numeric(gsub(',','', fact)) * 10^as.numeric(scale)) %>%
    #pivot_wider(names_from = endDate, values_from = fact) %>%
    arrange(elOrder)

top.level.income.statement <- unique(pres.df.num %>%
    filter(is.na(dimension1)) %>%
    select(endDate, elementId, fact)) %>%
    pivot_wider(names_from = endDate, values_from = fact)

deep.dive.income.statement <- unique(pres.df.num %>%
    filter(endDate == '2020-12-31') %>%
    mutate(
        heading = case_when(
            !is.na(dimension1) ~ value1,
            TRUE ~ elementId
        )
    ))

pres.df.num %>% filter(grepl("[hH]ardware", value1))
#pres.df.num %>% filter(elementId == "us-gaap_CostOfGoodsAndServicesSold", fact == "143,901")
#top.level.income.statement
#test <- deep.dive.income.statement %>% select(heading, elementId, contains("dimension"), contains("value"), fact)
