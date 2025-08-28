suppressMessages(require(jsonlite))
suppressMessages(require(zoo))
suppressMessages(require(tidyverse))

ua <- readLines('./.useragent')
options(HTTPUserAgent = ua)

source("R/get_cik.R")

#get_cik('crcl')

submissions.url <- function(cik) {
    paste0("https://data.sec.gov/submissions/CIK", cik, ".json")
}
concept.url <- function(cik, concept){
  paste0("https://data.sec.gov/api/xbrl/companyconcept/CIK",
         cik, "/us-gaap/", concept, ".json")
}
facts.url <- function(cik) {
  paste0("https://data.sec.gov/api/xbrl/companyfacts/CIK",
         cik, ".json")
}

########### Concept
#cpt <- "CashAndCashEquivalentsAtCarryingValue"
#cpt <- "CashAndCashEquivalentsSegregatedForTheBenefitOfStablecoinHolders"
#cpt <- 'Assets'
#data <- fromJSON(concept.url(get_cik('crcl'), cpt))
#x11()
#df <- zoo(data$units$USD$end, data$units$USD$val)
#plot(df)
#Sys.sleep(30)

########### Submissions
#data <- fromJSON(submissions.url(get_cik('crcl')))
#names(data)
#filings <- as_tibble(data$filings$recent)
#as.data.frame(filings %>% filter(form == "10-Q"))


############ Facts
#data <- fromJSON(facts.url(get_cik('crcl')))
#names(data)
#names(data$facts)
#facts <- names(data$facts$`us-gaap`)
#
#fdata <- data$facts$`us-gaap`
#fdata[["Current Liabilities"]]
#fdata[grep("Liabilities", facts),]
#facts[grep("Liabilities", facts)]
#facts[grep("Assets", facts)]
#
#assets <- fdata[["Assets"]]$units$USD$val
#liabilities <- fdata[["Liabilities"]]$units$USD$val
#(assets - liabilities) / assets
#equity <- fdata[["StockholdersEquity"]]$units$USD
#equity
#
#fdata[grep("CashAndCashEquivalentsSegregatedForTheBenefitOfStablecoinHolders", facts)]
#grep("CashAndCashEquivalentsSegregated", facts)
#
#liabilities/equity
