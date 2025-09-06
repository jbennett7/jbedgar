suppressMessages(require(jsonlite))
suppressMessages(require(zoo))
suppressMessages(require(tidyverse))

ua <- readLines('.ignore/useragent')
options(HTTPUserAgent = ua)

source("R/get_cik.R")

submissions.url <- function(cik) {
    paste0("https://data.sec.gov/submissions/CIK", cik, ".json")
}
concept.url <- function(cik, concept){
  paste0("https://data.sec.gov/api/xbrl/companyconcept/CIK",
         cik, namespace, concept, ".json")
}
facts.url <- function(cik) {
  paste0("https://data.sec.gov/api/xbrl/companyfacts/CIK",
         cik, ".json")
}

########### Concept
#cpt <- "CashAndCashEquivalentsAtCarryingValue"
#cpt <- "CashAndCashEquivalentsSegregatedForTheBenefitOfStablecoinHolders"
#cpt <- 'Revenues'
#data <- fromJSON(concept.url(cik, cpt))
#save(data, file='./.ignore/data/xyz_revenues.Rdata')
#load('./.ignore/data/xyz_revenues.Rdata')
#data$units
#data$taxonomy
#data$description
########### Submissions
#data <- fromJSON(submissions.url(get_cik('crcl')))
#names(data)
#filings <- as_tibble(data$filings$recent)
#as.data.frame(filings %>% filter(form == "10-Q"))

############ Facts
#cik <- get_cik('xyz')
#data <- fromJSON(facts.url(cik))
#save(data, file='./.ignore/data/xyz_facts.Rdata')
#load('./.ignore/data/xyz_facts.Rdata')
#names(data$facts$`us-gaap`)
#names(data$facts$`us-gaap`$Revenues)
#revenues <- data$facts$`us-gaap`$Revenues
#revenues
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
#assets <- as_tibble(fdata[["Assets"]]$units$USD)
#write_csv(assets, file='./.ignore/data/xyz_assets.csv')
#assets %>% filter(!is.na(frame))
#assets <- suppressMessages(read_csv('./.ignore/data/xyz_assets.csv')) %>%
#    filter(!is.na(frame)) %>%
#    select(!filed)

#cy2025 <- str_detect(assets$frame, "CY2025")
#q1 <- str_detect(assets$fp, "Q1")
#q1assets <- assets[q1,c("end", "val")]
#plot(x=q1assets$end, y=q1assets$val)
#liabilities <- fdata[["Liabilities"]]$units$USD$val
#(assets - liabilities) / assets
#equity <- fdata[["StockholdersEquity"]]$units$USD
#equity
#
#fdata[grep("CashAndCashEquivalentsSegregatedForTheBenefitOfStablecoinHolders", facts)]
#grep("CashAndCashEquivalentsSegregated", facts)
#
#liabilities/equity
