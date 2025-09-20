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
    # we want the 10-K report of a certian report date for the ticker
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
    filter(!is.na(endDate)) %>%
    select(
        elOrder,
        contains("level"),
        contains("dimension"),
        contains("value"),
        elementId,
        fact,
        scale,
        endDate
    ) %>%
    arrange(elOrder)

top.level.income.statement <- unique(pres.df.num %>%
    filter(is.na(dimension1)) %>%
    select(endDate, elementId, fact)) %>%
    pivot_wider(names_from = endDate, values_from = fact)
#top.level.income.statement

deep.dive.income.statement <- unique(pres.df.num %>%
    filter(endDate == '2020-12-31')) %>%
    mutate(
        heading = case_when(
            !is.na(dimension1) ~ value1,
            TRUE ~ elementId
        )
    )
#deep.dive.income.statement

collected <-
    inst$fact %>%
    left_join(inst$context, by = "contextId") %>%
    left_join(inst$element, by = "elementId") %>%
    select(-c(contextId, factId, ns.x, ns.y, scheme, identifier))

####################### Statement of operations ################################

### Revenue

transaction.based.revenue <-
    unique(collected %>%
    #filter(fact == "3,294,978"))
    filter(
        elementId == "us-gaap_RevenueFromContractWithCustomerExcludingAssessedTax",
        dimension1 == "srt:ProductOrServiceAxis",
        value1 == "sq:TransactionMember",
    ))

subscription.and.service.based.revenue <-
    unique(collected %>%
    #filter(fact == "1,539,403"))
    filter(
        elementId == "us-gaap_Revenues",
        dimension1 == "srt:ProductOrServiceAxis",
        value1 == "sq:SoftwareandDataProductsMember",
        is.na(dimension2),
    ))

hardware.revenue <-
    unique(collected %>%
    #filter(fact == "91,654")
    filter(
        #endDate == "2020-12-31",
        dimension1 == "srt:ProductOrServiceAxis",
        value1 == "sq:HardwareMember",
        #type == "xbrli:monetaryItemType",
        #type == "xbrli:stringItemType",
    ))

bitcoin.revenue <-
    unique(collected %>%
    #filter(fact == "4,571,543"))
    filter(
        elementId == "us-gaap_RevenueFromContractWithCustomerExcludingAssessedTax",
        dimension1 == "srt:ProductOrServiceAxis",
        value1 == "sq:CryptocurrencyDenominatedAssetsMember",
    ))

total.net.revenue <-
    unique(collected %>%
    #filter(fact == "9,497,578") %>%
    filter(
        elementId == "us-gaap_Revenues",
        is.na(dimension1),
        #type == "xbrli:monetaryItemType",
    ))

### Cost of Revenue

transaction.based.costs <-
    unique(collected %>%
    #filter(fact == "1,911,848"))
    filter(
        elementId == "us-gaap_CostOfGoodsAndServicesSold",
        dimension1 == "srt:ProductOrServiceAxis",
        value1 == "sq:TransactionMember",
    ))

subscription.and.services.based.costs <-
    unique(collected %>%
    #filter(fact == "222,712"))
    filter(
        elementId == "us-gaap_CostOfGoodsAndServicesSold",
        dimension1 == "srt:ProductOrServiceAxis",
        value1 == "sq:SoftwareandDataProductsMember",
    ))

hardware.costs <-
    unique(collected %>%
    #filter(fact == "143,901"))
    filter(
        elementId == "us-gaap_CostOfGoodsAndServicesSold",
        is.na(dimension1),
    ))

bitcoin.costs <-
    unique(collected %>%
    #filter(fact == "4,474,534"))
    filter(
        elementId == "us-gaap_CostOfGoodsAndServicesSold",
        dimension1 == "srt:ProductOrServiceAxis",
        value1 == "sq:CryptocurrencyDenominatedAssetsMember",
    ))

amortization.of.acquired.technology <-
    unique(collected %>%
    #filter(fact == "11,174"))
    filter(
        elementId == "us-gaap_CostOfGoodsAndServicesSoldAmortization",
    ))

total.cost.of.revenue <-
    unique(collected %>%
    #filter(fact == "6,764,169"))
    filter(
        elementId == "us-gaap_CostOfRevenue",
    ))

gross.profit <-
    unique(collected %>%
    #filter(fact == "2,733,409"))
    filter(
        elementId == "us-gaap_GrossProfit",
        is.na(dimension1),
    ))

product.development <-
    unique(collected %>%
    #filter(fact == "881,826"))
    filter(
        elementId == "us-gaap_ResearchAndDevelopmentExpense",
        is.na(dimension1),
    ))

sales.and.marketing <-
    unique(collected %>%
    #filter(fact == "1,109,670"))
    filter(
        elementId == "us-gaap_SellingAndMarketingExpense",
        is.na(dimension1),
    ))

general.and.administrative <-
    unique(collected %>%
    #filter(fact == "579,203"))
    filter(
        elementId == "us-gaap_GeneralAndAdministrativeExpense",
        is.na(dimension1),
    ))

transaction.and.loan.losses <-
    unique(collected %>%
    #filter(fact == "177,670"))
    filter(
        elementId == "us-gaap_ProvisionForLoanLeaseAndOtherLosses",
    ))

amortization.of.acquired.customer.assets <-
    unique(collected %>%
    #filter(fact == "3,855")
    filter(
        elementId == "us-gaap_AmortizationOfIntangibleAssets"
    ))

total.operating.expenses <-
    unique(collected %>%
    #filter(fact == "2,752,224"))
    filter(
        elementId == "us-gaap_CostsAndExpenses",
    ))

operating.income.loss <-
    unique(collected %>%
    #filter(fact == "18,815"))
    filter(
        elementId == "us-gaap_OperatingIncomeLoss",
    ))

gain.of.sale.of.asset.group <-
    unique(collected %>%
    #filter(fact == "0"))
    filter(
        elementId == "us-gaap_DisposalGroupNotDiscontinuedOperationGainLossOnDisposal",
        is.na(dimension1),
    ))

interest.expense.net <-
    unique(collected %>%
    #filter(fact == "56,943"))
    filter(
        elementId == "us-gaap_InterestIncomeExpenseNet",
    ))

other.expense.income.net <-
    unique(collected %>%
    #filter(fact == "291,725"))
    filter(
        elementId == "us-gaap_OtherNonoperatingIncomeExpense",
    ))

income.loss.before.income.tax <-
    unique(collected %>%
    #filter(fact == "215,967"))
    filter(
        elementId == "us-gaap_IncomeLossFromContinuingOperationsBeforeIncomeTaxesExtraordinaryItemsNoncontrollingInterest",
    ))

provision.for.income.tax <-
    unique(collected %>%
    #filter(fact == "2,862"))
    filter(
        elementId == "us-gaap_IncomeTaxExpenseBenefit",
    ))

net.income.loss <-
    unique(collected %>%
    #filter(fact == "213,105"))
    filter(
        elementId == "us-gaap_NetIncomeLoss",
        is.na(dimension1),
    ))

net.income.per.share.basic <-
    unique(collected %>%
    #filter(fact == ".48"))
    filter(
        elementId == "us-gaap_EarningsPerShareBasic",
    ))

net.income.per.share.diluted <-
    unique(collected %>%
    #filter(fact == ".44"))
    filter(
        elementId == "us-gaap_EarningsPerShareDiluted",
    ))

weighted.average.number.of.shares.outstanding.basic <-
    unique(collected %>%
    #filter(fact == "443,126"))
    filter(
        elementId == "us-gaap_WeightedAverageNumberOfSharesOutstandingBasic",
    ))

weighted.average.number.of.shares.outstanding.diluted <-
    unique(collected %>%
    #filter(fact == "482,167"))
    filter(
        elementId == "us-gaap_WeightedAverageNumberOfDilutedSharesOutstanding",
    ))
