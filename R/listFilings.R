listFilings <- function(ticker){
    subs <- getSubmissions(ticker)

    ret  <- as_tibble (subs$filings) |>
        filter (isXBRL == 1) |>
        mutate(
            # Remove dashes from accessionNumber to generate document link.
            tmp  = gsub ("-", "", accessionNumber),
            link = paste (edgar.filing.url,
                          'edgar/data',
                          as.numeric(subs$cik),
                          tmp,
                          primaryDocument,
                          sep='/'),
            link2 = paste (edgar.filing.url,
                          'edgar/data',
                          as.numeric(subs$cik),
                          tmp,
                          paste0(gsub('\\.','_', primaryDocument), '.xml'),
                          sep='/'),
        ) |>
        select (-tmp)

        return (ret)
}
