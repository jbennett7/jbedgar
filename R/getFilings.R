getFilings <- function(ticker){
    subs <- submissions(ticker)
    ret <- as_tibble(subs$filings$recent) %>%
        filter(isXBRL == 1) %>%
        mutate(
            accessionNumber = gsub("-", "", accessionNumber),
            instance.document = ifelse(
                str_detect(primaryDocDescription, '10-(K|Q)'),
                paste0(gsub("\\.", "_", primaryDocument), '.xml'),
                primaryDocument
            ),
            link = paste0(base.url, '/edgar/data/', as.numeric(subs$cik),
                          '/', accessionNumber, '/', instance.document),
        )
        return(ret)
}
