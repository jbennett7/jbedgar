#' Download an index for a year/quarter.
#'
#' \code{downloadSECQtr} Download  an index for a year/quarter.
#'
#' Downloads an SEC index file for a quarter. This function uses the
#' downloadSECIndex function and thus needs the `USERAGENT` environment
#' variable set.
#'
#' @usage downloadSECQtr(year, qtr)
#'
#' @param year the year to download.
#'
#' @param qtr the quarter to download.
#'
#' @return returns a data.table.
#'
#' @examples
#' \dontrun{
#'
#' link <- "https://www.sec.gov/Archives/edgar/daily-index/2024/QTR4/master.20241004.idx"
#' output <- downloadSECQtr(2020, 3)
#' output
#' }

downloadSECQtr <- function(year, qtr){
  link <- paste0("https://www.sec.gov/Archives/edgar/full-index/", year,
                 "/QTR", qtr, "/master.gz")
  db.path <- Sys.getenv("EDGAR_DB_ROOT", unset="./edgar_index")
  file.path <- paste0(db.path,"/",year,"/QTR", qtr, ".Rda")
  if(file.exists(file.path)){
    load(file.path)
    return(database)
  }else{
    if(! dir.exists(dirname(file.path))){
      dir.create(dirname(file.path),recursive=TRUE)
    }
    database <- downloadSECIndex(link)
    save(database, file=file.path)
    return(database)
  }
}
