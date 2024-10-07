#' Download an index for a certain day.
#'
#' \code{downloadSECQtr} Download  an index for a certain day.
#'
#' Downloads an SEC index file for a day. This function uses the
#' downloadSECIndex function and thus needs the `USERAGENT` environment
#' variable set.
#'
#' @usage downloadSECDaily(date)
#'
#' @param date a `Date` type object for the date to download.
#'
#' @return returns a data.table.
#'
#' @examples
#' \dontrun{
#'
#' link <- "https://www.sec.gov/Archives/edgar/daily-index/2024/QTR4/master.20241004.idx"
#' output <- downloadSECDaily("2024-10-04")
#' output
#' }

downloadSECDaily <- function(date){
  if(! is.Date(date)){
    return(FALSE)
  }
  yr <- year(date)
  mth <- month(date)
  day <- {
    if((day <- mday(date)) < 10)
      paste0("0", day)
    else
      day
  }
  ymd <- as.character(paste0(yr,mth,day))
  link <- paste0("https://www.sec.gov/Archives/edgar/daily-index/",
                 yr,"/QTR", ceiling(mth/3), "/master.",
                 ymd,".idx")
  db.path <- Sys.getenv("EDGAR_DB_ROOT", unset="./edgar_index")
  file.path <- paste0(db.path, "/", yr, "/Daily/", ymd, ".Rda")
  if(file.exists(file.path)){
    load(file.path)
    return(database)
  }else{
    if(! dir.exists(dirname(file.path))){
      dir.create(dirname(file.path), recursive=TRUE)
    }
    database <- downloadSECIndex(link)
    save(database, file=file.path)
    return(database)
  }
  return(file.path)
}
