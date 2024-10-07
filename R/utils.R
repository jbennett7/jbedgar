is.Date <- function(x) inherits(x, "Date")

is.weekday <- function(date){
  if(! is.Date(date)){
    cat(date, "is not of class `Date`\n")
    return(FALSE)
  }
  return(date %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
}
