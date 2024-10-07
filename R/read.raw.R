read.raw <- function(file.path) {
  conn <- file(file.path, 'r')
  ret <- readLines(conn)
  close(conn)
  return(ret)
}
