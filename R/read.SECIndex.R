read.SECIndex <- function(file.path){
  dat <- read.raw(file.path)
  idx <- grep("----+", dat)
  last <- length(dat)
  header <- gsub(" ", ".", strsplit(dat[idx-1], split="\\|")[[1]])
  dat <- fread(text=dat[(idx+1):last])
  colnames(dat) <- header
  return(dat)
}
