downloadSECIndex <- function(link) {
  UA <- Sys.getenv("USERAGENT")
  if(UA == ""){
    cat("User Agent is not set. Please set environment variable 'USERAGENT'\n")
    return(FALSE)
  }

  filename <- basename(link)
  download.path <- paste0("/tmp/", filename)
  req <- request(link) |> req_headers("User-Agent" = UA)
  resp <- req |> req_perform(path=download.path)

  data.path <- paste0("/tmp/", tools::file_path_sans_ext(filename), ".txt")

  if(file.exists(data.path)){
    file.remove(data.path)
  }

  if(tools::file_ext(filename) == "gz"){
    R.utils::gunzip(download.path, destname=data.path)
    dat <- read.SECIndex(data.path)
    dat$Date.Filed <- as.Date(dat$Date.Filed)
  }else{
    file.rename(download.path, data.path)
    dat <- read.SECIndex(data.path)
    dat$Date.Filed <- as.Date(as.character(dat$Date.Filed), "%Y%m%d")
  }
  file.remove(data.path)
  return(dat)
}
