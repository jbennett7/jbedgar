if(length(find.package('jbedgar', quiet = T) > 0)){
    remove.packages('jbedgar')
    message('jbedgar is removed')
    quit(save = "no", status = 0, runLast = T)
}

Sys.setenv(GITHUB_PAT = readLines('~/.github_token'))
remotes::install_github('jbennett7/jbedgar')
ua = readLines('./.ignore/useragent')
options(HTTPUserAgent = ua)

names(jbedgar::submissions('xyz'))
