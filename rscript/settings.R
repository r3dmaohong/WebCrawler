#' @title Default settings for crawler.
#' @author r3dmaohong
#' 
##libraries
libraries <- c("rvest", "XML", "RCurl", "httr", "rjson", "stringr")
invisible(sapply(libraries, function(x){
  if(x!="tmcn" & !is.element(x, installed.packages()[,1])){
    install.packages(x)#, dependencies=TRUE)
  }else if(x=="tmcn" & !is.element(x, installed.packages()[,1])){
    install.packages("http://download.r-forge.r-project.org/src/contrib/tmcn_0.1-4.tar.gz", repos =NULL)
  }
  library(x, character.only = TRUE)
}))

uastring <- "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"

# iconv or not
convertUTF8ornot <- function(x){
  ifelse(is.na(iconv(x, "UTF-8")), x, iconv(x, "UTF-8"))
}

# Homemade r/w json
write_jsonf <- function(obj, fn){
  write(toJSON(unname(split(obj, 1:nrow(obj)))), 
        file(fn, encoding="UTF-8"))
}
read_jsonf <- function(fn){
  tmp <- readChar(fn, file.info(fn)$size)
  tmp <- str_replace_all(iconv(tmp, "UTF-8"), perl('\\\\(?![tn"])'), '\\\\\\\\')
  do.call("rbind", fromJSON(tmp))
}



