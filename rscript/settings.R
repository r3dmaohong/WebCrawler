#' @title Default settings for crawler.
#' @author r3dmaohong
#' 
##libraries
libraries <- c("rvest", "XML", "RCurl", "httr", "rjson", "stringr", "data.table")
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
  #inx <- which(apply(obj, 2, function(x) length(unique(Encoding(x))))>1)
  obj <- apply(obj, 2, function(x) ifelse(is.na(iconv(x, to = "UTF-8")), iconv(x, from = "UTF-8", to = "UTF-8",sub=''), iconv(x, to = "UTF-8")))
  obj <- as.data.frame(obj, stringsAsFactors = F)
  setDT(obj)
  obj[, names(obj) := lapply(.SD, function(x) {if (is.character(x)) Encoding(x) <- "unknown"; x})]
  setDF(obj)
  obj <- apply(obj, 2, function(x) iconv(x, "UTF-8"))
  obj <- apply(obj, 2, function(x) iconv(x, to = "UTF-8"))
  obj <- as.data.frame(obj, stringsAsFactors = F)
  
  write(toJSON(unname(split(obj, 1:nrow(obj)))), 
        file(fn, encoding="UTF-8"))
}
read_jsonf <- function(fn){
  tmp <- readChar(fn, file.info(fn)$size)
  tmp <- str_replace_all(iconv(tmp, "UTF-8"), perl('\\\\(?![tn"])'), '\\\\\\\\')
  do.call("rbind", fromJSON(tmp))
}
