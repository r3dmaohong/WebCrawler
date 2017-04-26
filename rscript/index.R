##index
rm(list = ls()) #Remove all objects in the environment
gc() ##Free up the memory

getwd()
setwd("WebCrawler")

source("rscript\\settings.R", print.eval  = TRUE, encoding="UTF-8")
source("rscript\\ptt.R", print.eval  = TRUE, encoding="UTF-8")
# UTF8 problems : source(".\\rscript\\lineq.R", print.eval  = TRUE, encoding="UTF-8")
eval(parse("rscript\\lineq.R", encoding="UTF-8"))

# LineQ example ####
details(lineqCrawler)
docstring::docstring(lineqCrawler)

query       <- "µØ®L"
lineqResult <- lineqCrawler(query)

getwd()
fn     <- paste0("LineQ_", query)
fndate <- format(Sys.Date(), "%Y%m%d")
dir.create(paste0("output/", query, "/", fndate), recursive = TRUE)

write.csv(lineqResult$articles, paste0("output/", query, "/", fndate, "/", fn, "_articles.csv"), row.names = FALSE)
write.csv(lineqResult$replies, paste0("output/", query, "/", fndate, "/", fn, "_replies.csv"), row.names = FALSE)
write_jsonf(lineqResult$articles, paste0("output/", query, "/", fndate, "/", fn, "_articles.json"))
write_jsonf(lineqResult$replies, paste0("output/", query, "/", fndate, "/", fn, "_replies.json"))
#tmp <- read_jsonf(paste0("output/", query, "/", fndate, "/", fn, "_articles.json"))
# End ####
if(F){
  x1 <- 'pretty\\u003D\\u003Ebig'
  x2 <- parse(text = paste0("'", x1, "'"))
  x3 <- x2[[1]]
  x3
  # [1] "pretty=>big"
  is.character(x3)
  # [1] TRUE
  length(x3)
  # [1] 1
}

## Ptt Cralwer
query       <- "SENIORHIGH"
pttResult <- pttCrawler(query, min = 1000)

getwd()
fn     <- paste0("ptt_", query)
fndate <- format(Sys.Date(), "%Y%m%d")
dir.create(paste0("output/", query, "/", fndate), recursive = TRUE)

write.csv(pttResult$articles, paste0("output/", query, "/", fndate, "/", fn, "_articles.csv"), row.names = FALSE)
write.csv(pttResult$replies, paste0("output/", query, "/", fndate, "/", fn, "_replies.csv"), row.names = FALSE)
write_jsonf(pttResult$articles, paste0("output/", query, "/", fndate, "/", fn, "_articles.json"))
write_jsonf(pttResult$replies, paste0("output/", query, "/", fndate, "/", fn, "_replies.json"))
#tmp <- read_jsonf(paste0("output/", query, "/", fndate, "/", fn, "_articles.json"))


##auto-crawling
#min <- 1
while(F){
  ptt         <- ptt_list_crawler("https://www.ptt.cc/bbs/Soft_Job/index.html", min)
  min         <- ptt$max
  output_ptt  <- ptt_article_crawler(ptt)
  
  # re-start by tmp file
  # output_ptt  <- ptt_article_crawler()
  
  sleep_time <- runif(1,86000,86400)
  print("next time to operate...")
  print(Sys.time() + sleep_time)
  Sys.sleep(sleep_time)
}