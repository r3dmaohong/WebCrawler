#" @title    LineQ Crawler
#" @author   r3dmaohong
#' @keywords lineq, 

# Date format
dateFormatTrans <- function(tmpdate){
  if(grepl("小時", tmpdate)){
    time      <- as.numeric(unique(unlist(regmatches(tmpdate, gregexpr("[0-9]+", tmpdate)))))
    tmpdate <- substr(gsub("-", ".", Sys.time() - time*60*60), 1, unlist(gregexpr(pattern=":", Sys.time() - time*60*60))[length(unlist(gregexpr(pattern=":", Sys.time() - time*60*60)))]-1)
  }else if(grepl("分", tmpdate)){
    time      <- as.numeric(unique(unlist(regmatches(tmpdate, gregexpr("[0-9]+", tmpdate)))))
    tmpdate <- substr(gsub("-", ".", Sys.time() - time*60), 1, unlist(gregexpr(pattern=":", Sys.time() - time*60))[length(unlist(gregexpr(pattern=":", Sys.time() - time*60)))]-1)
  }else if(grepl("天", tmpdate)){
    time      <- as.numeric(unique(unlist(regmatches(tmpdate, gregexpr("[0-9]+", tmpdate)))))
    tmpdate <- paste0(gsub("-", ".", Sys.tmpdate() - time), " 00:00")
  }else{
  }
  return(tmpdate)
}

# Main program of crawling LineQ
lineqCrawler <- function(inputquery, min = 1, max = 9999999, wordquery = TRUE){
  # Doc string
  #' LineQ crawler
  #'
  #' Crawl article content and replies from LineQ.
  #' When access the last page, it will stop by it's own.
  #' 
  #' It will return a list which contains two data frames, 
  #' one is article data frame, the other one is replies data frame.
  #'
  #' @param inputquery Can be words or url. See @param wordquery.
  #' @param min Starting page index. Default is 1.
  #' @param max Last page index. Default is 9999999.
  #' @param wordquery Default is TRUE,which means the inputquery is words, not url. If the input is url, it should be setted as FALSE 
  
  
  # Crawl out articles links from the list pages. ####
  if(wordquery){
    link <- paste0("http://lineq.tw/search/question?q=", gsub(" ", "+", inputquery), "&sort=date&sel=all&page=")
  }else{
    link <- inputquery
  }
  
  article_links <- c()
  for(i in min:max){
    url       <- paste(link, i, sep = "")
    links <- read_html(html_session(url, user_agent(uastring))) %>% html_nodes("p a") %>% html_attr("href")
    if(toString(links) != ""){
      article_links <- c(article_links, links)
      gc()
      cat("\r LineQ Page ",i)
      Sys.sleep(runif(1,2,5))
    }else{
      break
    }
  }
  cat("\n ")
  if(max == 9999999)
    max = i - 1 
  print(paste0("Had accessed to the last page : Page ", max))
  article_links <- unique(article_links) 
  # End ####
  
  # Get data from articles #### 
  articleContent_df <- data.frame("uId" = character(), "Url" = character(), "Date" = character(), "Classification" = character(), "Title" = character(), "Content" = character(), stringsAsFactors = F)
  articleReplies_df <- data.frame("uId" = character(), "Date" = character(), "Replies" = character(), stringsAsFactors = F)
  
  for(i in 1:length(article_links)){
    tryCatch({
      url          <- paste0("http://lineq.tw", article_links[i])
      session      <- html_session(url, user_agent(uastring))
      total_css    <- read_html(session)
      main_content <- total_css %>% html_nodes(".question_content .content_text") %>% html_text() %>% convertUTF8ornot
      replies      <- total_css %>% html_nodes(".reply_content .content_text") %>% html_text() %>% convertUTF8ornot
      article_class<- total_css %>% html_nodes(".question_relations .item") %>% html_text() %>% iconv(., "UTF-8") %>% paste0(collapse = ", ")
      article_title<- total_css %>% html_nodes(".emphasized_sentence") %>% html_text() %>% convertUTF8ornot
      uid          <- gsub("/q/", "", article_links[i], fixed = T)
      
      # Date
      post_date  <- total_css %>% html_nodes(".header_time") %>% html_text() %>% iconv(., "UTF-8")
      sub_date   <- total_css %>% html_nodes(".sub_date") %>% html_text() %>% iconv(., "UTF-8")
      
      post_date <- dateFormatTrans(post_date)
      sub_date  <- sapply(sub_date, dateFormatTrans) %>% unname
      
      articleContent_df <- rbind(articleContent_df, 
                                 data.frame("uId" = uid, "Url" = url, "Date" = post_date, "Classification" = article_class, "Title" = article_title, "Content" = main_content, stringsAsFactors = F))
      articleReplies_df <- rbind(articleReplies_df, 
                                 data.frame("uId" = uid, "Date" = sub_date, "Replies" = replies, stringsAsFactors = F))
      gc()
      
      cat("\r LineQ article ",i, " ==> ", format(round(i/length(article_links)*100, 3), nsmall=3), "% completed ")
      Sys.sleep(runif(1, 2, 5))
    },error=function(e){
    })
  }
  # End ####
  cat("\n ")
  cat("Crawler Completed.")
  return(list("articles" = articleContent_df,  "replies" = articleReplies_df))
}

# Brief description.
details <- function(obj) attr(obj, "help")
attr(lineqCrawler, "help") <- "Lineq crawler. Input will be url or word. See other details by 'docstring::docstring(lineqCrawler)'"
