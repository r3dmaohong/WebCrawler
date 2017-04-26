#" @title    ptt Crawler
#" @author   r3dmaohong
#' @keywords  


# Main program of crawling ptt
pttCrawler <- function(inputquery, min = 1, max = 9999999, wordquery = TRUE){
  # Doc string
  #' ptt crawler
  #'
  #' Crawl article content and replies from ptt.
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
    link <- paste0("https://www.ptt.cc/bbs/", inputquery, "/index")
  }else{
    gregexpr("/", inputquery)
    link <- inputquery
  }
  
  if(max == 9999999){
    # Get the latest page's index
    url <- paste0(link, ".html")
    getmax <- read_html(html_session(url, user_agent(uastring))) %>% html_nodes(".action-bar a") %>% html_attr("href")
    urlindex <- as.numeric(unlist(regmatches(getmax, gregexpr("[[:digit:]]+", getmax))))
    max <- urlindex[which.max(urlindex)] + 1
  }
  
  article_links <- c()
  for(i in min:max){
    Sys.sleep(runif(1,2,5))
    tmp <- paste0(i, ".html")
    url <- paste0(link, tmp)
  
    links <- read_html(html_session(url, user_agent(uastring))) %>% html_nodes(".title a") %>% html_attr("href")
    
    article_links <- c(article_links, links)
    gc()
    cat("\r ptt Page ",i)
  }
  
  cat("\n ")
  
  print(paste0("Had accessed to the last page : Page ", max))
  article_links <- unique(article_links) 
  # End ####
  
  # Get data from articles #### 
  articleContent_df <- data.frame("uId" = character(), "Url" = character(), "Date" = character(), "Classification" = character(), "Title" = character(), "Content" = character(), stringsAsFactors = F)
  articleReplies_df <- data.frame("uId" = character(), "Date" = character(), "Replies" = character(), stringsAsFactors = F)
  
  for(i in 1:length(article_links)){
    tryCatch({
      url          <- paste0("http://ptt.cc", article_links[i])
      session      <- html_session(url, user_agent(uastring))
      total_css    <- read_html(session)
      
      
      uid          <- gsub(".html", "", str_split_fixed(article_links[i], "/", 4)[1,4], fixed = T)
      article_class<- total_css %>% html_nodes("#topbar .board") %>% html_text() %>% convertUTF8ornot %>% gsub("看板", "", .) %>% trimws(.)
      main_content <- total_css %>% html_nodes("#main-content") %>% html_text() %>% convertUTF8ornot
      meta_data    <- total_css %>% html_nodes("#main-content .article-metaline") %>% html_text() %>% convertUTF8ornot
      pushs       <- total_css %>% html_nodes("#main-content .push") %>% html_text()
      
      main_content <- substr(main_content, gregexpr("\n", main_content, fixed=TRUE)[[1]][1], gregexpr(pushs[1], main_content, fixed=TRUE)[[1]][1] - 3)
      
      if(any(sapply(meta_data, function(x) grepl("時間", x)))){
        post_date <- sub("時間", "", meta_data[grepl("時間", meta_data)])
      }else{
        post_date <- ""
      }
      if(any(sapply(meta_data, function(x) grepl("標題", x)))){
        article_title <- sub("標題", "", meta_data[grepl("標題", meta_data)])
      }else{
        article_title <- ""
      }
      
      
      replies  <- total_css %>% html_nodes(".push-content") %>% html_text() %>% convertUTF8ornot %>% sub(": ", "", .)
      #reply_id <- total_css %>% html_nodes(".push-userid") %>% html_text() 
      sub_date  <- total_css %>% html_nodes(".push-ipdatetime") %>% html_text()
      
      articleContent_df <- rbind(articleContent_df, 
                                 data.frame("uId" = uid, "Url" = url, "Date" = post_date, "Classification" = article_class, "Title" = article_title, "Content" = main_content, stringsAsFactors = F))
      articleReplies_df <- rbind(articleReplies_df, 
                                 data.frame("uId" = uid, "Date" = sub_date, "Replies" = replies, stringsAsFactors = F))
      gc()
      
      cat("\r ptt article ",i, " ==> ", format(round(i/length(article_links)*100, 3), nsmall=3), "% completed ")
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
attr(pttCrawler, "help") <- "ptt crawler. Input will be url or word. See other details by 'docstring::docstring(pttCrawler)'"
