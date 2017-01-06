##jiebar main program.
##Include noun extraction

##libraries
libraries <- c("jiebaR", "dplyr", "data.table", "text2vec", "tmcn")
invisible(sapply(libraries, function(x){
  if(x!="tmcn" & !is.element(x, installed.packages()[,1])){
    install.packages(x)#, dependencies=TRUE)
  }else if(x=="tmcn" & !is.element(x, installed.packages()[,1])){
    install.packages("http://download.r-forge.r-project.org/src/contrib/tmcn_0.1-4.tar.gz", repos =NULL)
  }
  library(x, character.only = TRUE)
}))


##
cat("\njiebaR_n <- function(x_data, forum_name = \"\", recent = 0, last = 0)")
jiebaR_n <- function(x_data, forum_name = "", recent = 0, last = 0){
  ##
  ##Import custom words library...
  admin_check <- readline("Are you the administrator? (y or n): ")
  if(admin_check=="y"){
    check_files <- list.files("..\\school_performence_analysis\\")
    if(("__處理後公司名稱.csv" %in% check_files) & ("學校名稱正規化表格.csv" %in% check_files)){
      co.names     <- fread("..\\school_performence_analysis\\__處理後公司名稱.csv")
      customWords  <- unique(c(co.names$company,co.names$最終比對結果))
      school.names <- fread("..\\school_performence_analysis\\學校名稱正規化表格.csv",stringsAsFactors=F)
      customWords  <- unique(c(customWords,school.names$trim後原始,school.names$對應表))
    }else{
      cat("\nNo, You're not!")
    }
    
  }
  customW_check  <- readline("Do you have any custom words? (y or n): ")
  if(customW_check==T){
    if("terms_DB.csv" %in% list.files){
      customWords  <- c(customWords, read.csv("terms_DB.csv",stringsAsFactors=F)[,1]) 
    }else{
      customWords  <- c(customWords, read.csv(file.choose(),stringsAsFactors=F)[,1])
    }
  }
  ##
  if(!exists("customWords"))
    customWords <- ""
  ##
  word_DB <- tolower(customWords)
  
  ##jiebar cutter
  cutter=worker("tag", bylines = T)
  #sapply(temp,function(x) new_user_word(cutter,x,"n"))
  ##Setting words as new user words
  for(xj in 1:length(word_DB)){
    new_user_word(cutter,word_DB[xj],"n")
  }
  
  ##Extract words which is noun.
  get_noun <- function(x){
    stopifnot(inherits(x,"character"))
    index = names(x) %in% c("n","nr","nr1","nr2","nrj","nrf","ns","nsf","nt","nz","nl","ng")
    x[index]
  }
  
  jieba_x      <- {}
  jieba_x_noun <- {}
  x_data       <- tolower(x_data)
  
  #x_data = x_data[,2]
  #Remove all punctuation except comma[^[:alnum:],]
  x_data = gsub('[^[:alnum:]]','',x_data)
  
  cat("\nStart using jiebar cutter...")
  jieba_x      <- lapply(x_data, function(x) cutter <=x)
  jieba_x      <- lapply(jieba_x, '[[', 1)
  jieba_x_noun <- lapply(jieba_x, function(x) get_noun(unlist(x)))
  cat("\n")
  
  ##Data extraction
  data_ep <- function(x){
    ##Change list of words to table
    a.token <- itoken(x)
    a.vocab <- create_vocabulary(a.token, ngram=c(1, 1))
    #class(a.vocab$vocab)
    a.vocab$vocab$terms <- a.vocab$vocab$terms %>% toUTF8()
    a.vocab$vocab       <- a.vocab$vocab[order(-a.vocab$vocab$terms_counts),]
    
    x_cdf <- a.vocab$vocab
    ##Remove words which nchar==1.
    x_cdf = x_cdf[which(nchar(x_cdf$terms)>1),]
    ##Remove words with num...(ex. IDs)
    #x = x[which(!grepl('[0-9]',x))]
    #x = tolower(x)
    
    return(x_cdf)
  }
  
  ##DF: terms counts
  jieba_x_cdf = data_ep(jieba_x)
  jieba_x_n_cdf = data_ep(jieba_x_noun)
  
  if(forum_name==""){
    forum_name <- readline("Enter folder name where you want to export: ")
  }
  dir.create(paste0(".\\output\\", forum_name, "\\after jiebar"), showWarnings = FALSE)
  
  write.csv(jieba_x_cdf, paste0(".\\output\\", forum_name, "\\after jiebar\\", format(Sys.time(), "%Y_%m_%d_%H%M%OS_"), "jieba", forum_name, "_", recent, "_", last, ".csv"), row.names=F)
  write.csv(jieba_x_n_cdf, paste0(".\\output\\", forum_name, "\\after jiebar\\", format(Sys.time(), "%Y_%m_%d_%H%M%OS_"), "jieba名詞", forum_name, "_", recent, "_", last, ".csv"), row.names=F)
  
  #co.names     <- co.names[,list(company, 最終比對結果)] %>% unique
  #school.names <- school.names[, 1:2, with=F] %>% unique
  #colnames(co.names)     <- c("before", "after")
  #colnames(school.names) <- c("before", "after")
  #tmp3 = rbind(co.names, school.names)
  
  removeWordOutput <- function(x, filename){
    inter_list    <- intersect(x$terms, word_DB)
    AfterRemoveDF <- x[which(x$terms %in% inter_list),]
    
    ##Word which should be removed. 
    word_remove   <- read.table("應剔除字串.txt")
    word_remove   <- word_remove[,1]
    AfterRemoveDF <- AfterRemoveDF[which(!(AfterRemoveDF$terms %in% word_remove)),]  
    
    write.csv(AfterRemoveDF, paste0(".\\output\\", forum_name, "\\after jiebar", "\\", format(Sys.time(), "%Y_%m_%d_%H%M%OS_"), forum_name, "_", recent, "_", last, "剔除字串結果", filename, ".csv"), row.names=F)
  }
  
  if(exists("jieba_x_cdf") & ("應剔除字串.txt" %in% list.files())){
    removeWordOutput(jieba_x_cdf, "整體")  
  }
  if(exists('jieba_x_n_cdf')){
    removeWordOutput(jieba_x_n_cdf, "名詞")
  }
  if(!exists('jieba_x_n_cdf') & !exists('jieba_x_cdf')){
    cat("\njiebar failed.")
  }else{
    cat("\n", forum_name, " Chinese text segmentation and keyword extraction Completed.")
  }
  
}
