library(tm)
library(ngram)

BLANK_SPACE <- ""

SystemConstants = list(
  PATH_BAD_WORDS = "./dataset/swearWords.txt",
  ULR_BAD_WORDS = "http://www.bannedwordlist.com/lists/swearWords.txt"
)


# Errors
SystemErrors = list(
  EMPTY_FILE = "The file is empty.",
  FILE_NOT_FOUND = "The file wasn't found.",
  INVALID_PARAMETER_VALUE = "The paramater has an invalid value."
  
)

# Returns a sentence without twitter characters
remove_twitter_characters <- function(sentence){
  
  # Removing URLs
  sentence <- gsub("\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)", BLANK_SPACE, sentence)
  
  # Removing hash symbol
  sentence <- gsub("#", BLANK_SPACE, sentence)
  
  return(sentence)
}

# Replace the pattern with blank space character
blank_space_replace <- function(data, pattern){
  return(gsub(pattern, BLANK_SPACE, data))
}

# Preprocessing corpus, removing punctuations, twitter characteres, stopwords and bad words
preprocessing_corpus <- function(corpus, badWordsFilePath = SystemConstants$PATH_BAD_WORDS){
  
  # Removing punctuations
  corpus <- tm_map(corpus, removePunctuation)
  
  # Removing numbers
  corpus <- tm_map(corpus, removeNumbers)
  
  # Setting to lowercase
  corpus <- tm_map(corpus, content_transformer(tolower))
  
  # Removing special characters
  corpus <- tm_map(corpus, content_transformer(blank_space_replace), "–|“|”|-|_")
  
  # Removing twitter characters
  corpus <- tm_map(corpus, content_transformer(remove_twitter_characters))
  
  # Removing stopwords in english
  corpus <- tm_map(corpus, removeWords, stopwords(kind = "en"))
  
  # Dowloading bad words file if doesn't exist
  if(!file.exists(badWordsFilePath))
    download.file(SystemConstants$ULR_BAD_WORDS, destfile = badWordsFilePath)
  
  # Reading bad words file
  badWords <- readLines(badWordsFilePath)
  vsBadWords <- VectorSource(badWords)
  
  # Removing bad Words
  corpus <- tm_map(corpus, removeWords, vsBadWords)
  
  return(corpus)
  
}

# Read the file(s) and create corpus according sample percentage parameter
get_corpus_from_file <- function(strFilePath, samp = 1.0, verbose = F, seed = 1505){
  
  # Checking parameters values
  if(samp <= 0 || samp > 1)
    stop(SystemErrors$INVALID_PARAMETER_VALUE)
  
  # Total corpus
  corpus <- NULL
  
  # Looping trough files
  for(filePath in strFilePath){
    
    if(!file.exists(filePath))
      stop(SystemErrors$FILE_NOT_FOUND)
    
    # File Connection 
    fileConn <- file(filePath, encoding = "UTF-8")
    
    # Reading the file
    fileContent <- readLines(fileConn)
    fileLines <- length(fileContent)
    
    # Closing connection
    close(fileConn)
    
    if(fileLines == 0)
      stop(SystemErrors$EMPTY_FILE)
    
    # Getting lines to read according sample number
    linesToRead <- as.numeric(ceiling(samp * fileLines))
    set.seed(seed)
    sampleLines <- fileContent[sort(sample(1:fileLines, linesToRead))]
    sampleCorpus <- VCorpus(VectorSource(sampleLines))
    
    if(verbose) print(paste0("File:", basename(filePath) , ", # File Lines:", fileLines, ", # Sample Lines:", linesToRead))
    
    # Merging corpus if it's not null
    if(is.null(corpus)) corpus <- sampleCorpus
    else corpus <- c(corpus, sampleCorpus)
    
  }
  
  return(corpus)
}


strFileTemp <- c("./dataset/en_US/en_US.blogs.txt", "./dataset/en_US/en_US.news.txt", "./dataset/en_US/en_US.twitter.txt")
corpus <- get_corpus_from_file(strFileTemp, samp = 0.1)
corpus <- preprocessing_corpus(corpus)


# Creating Document Matrix
bigramdocumentmatrix <- TermDocumentMatrix(corpus, control = list(tokenize = bigram_tokenizer))

# Creating 
bigramFreq <- findFreqTerms(bigramdocumentmatrix, lowfreq = 50)





