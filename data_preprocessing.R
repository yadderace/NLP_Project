library(tm)

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


# Preprocessing corpus, removing punctuations, twitter characteres, stopwords and bad words
preprocessing_corpus <- function(corpus, badWordsFilePath = SystemConstants$PATH_BAD_WORDS){
  
  # Removing punctuations
  corpus <- tm_map(corpus, removePunctuation)
  
  # Setting to lowercase
  corpus <- tm_map(corpus, content_transformer(tolower))
  
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


# Read a file a return a list of tokens
preprocessing_file <- function(strFilePath, samp = 1.0){
  
  # Checking parameters values
  
  if(!file.exists(strFilePath))
    stop(SystemErrors$FILE_NOT_FOUND)
    
  if(samp <= 0 || samp > 1)
    stop(SystemErrors$INVALID_PARAMETER_VALUE)
  
  
  # File Connection 
  fileConn <- file(strFilePath, "r")
  
  # Reading the file
  fileContent <- readLines(fileConn)
  fileLines <- length(fileContent)
  
  # Closing connection
  close(fileConn)
  
  if(fileLines == 0)
    stop(SystemErrors$EMPTY_FILE)
  
  # Getting lines to read according sample number
  linesToRead <- as.numeric(ceiling(samp * fileLines))
  sampleLines <- fileContent[sort(sample(1:fileLines, linesToRead))]
  
  sampleCorpus <- VCorpus(VectorSource(sampleLines))
  
  # Preprocessing the corpus
  sampleCorpus <- preprocessing_corpus(sampleCorpus)
  
  return(sampleCorpus)
}


strFileTemp <- "./dataset/en_US/en_US.blogs.txt"

corpus <- preprocessing_file(strFileTemp, samp = 0.1)



