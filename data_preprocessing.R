library(stringr)

BLANK_SPACE <- ""

# Returns tokens that are not in the list of profanity words
tokens_profanity_filter <- function(tokens){
  
  # Profanity list
  profanityWords <- c('anal', 'asshole', 'ball sucking', 'bastard', 'big breasts', 'bitch',
                         'bitches', 'blowjob', 'boob', 'boobs', 'bullshit', 'butthole', 'cock', 
                         'cocks', 'cum', 'dick', 'fuck', 'fucking', 'gay', 'genitals', 'hooker',
                         'masturbation', 'milf', 'motherfucker', 'nigga', 'nipple', 'nude', 
                         'orgasm', 'orgy', 'porn', 'pussy', 'semen', 'sex', 'slut', 'sucks',
                         'suck', 'tits', 'tit', 'vagina')
  
  profanityFound <- !(tokens %in% profanityWords)
  
  return(tokens[profanityFound])
  
}

# Returns a cleaned sentence
sentence_filter <- function(sentence){
  
  # To Lower
  newSentence <- tolower(sentence)
  
  # Removing URLs
  newSentence <- gsub("\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)", BLANK_SPACE, newSentence)
  
  # Removing hash symbol
  newSentence <- gsub("#", BLANK_SPACE, newSentence)
  
  # Remove everything that isn't a word or number
  newSentence <- str_replace_all(newSentence, "[^a-zA-Z\\s]", BLANK_SPACE)
  
  return(sentence)
}

# Receives a sentence (character variable) and return a list of tokens
sentence_tokenization <- function(sentence){
  
  # Cleaning the sentence
  newSentence <- sentence_filter(sentence)
  
  # Split it
  tokens <- str_split(newSentence, " ")
  
  # Get rid of trailing "" if necessary
  indexes <- which(tokens == "")
  if(length(indexes) > 0){
    tokens <- tokens[-indexes]
  }
  
  # Removing profanity words
  tokens <- tokens_profanity_filter(tokens)
  
  return(tokens)
}

# Read a file a return a list of tokens
file_tokenization <- function(strFilePath){
  
  # File Connection 
  fileConn <- file(strFilePath, "r")
  
  # Reading the file
  fileContent <- readLines(fileConn)
  
  # Creating list
  tokenList <- list()
  
  # Iterating over sentences
  for(sentence in fileContent){
    tokens <- sentence_tokenization(sentence)
    
    # Adding to a list of tokens
    if(length(tokens) > 0)
      tokenList[[length(tokenList) + 1]] <- tokens
    
  }
  
  # Closing connection
  close(fileConn)
  
  return(tokenList)
}


strFileTemp <- "./dataset/en_US/en_US.blogs.txt"

# File Connection 
fileConn <- file(strFileTemp, "r")

# Reading the file
fileContent <- readLines(fileConn)


# Closing connection
close(fileConn)


gsub("#", "", "Yo solo #espero que se encuentren #SuperBien")







