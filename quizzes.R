
#########################
#       QUIZ 1          #
#########################

strDir <- "./dataset/en_US/"

# Getting documents from directory
strFiles <- list.files(strDir, pattern = ".*.txt")



########################## Question 3



for (file in strFiles){
  
  # File Connections 
  fileConn <- file(paste0(strDir, file), "r")
  
  # Reading lines of file
  fileContent <- nchar(readLines(fileConn))
  

  
  # Printing information
  print("=========================================")
  print(paste0("File: ", file))
  print(paste0("Lines: ", length(fileContent)))
  print(paste0("Length of Longest Line: ", max(fileContent)))
  
  # Closing connection
  close(fileConn)

}


########################## Question 4

# Reading twitter data

# File Connections 
fileConn <- file(paste0(strDir, strFiles[3]), "r")

# Reading lines of file
fileContent <- readLines(fileConn)

# Getting lines with word 'love'
loveLines = fileContent[grepl("love", fileContent)]

# Getting lines with word 'hate'
hateLines = fileContent[grepl("hate", fileContent)]

print(paste0("#Love / #Hate = ", length(loveLines)/ length(hateLines)))


########################## Question 5

# Getting lines with word 'biostats'
biostatsLines = fileContent[grepl("biostats", fileContent)]

print(biostatsLines)

########################## Question 6

matchedLines = fileContent[grepl("A computer once beat me at chess, but it was no match for me at kickboxing", fileContent)]

print(matchedLines)

# Closing connection
close(fileConn)


fileTemp <- file(paste0(strDir, strFiles[[1]]), "r")
temp <- readLines(fileTemp)
