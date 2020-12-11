# Create unigram tokens
unigram_tokenizer <- function(document) unlist(lapply(ngrams(words(document), n = 1), paste, collapse = " "), use.names = F)

# Create bigram tokens
bigram_tokenizer <- function(document) unlist(lapply(ngrams(words(document), n = 2), paste, collapse = " "), use.names = F)

# Create trigram tokens
trigram_tokenizer <- function(document) unlist(lapply(ngrams(words(document), n = 3), paste, collapse = " "), use.names = F)
