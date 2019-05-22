#Set the working directory
setwd("e:/Luciano/R_WorkingDir/CapstoneProject/")


## Step 0: Load all needed libaries
#load the libraries
totalPack <- installed.packages()
if(!("ggplot2" %in% totalPack)){
    install.packages("ggplot2")
}
if(!("NLP" %in% totalPack)){
    install.packages("NLP")
}
if(!("tm" %in% totalPack)){
    install.packages("tm")
}
if(!("RWeka" %in% totalPack)){
    install.packages("RWeka")
}
if(!("data.table" %in% totalPack)){
    install.packages("data.table")
}
if(!("dplyr" %in% totalPack)){
    install.packages("dplyr")
}
if(!("stringi" %in% totalPack)){
    install.packages("stringi")
}
if(!("textcat" %in% totalPack)){
    install.packages("textcat")
}
if(!("quanteda" %in% totalPack)){
    install.packages("quanteda")
}
library(ggplot2)
library(NLP)
library(tm)
library(RWeka)
library(data.table)
library(dplyr)
library(stringi)
library(textcat)
library(quanteda)

# Step 1: Download the dataset and unzip 
if(!dir.exists("final/")){
    dir.create("./final/", showWarnings = TRUE)
}
urlPath <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"

## Check if file already downloaded
if(!file.exists("Coursera_Siftkey.zip")){
    download.file(url = urlPath, destfile = "Coursera_Siftkey.zip", mode = "wb")
}
rm(urlPath)

## Check if already unzipped
if(!file.exists("./final/en_US/en_US.twitter.txt") | !file.exists("./final/en_US/en_US.news.txt") | !file.exists("./final/en_US/en_US.blogs.txt")){
    unzip(zipfile = "Coursera_Siftkey.zip")
}

# Step 2: Read in the appropriate data
## Read line by line and close connection after finish
connection <- file("final/en_US/en_US.news.txt", open = "r")
newLines <- readLines(con = connection, n = -1L, encoding="UTF-8", skipNul = TRUE, warn = FALSE, ok = TRUE)
close(connection)
connection <- file("final/en_US/en_US.twitter.txt", open = "r")
twitterLines <- readLines(con = connection, n = -1L, encoding="UTF-8", skipNul = TRUE, warn = FALSE, ok = TRUE)
close(connection)
connection <- file("final/en_US/en_US.blogs.txt", open = "r")
blogsLines <- readLines(con = connection, n = -1L, encoding="UTF-8", skipNul = TRUE, warn = FALSE, ok = TRUE)
close(connection)

# Step 3: Summarize the data
## Get files sizes
newLines.size <- file.size("final/en_US/en_US.news.txt")
twitterLines.size <- file.size("final/en_US/en_US.twitter.txt")
blogsLines.size <- file.size("final/en_US/en_US.blogs.txt")

## Get words in flies
newLines.words <- stri_count_words(str = newLines, locale = "")
twitterLines.words <- stri_count_words(str = twitterLines, locale = "")
blogsLines.words <- stri_count_words(str = blogsLines, locale = "")


set.seed(5000)
sampleNum <- 1000
sampleNewSel <- sample(1:length(newLines), size = sampleNum, replace = F)
sampleTwitterSel <- sample(1:length(twitterLines), size = sampleNum, replace = F)
sampleBlogSel <- sample(1:length(blogsLines), size = sampleNum, replace = F)

sampleNew <- newLines[sampleNewSel]
sampleTwitter <- twitterLines[sampleTwitterSel]
sampleBlogs <- blogsLines[sampleBlogSel]

rm(sampleNewSel, newLines, sampleTwitterSel, twitterLines, sampleBlogSel, blogsLines)

# Funtcion to automatize n-gram generation
gen_ngram <- function(iDat, i, j=4){
    iDatTemp <- removePunctuation(iDat)
    iDatTemp <- stripWhitespace(iDatTemp)
    iDatTemp <- stri_trans_tolower(iDatTemp)
    iDatTemp1 <- gsub("ð|â|???|T|o|'|³|¾|ñ|f|.|º|°|»|²|¼|>|<|¹|·|¸|¦|~|ðÿ", " ", iDatTemp)
    iDatTemp1 <- removeWords(iDatTemp1, "\\b\\W{1}\\b")
    
    tokenDat <- tokens(iDatTemp1, what = "word", remove_numbers = TRUE,
                       remove_punct = TRUE, remove_separators = TRUE, remove_symbols = TRUE)
    
    gramDat <- tokens_ngrams(tokenDat, n = i, concatenator = " ")
    gramDatDF <- dfm(gramDat, tolower = TRUE, remove_punct = TRUE)
    # gramDatDF <- dfm_trim(gramDatDF, min_termfreq = j, max_docfreq = 0.4)
    
    gramDatMat <- sort(colSums(as.matrix(gramDatDF)), decreasing = TRUE)
    gramDF <- data.table(terms = names(gramDatMat), freq = gramDatMat)
    return(gramDF)
}

unigramNews <- gen_ngram(sampleNew, 1, 100)
bigramNews <- gen_ngram(sampleNew, 2, 4)
trigramNews <- gen_ngram(sampleNew, 3,4)
quadgramNews <- gen_ngram(sampleNew, 4,4)
gc()

unigramTwitt <- gen_ngram(sampleTwitter, 1, 100)
bigramTwitt <- gen_ngram(sampleTwitter, 2, 4)
trigramTwitt <- gen_ngram(sampleTwitter, 3,4)
quadgramTwitt <- gen_ngram(sampleTwitter, 4,4)
gc()

unigramBlogs <- gen_ngram(sampleBlogs, 1, 100)
bigramBlogs <- gen_ngram(sampleBlogs, 2, 4)
trigramBlogs <- gen_ngram(sampleBlogs, 3,4)
quadgramBlogs <- gen_ngram(sampleBlogs, 4,4)
gc()

unigramDF <- rbind(unigramNews, unigramTwitt, unigramBlogs)
bigramDF <- rbind(bigramNews, bigramTwitt, bigramBlogs)
trigramDF <- rbind(trigramNews, trigramTwitt, trigramBlogs)
quadgramDF <- rbind(quadgramNews, quadgramTwitt, quadgramBlogs)


unigramDF <- unigramDF[order(freq),]
bigramDF <- bigramDF[order(freq),]
trigramDF <- trigramDF[order(freq),]
quadgramDF <- quadgramDF[order(freq),]

rm(unigramNews, bigramNews, trigramNews, quadgramNews)
rm(unigramTwitt, bigramTwitt, trigramTwitt, quadgramTwitt)
rm(unigramBlogs, bigramBlogs, trigramBlogs, quadgramBlogs)

save(unigramDF, file = "unigram.RData")
save(bigramDF, file = "bigram.RData")
save(trigramDF, file = "trigram.RData")
save(quadgramDF, file = "quadgram.RData")

load("unigram.RData")
load("bigram.RData")
load("trigram.RData")
load("quadgram.RData")

