#Building the next word prediction

#Load the n-gram databases 
if(!is.loaded("unigram.RData")){
    load("unigram.RData")
}
if(!is.loaded("bigram.RData")){
    load("bigram.RData")
}
if(!is.loaded("trigram.RData")){
    load("trigram.RData")
}
if(!is.loaded("quadgram.RData")){
    load("quadgram.RData")
}


#Load the necessary libraries
totalPack <- installed.packages()
if(!("tm" %in% totalPack)){
    install.packages("tm")
}
if(!("DT" %in% totalPack)){
    install.packages("DT")
}
if(!("dplyr" %in% totalPack)){
    install.packages("dplyr")
}
if(!("stringi" %in% totalPack)){
    install.packages("stringi")
}
if(!("quanteda" %in% totalPack)){
    install.packages("quanteda")
}
if(!("data.table" %in% totalPack)){
    install.packages("data.table")
}
library(DT)
library(tm)
library(dplyr)
library(stringi)
library(quanteda)
library(data.table)

## Clean string function
cleanText <- function(inStr){
    inStr <- iconv(x = inStr, "latin1", "ASCII", sub = " ")
    inStr <- gsub("[^[:alpha:][:space:][:punct:]]", "", inStr);
    
    inStrCrps <- VCorpus(VectorSource(inStr))
    inStrCrps <- tm_map(inStrCrps, content_transformer(tolower))
    inStrCrps <- tm_map(inStrCrps, removePunctuation)
    inStrCrps <- tm_map(inStrCrps, removeNumbers)
    inStrCrps <- tm_map(inStrCrps, stripWhitespace)
    inStr <- as.character(inStrCrps[[1]])
    inStr <- gsub("(^[[:space:]]+|[[:space:]]+$)", "", inStr)
    
    
    if (nchar(inStr) > 0) {
        return(inStr); 
    } else {
        return("");
    }
}

## Word prediction function
wordPred <- function(inStr){
    splitStr <- unlist(strsplit(cleanText(inStr = inStr), split = " "))
    lengthStr <- length(splitStr)
    
    isQuadGram <- NULL
    isTriGram <- NULL
    isBiGram <- NULL
    
    if(lengthStr >= 3){
        strTemp <- paste(splitStr[(lengthStr-2):lengthStr], collapse = " ");
        isQuadGram <- quadgramDF[grep(paste("^", strTemp, sep = ""), quadgramDF$terms),]
        if(length(isQuadGram[,1]) >= 1){
            tempVar <- sum(isQuadGram$freq)
            isQuadGram$disc <- isQuadGram$freq *(0.5/tempVar) 
        }
        strTemp <- paste(splitStr[(lengthStr-1):lengthStr], collapse = " ");
        isTriGram <- trigramDF[grep(paste("^", strTemp, sep = ""), trigramDF$terms),]
        if(length(isTriGram[,1]) >= 1){
            tempVar <- sum(isTriGram$freq)
            isTriGram$disc <- isTriGram$freq *(0.3/tempVar) 
        }
        isBiGram <- bigramDF[grep(paste("^", splitStr[lengthStr], sep = ""), bigramDF$terms),]
        if(length(isBiGram[,1]) >= 1){
            tempVar <- sum(isBiGram$freq)
            isBiGram$disc <- isBiGram$freq *(0.2/tempVar) 
        }
    }
    if(lengthStr == 2){
        strTemp <- paste(splitStr[(lengthStr-1):lengthStr], collapse = " ");
        isTriGram <- trigramDF[grep(paste("^", strTemp, sep = ""), trigramDF$terms),]
        if(length(isTriGram[,1]) >= 1){
            tempVar <- sum(isTriGram$freq)
            isTriGram$disc <- isTriGram$freq *(0.3/tempVar) 
        }
        isBiGram <- bigramDF[grep(paste("^", splitStr[lengthStr], sep = ""), bigramDF$terms),]
        if(length(isBiGram[,1]) >= 1){
            tempVar <- sum(isBiGram$freq)
            isBiGram$disc <- isBiGram$freq *(0.2/tempVar) 
        }
    }
    if(lengthStr == 1){
        isBiGram <- bigramDF[grep(paste("^", splitStr[lengthStr], sep = ""), bigramDF$terms),]
        if(length(isBiGram[,1]) >= 1){
            tempVar <- sum(isBiGram$freq)
            isBiGram$disc <- isBiGram$freq *(0.2/tempVar) 
        }
    }

    NextPredDF <- rbind(isQuadGram, isTriGram, isBiGram)
    if(is.null(NextPredDF)){
        foundWord <- FALSE
        return(NULL)
    } else{
        NextPredDF$NextPrediction <- lapply(NextPredDF$terms, function(x) sub(".*\\b(\\w+)$", "\\1", x))
        NextPredDF$NextPrediction <- unlist(NextPredDF$NextPrediction)
        NextPredDF <- NextPredDF[, -c(1,2)]

        newWordDF <- NextPredDF%>%group_by(NextPrediction)%>%summarize(sum(disc))
        colnames(newWordDF) <- c("Next_Word", "Probability")
        newWordDF <- newWordDF[order(newWordDF$Probability, decreasing = TRUE),]
        tempVar <- data.frame(Next_Word = newWordDF$Next_Word[1:15],
                              Probability = format(round(newWordDF$Probability[1:15]*100,2), nsmall = 2))
        # return(as.String(tempVar$Next_Word[1]))
        return(tempVar)
    }


}
