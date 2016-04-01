library(plyr)
library(tm)
library(slam)
library(RWeka)
library(reshape2)
library(stringr)
library(klaR)
library(caret)
library(ggplot2)

Remove_Stop <-TRUE  #Remove Stopwords
Custom_Stop <-TRUE   #Use Custom Stopwords
Custom_Stop_Limit <- 75

if(Remove_Stop)
{
        if(Custom_Stop)
        {
                load("MyCommon.rda")
                CommonWords <- commonwords
        }else
        {
                CommonWords <- stopwords("en")
        }
}

options("java.parameters" = "-Xmx4096m")

WorkingDir <- ("D:/Repository/Capstone/")
DataDir <- ("D:/Repository/Capstone/Dataset/")
# Data has been downloaded and extracted to the Datasets folder
DataLang <- "en_US."
FileNews <- paste(c(DataDir, DataLang, "news.txt"), collapse = "")
FileBlog <- paste(c(DataDir, DataLang, "blogs.txt"), collapse = "")
FileTwitter <- paste(c(DataDir, DataLang, "twitter.txt"), collapse = "")
ProfanityList <- read.csv("D:/Repository/Capstone/Dataset/Profanity/Swearwords.csv")

#*****************************************************************************
#***   This only needs to be run on time as it will clean the twitter file ***
#*****************************************************************************
#TwitterText <- readLines(FileTwitter, encoding="UTF-8")
#    Here are a list of the characters
#TwitterText <- gsub("[\u201E\u201F\u0097\u0083\u0082\u0080\u0081\u0090\u0095\u009f\u0098\u008d\u008b\u0089\u0087\u008a¦??.¤º-»«Ã¢â¬Å¥¡Â¿°£·©Ë¦¼¹¸±???ð]+", " ", TwitterText)
#write.table(TwitterText,file=FileTwitter,row.names=FALSE,quote=F)

#********************************************************************************
#***    Remove apostrophes from the 3 Text files prior to reading in to corpus **
#** tm seems to have issues with contractions.  Bypassing as a work around.    **
#************************************************************************** *****
#TwitterText <- readLines(FileTwitter, encoding = "UTF-8")
#BlogText <- readLines(FileBlog, encoding = "UTF-8", warn = FALSE)
#NewsText <- readLines(FileNews, encoding = "UTF-8", warn = FALSE)
#removeApostrophe <- function(x)
#{
#        x<-gsub("'", "", x)
#        x<-gsub("`", "", x)
#        return (x)
#}
#TwitterText <- removeApostrophe(TwitterText)
#BlogText <- removeApostrophe(BlogText)
#NewsText <- removeApostrophe(NewsText)

#write(TwitterText,file=FileTwitter)
#write(BlogText,file=FileBlog)
#write(NewsText,file=FileNews)
#********************************************************************************


# Corpus Creation and Cleaning
# encoding = latin1 corrects for probles of international characters that showed up in the toLower() transformation on the corpus
TimeStart <- Sys.time()
RawCorpus <- VCorpus(DirSource(directory=DataDir, encoding = "latin1")
                     , readerControl = list(language = "en"))
TimeStop <- Sys.time()
TimeLoadFiles <- TimeStop - TimeStart

#make a copy of the original corpus prior to making any changes to prevent having to re-read the data.

pCorpus <- RawCorpus

##Adjusting the sample rates for the corpus
#To balance the smaller news corpus, the news sample will be weighted with a multiple of 4
#To counter the "Twitter Bias", the twitter sample will be weighted with a multiple of .5
#The Blog sample weight will remain the same.

set.seed (132016) 
SamplePercentage <- 0.20 #percentage to sample as a decimal
blogWeight <- 1
newsWeight <- 4
TwitterWeight <- .5
pCorpus[[1]]$content<-sample(pCorpus[[1]]$content, length(pCorpus[[1]]$content)*SamplePercentage*blogWeight)
pCorpus[[2]]$content<-sample(pCorpus[[2]]$content, length(pCorpus[[2]]$content)*SamplePercentage*newsWeight)
pCorpus[[3]]$content<-sample(pCorpus[[3]]$content, length(pCorpus[[3]]$content)*SamplePercentage*TwitterWeight)

### Clean the corpus

# Remove any numbers
pCorpus <- tm_map(pCorpus, removeNumbers)   
#to Lower requires a content_transformer in order to not disrupt the corpus.
#Apostrophies missing are readable.  But we are giving up a word on the token that could be useful.
#replacing Apostrophe with "" first then remove the rest of punctuation.
remApost <- function(x) gsub(pattern =  "'",replacement = "",x)
pCorpus <- tm_map(pCorpus, content_transformer(remApost))
remApost1 <- function(x) gsub(pattern = "`",replacement = "",x )
pCorpus <- tm_map(pCorpus, content_transformer(remApost))

pCorpus <- tm_map(pCorpus, content_transformer(tolower))

# The corpus appears to include two types of apostophes.
# Remove punctuation - this will remove apostrophes as well but the contraction would still be readable
pCorpus<- tm_map(pCorpus, removePunctuation)
# remove profanity
pCorpus<-tm_map(pCorpus,removeWords, ProfanityList)

# remove stopwords 
if(Remove_Stop)
{
        pCorpus <- tm_map(pCorpus, removeWords, CommonWords)
}

#remove all non-alpha
remNonAlph <- function(x) gsub("[^a-z0-9]"," ",x)
pCorpus <- tm_map(pCorpus, content_transformer(remNonAlph))

# Remove Whitespace
pCorpus<- tm_map(pCorpus, stripWhitespace)

### Read vector(s) back into the corpus.  Some of the transforms break the corpus format.
pCorpus <- Corpus(VectorSource(pCorpus))

###
#TimeStart <- Sys.time()
#tdm <- TermDocumentMatrix(pCorpus)
#TimeStop <- Sys.time()

#TimeCreatetdm <- TimeStop - TimeStart

#put in a standard matrix
#tdm.single <- as.matrix(tdm)

#Word count by document
#wordcount.doc <- as.data.frame(colSums(tdm.single))
#rownames(wordcount.doc)<- c("Blog","News","Twitter")

#Tokenize the documents and configure the results into a Term-Document Matrix
sgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=1,max=1))
bgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2,max=2))
tgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=3,max=3))
frgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=4,max=4))
fvgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=5, max=5))

tdm.single <- TermDocumentMatrix(pCorpus, control = list(tokenize = sgramTokenizer, wordLengths=c(1,Inf)))
tdm.bi <- TermDocumentMatrix(pCorpus, control = list(tokenize = bgramTokenizer, wordLengths=c(1,Inf)))
tdm.tri <- TermDocumentMatrix(pCorpus, control = list(tokenize = tgramTokenizer,wordLengths=c(1,Inf)))
tdm.four <-TermDocumentMatrix(pCorpus, control = list(tokenize = frgramTokenizer, wordLengths=c(1,Inf)))
tdm.five <-TermDocumentMatrix(pCorpus, control = list(tokenize = fvgramTokenizer, wordLengths=c(1,Inf)))

if(Remove_Stop)
{
        save(list=c("tdm.single","tdm.bi", "tdm.bi", "tdm.tri","tdm.four", "tdm.five"), file="TDMData_NS.rda")
}else
{
        save(list=c("tdm.single","tdm.bi", "tdm.bi", "tdm.tri","tdm.four", "tdm.five"), file="TDMData.rda")
}

if(Remove_Stop)
{
        load(file = "TDMData_NS.rda")
}else
{
        load(file = "TDMData.rda")
}
#Data had to be saved in a different format
#returning data to original state.

CntMtx.sng <- rowSums(as.matrix(tdm.single))
CntMtx.bi <- rowSums(as.matrix(tdm.bi))
CntMtx.tri <- rowSums(as.matrix(tdm.tri))
CntMtx.four <- rowSums(as.matrix(tdm.four))
CntMtx.five <- rowSums(as.matrix(tdm.five))

#Generate Usage Dataframe

genstats <- function(count)
{
        TotalCount <- sum(count)
        Prob <- count/TotalCount
        df.stats <- as.data.frame(cbind(count,Prob))
        grams <- rownames(df.stats)
        df.stats <- cbind(df.stats,grams)
        rownames(df.stats) <- NULL
        df.ordered.stats <- df.stats[order(-Prob),]
        df.ordered.stats
}

gramsStats.single <- genstats(CntMtx.sng)
gramsStats.bi <- genstats(CntMtx.bi)
gramsStats.tri<- genstats(CntMtx.tri)
gramsStats.four <- genstats(CntMtx.four)
gramsStats.five <- genstats(CntMtx.five)

# Save full Data With Stats
if(Remove_Stop)
{
        save(list=c("gramsStats.single", "gramsStats.bi", "gramsStats.tri","gramsStats.four", "gramsStats.five"), file="FullData_NS.rda") 
}else
{
        save(list=c("gramsStats.single", "gramsStats.bi", "gramsStats.tri","gramsStats.four", "gramsStats.five"), file="FullData.rda")
}
