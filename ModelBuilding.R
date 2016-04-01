Remove_Stop <- TRUE
options("java.parameters" = "-Xmx4096m")
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

### Requirements
setwd("D:/Repository/NGram")
library(stringr)
library(reshape2)
library(slam)

if (Remove_Stop)
{
        load("tdmdata_NS.rda")        
}else
{
        load("tdmdata.rda")
}

#must have the library(slam) installed
#rowSums combines the 3 documents to a single document.  
#This will collapse the data.  Since the model will combine the 3 corpus documents saving on overall storage
#  and system requirements.
CntMtx.sng <- rowSums(as.matrix(tdm.single))
CntMtx.bi <- rowSums(as.matrix(tdm.bi))
CntMtx.tri <- rowSums(as.matrix(tdm.tri))
CntMtx.four <- rowSums(as.matrix(tdm.four))
CntMtx.five <- rowSums(as.matrix(tdm.five))

#Determines total number of n-gram tokens.
#Based on the total n-gram tokens it will determine the probablility of the given token.
#It will then order the n-gram tokens from most common to least frequent.
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

#Process all given tokens
gramsStats.single <- genstats(CntMtx.sng)
gramsStats.bi <- genstats(CntMtx.bi)
gramsStats.tri<- genstats(CntMtx.tri)
gramsStats.four <- genstats(CntMtx.four)
gramsStats.five <- genstats(CntMtx.five)


#Due to memory limitations - The model cannot include the full corpus.  
#The CoverageCompute function is used to gain an understanding of percentage of total corpus covered
#  by the percentage of total tokens included in the limiting set.
#The total tokens are ordered from frequent to infrequent.  
#This data will be used to determine cut points for the n-gram token commonality.

CoverageCompute<- function(x)
{
        TotalCount <- sum(x$count)
        TotalEntries <- nrow(x)
        coverageList <- NULL
        StepCount <- TotalEntries/20 #Change the demoninator to set the steps increases.  ie. 20 -> 100%/20 gives 5% steps.
        stepnum <- 1
        rIndex <- round(StepCount, digits = 0)
        while (rIndex <= TotalEntries)
        {
                coverage <- sum(x$count[1:rIndex])/TotalCount
                #Change from string to matrix to allow for graphs.
                newEntry <- c(stepnum * 5, coverage)
                coverageList <- rbind(coverageList,newEntry)
                stepnum <- stepnum +1
                rIndex <- rIndex + StepCount
                
        }
        rownames(coverageList) <- NULL
        colnames(coverageList) <- c("Frequent","Coverage")
        coverageList <- as.data.frame(coverageList)
}

Coverage.single <- CoverageCompute(gramsStats.single)
Coverage.bi <- CoverageCompute(gramsStats.bi)
Coverage.tri <- CoverageCompute(gramsStats.tri)
Coverage.four <- CoverageCompute(gramsStats.four)
Coverage.five <- CoverageCompute(gramsStats.five)

###Additional cutpoint analysis
#For Single Gram 95% coverage is between 15 and 20%
#span takes the percentage range and returns where the cutpoint would be in the given range.
FindSpan <- function(x, perc.low, perc.high)
{
        TotalCount <- nrow(x)
        RowLow <- TotalCount * (perc.low/100)
        RowHigh <- TotalCount * (perc.high/100)
        CountLow <- x[RowLow,1]
        CountHigh <- x[RowHigh,1]
        return <- c(CountLow,CountHigh)
}

Span.single <- FindSpan(gramsStats.single, 15,20)
Span.bi <- FindSpan(gramsStats.bi, 20,25)
Span.tri <- FindSpan(gramsStats.tri, 20,25)

### Build Model
## Cut counts.  Limits the data saved in the model to tokens captured in the corps at the cut count or above.
## Lower counts could be creative spelling etc.

# Initial Start of High Model 
cut.single <- 10
cut.bi <- 2
cut.tri <- 1
cut.four <- 1
cut.five <- 1

#Limited gramstats
lgramstats.single <- subset(gramsStats.single,count > cut.single)
lgramstats.bi <- subset(gramsStats.bi,count > cut.bi)
lgramstats.tri <- subset(gramsStats.tri,count > cut.tri)
lgramstats.four <- subset(gramsStats.four,count > cut.four)
lgramstats.five <- subset(gramsStats.five,count > cut.five)

#Convert Row Names to grams
rownames(lgramstats.single) <- NULL
rownames(lgramstats.bi) <- NULL
rownames(lgramstats.tri) <- NULL
rownames(lgramstats.four) <- NULL
rownames(lgramstats.five) <- NULL

#Drop unneeded columns
lgramstats.single <- lgramstats.single[,c(1,3)]
lgramstats.bi <- lgramstats.bi[,c(1,3)]
lgramstats.tri <- lgramstats.tri[,c(1,3)]
lgramstats.four <- lgramstats.four[,c(1,3)]
lgramstats.five <- lgramstats.five[,c(1,3)]


### Build Model based on cut levels
## The data for the model will need the gram.head - The gram head is the token with the final word removed.
## Given the gram.head.  There is a Final.Prob Probability that the gram.final will
##   be the correct prediction


BreakFinalProb <- function (x)
{
        #Reorder based on Alpha of Grams
        x$grams <- as.character(x$grams)
        x <- x[order(x$grams),]
        n<- str_count(x[1,2], pattern = " ")
        gram.head <- paste(word(x$grams,1,n),sep = " ")
        x<-cbind(x,gram.head)
        gram.final <- word(x$grams, -1)
        x<-cbind(x,gram.final)
        
        #Drop down to the bottom of the function to keep in line with the probability
        x$gram.final <- as.character(x$gram.final)
        x$gram.head <- as.character(x$gram.head)
        
        #Calculate probability of final based in given gram.head
        i <- 1
        newStats <- NULL
        nrow(x)
        while(i < nrow(x))
        {
                Head.Text <- x[i,"gram.head"]
                df.head <- subset(x,x$gram.head==Head.Text)
                Count.Total <- sum(df.head$count)
                Final.Prob <- df.head$count/Count.Total
                df.head <- cbind(df.head,Final.Prob)
                df.head <-df.head[order(df.head$count, decreasing = TRUE),]
                newStats <- rbind(newStats,df.head)
                
                i <- i + nrow(df.head)
                df.head<-NULL
        }
        
        newStats
}

#Build the n-gram Model Data
TimeStart <- Sys.time()
        FinalProb4 <- BreakFinalProb(lgramstats.five)
        FinalProb3 <- BreakFinalProb(lgramstats.four)
        FinalProb2 <- BreakFinalProb(lgramstats.tri)
        FinalProb1 <- BreakFinalProb(lgramstats.bi)
TimeStop <- Sys.time()
TimeProcessFinalProb.all <- TimeStop - TimeStart


### Adding model data for additonal models (skip models, w/o stopwords, etc.)

## Skip word model data construction

#To create a sample set.
#skipgram.raw <- lgramstats.four[sample(nrow(lgramstats.four),2000),]

#For each line edit tokens.  
#First word becomes Head
#Final word is finish.  
#The middle word(S) dropped or skiped.
SkipTheMiddle <- function(x)
{
        gram.head <-word(x$grams, 1)
        x<-cbind(x,gram.head)
        gram.final <- word(x$grams, -1)
        x<-cbind(x,gram.final)
        
}

SkipPrep.3 <- SkipTheMiddle(lgramstats.tri)
SkipPrep.4 <- SkipTheMiddle(lgramstats.four)

#Combine like gram.head  then loop through the prediction.  Combine counts of the prediction.
#Determine probability given head.

combine_count.gram <-function (x)
{
        x$gram.head <- as.character(x$gram.head)
        x$gram.final <- as.character(x$gram.final)
        x$grams <- as.character(x$grams)
        x <- x[order(x$gram.head),]
        i <- 1
        new.grams <- NULL
        while (i < nrow(x))
        {
                head <- x[i,3]
                #group by gram.head
                head.same <- subset(x, gram.head == head)
                #order by tail
                finish.order <-head.same[order(head.same$gram.final),]
                k <- 1
                head.same.tCount <- sum(head.same$count) #Total count of the gram.head occurance
                while (k <= nrow(head.same))
                {
                        tail <- head.same[k,4]
                        final.same <-subset(head.same, gram.final == tail)
                        count.new <- sum(final.same$count)
                        prob.new <- round(count.new/head.same.tCount, digits = 4)
                        
                        #increment over the same records.
                        
                        #new combinied record
                        newskip.gram <- c(count.new, paste(final.same[1,3], final.same[1,4], sep = " "),
                                          final.same[1,3],final.same[1,4],prob.new)
                        new.grams <- rbind(new.grams, newskip.gram)
                        k<- k + nrow(final.same)
                }
                i <- i + nrow(head.same)
                
        }
        colnames(new.grams)<-c("count","grams", "gram.head", "gram.final", "Final.Prob")
        rownames(new.grams)<- NULL
        
        
        return(new.grams)
}

# Creation of rows is forcing creation of matrix.  The matrix then requires all data to be of same type.
# This creates a matrix of factors.
# The RevertToClassType levels the factors to their original data.
RevertToClassType <- function (x)
{
        x$count <- as.numeric(levels(x$count))[x$count]
        x$grams <- as.character(x$grams)
        x$gram.head <- as.character(x$gram.head)
        x$gram.final <- as.character(x$gram.final)
        #x$Final.Prob <- as.numeric(x$Final.Prob)
        x$Final.Prob <- as.numeric(levels(x$Final.Prob))[x$Final.Prob]
        return(x)
}


TimeStart <- Sys.time()
        FinalProbSk1 <- as.data.frame(combine_count.gram(SkipPrep.3))
        FinalProbSk1 <- RevertToClassType(FinalProbSk1)
        FinalProbSk2 <- as.data.frame(combine_count.gram(SkipPrep.4))
        FinalProbSk2 <- RevertToClassType(FinalProbSk2)
TimeStop <- Sys.time()
Time.SkipBuild.Process <- TimeStop - TimeStart

##Remove common words from prediction model.
## Trim lgramstats.n by removing rows with common words as the predicted word
#TrimCommonPredictions <- function(x,cword) #x is the dataframe, cword is the number of commonwords to remove
#{
#        commonwords <- gramsStats.single[order(gramsStats.single$count, decreasing = TRUE),]
#        cutcount <- gramsStats.single[cword,1]
#        commonwords <- subset(commonwords, count >= cutcount)
#        cwords <- as.character(commonwords$grams)
#        word.predicted <- word(x$grams, -1)
        #remove rows where gram.final is in commonwords.
#        lFinal <- x[!(word.predicted %in% cwords),]
#        return (lFinal)
#}
#limited.five <- TrimCommonPredictions(gramsStats.five)
#limited.four <- TrimCommonPredictions(gramsStats.four)
if(Remove_Stop)
{
        save(list =c("FinalProb1","FinalProb2","FinalProb3","FinalProb4","FinalProbSk1","FinalProbSk2"), file = "ProbDF_NS.rda")        
}else
{
        save(list =c("FinalProb1","FinalProb2","FinalProb3","FinalProb4","FinalProbSk1","FinalProbSk2"), file = "ProbDF.rda")
}

