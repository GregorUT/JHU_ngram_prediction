
library(tm)

setwd("D:/Repository/NGram")
InputString <- readLines("Quiz3Strings.txt")

sw <- stopwords("en")
PrepInputString <- function (x)
{
        x <- tolower(x)
        broken <- strsplit(x, " ")
        broken <- gsub( "[^[:alnum:],]", "", broken )
        cleanString <- unlist(broken)[!(unlist(broken) %in% sw)]
        return(cleanString)

}

PrepedString <- PrepInputString(workingString)