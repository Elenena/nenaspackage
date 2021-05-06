#' Splits a string in a vector of words
#'
#' Words can be separated by a space, an apostrophe or by one of the following characters: \code{, . ; : ? !}
#' @param x A string
#' @return A character vector
#' @export



word_split<- function(x) {
    a<-unlist(strsplit(x, " "))
    b<-unlist(strsplit(a, ","))
    c<-unlist(strsplit(b, "\\."))
    d<-unlist(strsplit(c, ";"))
    e<-unlist(strsplit(d, ":"))
    f<-unlist(strsplit(e, "\\?"))
    g<-unlist(strsplit(f, "!"))
    h<-unlist(strsplit(g, "'"))
    h
}

#' Counts the number of words in a text
#'
#' Words can be separated by a space, an apostrophe or by one of the following characters: \code{, . ; : ? !}
#' @param x A string
#' @return A numeric vector with the number of words
#' @export
word_count<- function(x) {
     w<-word_split(x)
     length(w)
}


ordering<- function(x) {
    w<-word_split(x)
    c<-sapply(w, function(x) strsplit(x,""))
    l<-sapply(c, length)
    df<-data.frame(word=w, length=l)
    df[order(-df$length, df$word),]
}

#' Finds the longest words in a text
#'
#' Words can be separated by a space, an apostrophe or by one of the following characters: \code{, . ; : ? !}
#' @param x A string
#' @return A character vector with the longest word and its length.
#' @details If there is more than one word with the max length, the first
#' in alphabetical order is returned
#' @export

longest_word<- function(x) {
    df<-ordering(x)
    as.character(df[1,])
}
