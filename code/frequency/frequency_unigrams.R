# Title     : TODO
# Objective : TODO
# Created by: coenhacking
# Created on: 14/09/2020
# Based on: https://web.stanford.edu/~jurafsky/slp3/4.pdf

# install packages
#install.packages("rjson")
#install.packages("purrr")
#install.packages("dict")
#install.packages(Ternary)

# load the package required to read JSON files.
library(rjson)
library(purrr)
library(collections)
library(Ternary)
library(ggplot2)
library(dplyr)
library(gridExtra, warn.conflicts = FALSE)
library(stopwords)

# load stemming functions
source("../shared/stemming.R", chdir = TRUE)

# load stemmed lexicon
d <- get_stemming_list()

# ----------------------
# filter stop words
# ----------------------

#remove the (n = 40) words that occur most often
for(stopword in stopwords("dutch")) {
  if (d$has(stopword))
    d$pop(stopword)
}

# ---------------------------------
# calculate transcript sentiment
# ---------------------------------

avg_sentiment <- function (sentiment_vec) {
    total <- sentiment_vec[1] + sentiment_vec[2] + sentiment_vec[3]
    return((sentiment_vec[1] - sentiment_vec[3]) / total)
}

groupName <- function (filename) {
    if (length(grep("[RB]-", filename)) > 0)
        return('R')
    if (length(grep("F-", filename, fixed = TRUE)) > 0)
        return('F')
    else # it's a care professional
        return('C')
}

# load the transcript of every connecting conversation from CSV
files <- list.files(path="../survey/data", pattern="*.csv", full.names=TRUE, recursive=FALSE)

# init the chart data points
totalS <- 0
totalW <- 0
total <- dict()
residents <- dict()
caregivers <- dict()
family <- dict()

# write down mapping
stemming_mapping <- get_stemming_mapping(d, TRUE, TRUE, TRUE)

# for each transcript file, write down the distribution of positive, neutral and negative sentiment
for(file in files) {

    # load the file into the variable 'transcript'
    transcript <- read.delim(file)

    # take the filename from the full file path
    group <- groupName(basename(file))

    # for each word, multiply the probability with the probability of 'w' exisitng in sentiment 's'
    for(sentence in transcript[[1]]) {

        #remove the punctuation from the sentence
        sentence <- gsub('[[:punct:] ]+',' ', sentence)

        # make all words lower case
        sentence <- tolower(sentence)

        # convert the sentence into a set of words
        words <- strsplit(sentence, " ")
        totalW <- totalW + length(words[[1]])
        totalS <- totalS + 1

        map(words[[1]], function(word) {
            # get stem or simply the word if no stem exists
            word <- stemming_mapping$get(word, word);

            if (d$has(word) && word != ' ' && nchar(word) > 2) {

                # add to total
                total$set(word, total$get(word, 0) + 1)

                if (group == 'R') {
                    residents$set(word, residents$get(word, 0) + 1)
                } else if (group == 'C') {
                    caregivers$set(word, caregivers$get(word, 0) + 1)
                } else {
                    family$set(word, family$get(word, 0) + 1)
                }
            }
        })
    }
}

sortOccurances <- function (words, num = 20, reverse = FALSE) {
    words <- unlist(words$as_list())

    # order based on the # of occurances
    words <- words[order(words, decreasing = TRUE)]

    # take the 'num' most occuring words
    words <- head(words, n = num)

    # convert into data frame
    df <- data.frame(word = unlist(names(words)), occurances = unlist(words))

    # return the list
    return(df)
}

total <- sortOccurances(total, num = 50)
residents <- sortOccurances(residents)
caregivers <- sortOccurances(caregivers)
family <- sortOccurances(family)

# totalW
# totalS

# make a plot of the most frequent words
ggplot(total, aes(x = reorder(word, occurances), y = occurances)) +
    geom_bar(stat = "identity", fill = '#aaaaaa') +
    xlab("") +
    ylab("") +
    theme_minimal(20) +
    theme(legend.position = "none",
        text=element_text(family="Times"),
        plot.title=element_text(size=12, hjust=0, face='bold'),
        plot.subtitle=element_text(size=12, hjust=0, face='italic'),
        plot.caption=element_text(size=8, hjust=1),
        axis.text.x = element_text(size=12),
        axis.title.x = element_text(size=12, face='bold'),
        axis.text.y = element_text(size=12),
        axis.ticks.y = element_blank()) +
    coord_flip()