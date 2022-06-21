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
library(rjson, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)
library(collections, warn.conflicts = FALSE)
library(Ternary, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(gridExtra, warn.conflicts = FALSE)
library(stopwords)

# load stemming functions
source("../shared/stemming.R")

# load stemmed lexicon
d <- get_stemming_list()

# function that takes in:
#   dictionary 'd',
#   a word 'w'
#   sentiment s (1 psotive, 2 neutral or 3 negative)
#   the number of words in
# returns the probability that a word occurs in sentence, given the sentiment is equal to 's'
prob_sentiment <- function(d, w, s, ws) {

    # if the word doesn't exist in the document, ignore and return 1
    # this way it won't affect the algorithm
    if (!d$has(w))
      return(1)

    # bayes rule, the number of times a word has occured in a sentence with sentiment s, divided by total ammoutn of word occurances give a sentiment 's' + the size of the vocabulary
    top <- as.numeric(d$get(w)[s]) + 1.0
    bottom <- as.numeric(ws[s]) + as.numeric(d$size())

    return(top / bottom)
}

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

# init numbers
numResidents <- 0
numCaregivers <- 0
numFamily <- 0
numWords <- 0

# init the chart data points
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

    if (group == 'R') {
        numResidents <- numResidents + 1
    } else if (group == 'C') {
        numCaregivers <- numCaregivers + 1
    } else {
        numFamily <- numFamily + 1
    }

    # for each word, multiply the probability with the probability of 'w' exisitng in sentiment 's'
    for(sentence in transcript[[1]]) {

        #remove the punctuation from the sentence
        sentence <- gsub('[[:punct:] ]+',' ', sentence)

        # make all words lower case
        sentence <- tolower(sentence)

        # convert the sentence into a set of words
        words <- strsplit(sentence, " ")

        # add counted words
        numWords <- numWords + length(words)

        mapping <- map(words[[1]], function(word) {
            # get stem or simply the word if no stem exists
            word <- stemming_mapping$get(word, word);

            # reomve the name of a resident
            if (word == 'naambewoner')
                word <- '';

            if (d$has(word) && word != ' ' && nchar(word) > 2) {

                word_sentiment <- avg_sentiment(d$get(word, c(0,0,0)))

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

sortOccurances <- function (words, dic, num = 40, reverse = FALSE) {
    words <- unlist(words$as_list())

    # change visualisation for name of resident
    names(words) <- lapply(names(words), function (x) {
        if (x == 'naambewoner')
          x <- '[NAME OF RESIDENT]';

        return(x)
    })

    # order based on the # of occurances
    words <- words[order(words, decreasing = TRUE)]

    # calculate the sentiment
    sentiment <- lapply(names(words), function (x) {
        vec <- dic$get(x, c(0,0,0))
        total <- vec[1] + vec[2] + vec[3]
        avg <- (vec[1] - vec[3]) / total

        if (reverse)
          return(-avg)

        return(avg)
    })

    # remove overlapping values
    tmpWords <- list()
    tmpSentiment <- list()

    for (idx in seq_along(words)) {
        if (length(tmpWords) > num)
            break;

        add <- TRUE

        for (idx2 in seq_along(tmpWords)) {
            # add item
            if (abs(words[[idx]] - tmpWords[[idx2]]) < 10 &&
              abs(sentiment[[idx]] - tmpSentiment[[idx2]]) < 0.1) {
                add <- FALSE
                break;
            }
        }

        if (add) {
            tmpWords <- append(tmpWords, words[idx])
            names(tmpWords)[length(tmpWords)] <- names(words)[idx]
            tmpSentiment <- append(tmpSentiment, sentiment[idx])
        }
    }

    words <- tmpWords
    sentiment <- tmpSentiment

    # convert into data frame
    df <- data.frame(word = unlist(names(words)), occurances = unlist(words), sentiment = unlist(sentiment))

    # take the 'num' most occuring words
    df <- head(df, n = num)

    # return the list
    return(df)
}

residents <- sortOccurances(residents, d)
caregivers <- sortOccurances(caregivers, d)
family <- sortOccurances(family, d)

# make a plot of the most residents' words
p1 <- ggplot(residents, aes(sentiment, occurances, label = word)) +
    geom_text(size=5, family = "Times") +
    xlab("Sentiment") +
    ylab("Frequency") +
    xlim(-1.0, 1.0) +
    scale_y_log10() +
    labs(caption="a. Most commonly occurring words among residents (n = 40)") +
    theme_minimal(20) +
    theme(legend.position = "none",
        text=element_text(family="Times"),
        plot.caption = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.ticks.y = element_blank())

# make a plot of the most family members' words
p2 <- ggplot(family, aes(sentiment, occurances, label = word)) +
    geom_text(size=5, family = "Times") +
    xlab("Sentiment") +
    ylab("Frequency") +
    xlim(-1.0, 1.0) +
    scale_y_log10() +
    labs(caption="b. Most commonly occurring words among family members (n = 40)") +
    theme_minimal(20) +
    theme(legend.position = "none",
        text=element_text(family="Times"),
        plot.caption = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.ticks.y = element_blank())

# make a plot of the most care professionals' words
p3 <- ggplot(caregivers, aes(sentiment, occurances, label = word)) +
    geom_text(size=5, family = "Times") +
    xlab("Sentiment") +
    ylab("Frequency") +
    xlim(-1.0, 1.0) +
    scale_y_log10() +
    labs(caption="c. Most commonly occurring words among care professionals (n = 40)") +
    theme_minimal(20) +
    theme(legend.position = "none",
        text=element_text(family="Times"),
        plot.caption = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.ticks.y = element_blank())
