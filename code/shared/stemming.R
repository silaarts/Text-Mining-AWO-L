# Title     : TODO
# Objective : TODO
# Created by: coenhacking
# Created on: 17/07/2020

# install packages
#install.packages("rjson")
#install.packages("purrr")
#install.packages("dict")

# load the package required to read JSON files.
library(rjson)
library(purrr)
library(collections)

get_stemming_list <- function (d = create_dictionary(), diminutives = TRUE, plurals = TRUE, misc = TRUE) {
    # create list
    stemming <- stem_list(diminutives, plurals, misc)

    # for each word in the lexicon
    for(w in d$keys()) {
        # define 'ws' to be the stemmed substitue of word 'w'
        ws <- w

        # for each defined stemming operation 'op'
        for(op in seq_along(stemming$as_list())) {
            pattern <- names(stemming$as_list())[op]
            replacement <- stemming$as_list()[op]

            # attemp substitution
            ws <- sub(pattern, replacement, ws)

            if (nchar(ws) > 1 && (ws != w || replacement == w) && d$has(ws))
                break; # if it exists, it can be combined

            # if no subsitute is found, reset 'ws'
            ws <- w
        }

        # if a substitute was found combine the sentiment occurances
        if (w != ws) {
            # print both word 'w' and it's substitue 'ws'
            #print(paste(w, '->', ws));

            # add the assiged sentiment of 'w' to substitue 'ws'
            d$set(ws, d$get(ws) + d$get(w))

            # remove 'w'
            d$pop(w)
        }
    }

    # return the resulting dictionary
    return(d)
}

get_stemming_mapping <- function (d = create_dictionary(), diminutives = TRUE, plurals = TRUE, misc = TRUE) {
    # create mapping dictionary
    dic <- dict()

    # create list
    stemming <- stem_list(diminutives, plurals, misc)

    # for each word in the lexicon
    for(w in d$keys()) {
        # define 'ws' to be the stemmed substitue of word 'w'
        ws <- w

        # for each defined stemming operation 'op'
        for(op in seq_along(stemming$as_list())) {
            pattern <- names(stemming$as_list())[op]
            replacement <- stemming$as_list()[op]

            # attemp substitution
            ws <- sub(pattern, replacement, ws)

            if (nchar(ws) > 1 && (ws != w || replacement == w) && d$has(ws))
                break; # if it exists, it can be combined

            # if no subsitute is found, reset 'ws'
            ws <- w
        }

        dic$set(w, ws)
    }

    # return the resulting dictionary
    return(dic)
}

# create the dictionary
create_dictionary <- function () {
    # create an empty lexicon
    d <- dict()

    # load every survey response
    files <- list.files(path="responses", pattern="*.json", full.names=TRUE, recursive=FALSE)

    # apply a transform to each filename x in the file list
    for(file in files) {
        # load the file into a variable result
        result <- fromJSON(file = file)

        # map every sentence in the answer to a labelled word
        for(sentence in names(result$answers)) {

            # write down the sentiment for this sentence
            sentiment <- result$answers[sentence]

            #remove the punctuation from the sentence
            sentence <- gsub('[[:punct:] ]+',' ', sentence)

            # make all words lower case
            sentence <- tolower(sentence)

            # convert the sentence into a set of words
            words <- strsplit(sentence, " ")

            # write the sentiment of this sentence as a vector (posistive, neutral, negative)
            if (sentiment == 1)
                sentimentVec <- c(1, 0, 0)
            else if (sentiment == 0)
                sentimentVec <- c(0, 1, 0)
            else
                sentimentVec <- c(0, 0, 1)

            # for each word count the number of times the words were used in the context of the sentiment that was given to this sentence
            mapping <- map(words[[1]], function(word) {
                d$set(word, d$get(word, c(0, 0, 0)) + sentimentVec)
            })
        }
    }

    return(d)
}

stem_list <- function (diminutives = TRUE, plurals = TRUE, verbs = TRUE, adjectives = TRUE) {
    # define a dictionary for storing stemming procedures (removal of prefixes and suffixes)
    stemming <- dict()

    # exceptions
    stemming$set('zege$', 'zege'); # zege
    stemming$set('uitje(s?)$', 'uitje'); # uitje(s)
    stemming$set('sprookje(s?)$', 'sprookje'); # uitje(s)
    stemming$set('jongetje(s?)$', 'jongetje'); # uitje(s)
    stemming$set('kleineren', 'kleineren'); # kleineren
    stemming$set('jongen(s?)', 'jongen'); # jongen
    stemming$set('natje', 'natje'); # natje

    # define plural suffixes
    if (plurals) {
        stemming$set('(ij)ven$', '\\1f') # olijven -> olijf
        stemming$set('zen$', 's') # reizen -> reis
        stemming$set('(ij)(.)en$', '\\1\\2') # konijnen -> konijn
        stemming$set('([qwrtypsdfghjklzxcvbnm])([aeiuo])(.)en$', '\\1\\2\\2\\3') # beren -> beer, lopen -> loop
        stemming$set('([qwrtypsdfghjklzxcvbnm])([aeiuo])(.){2}en$', '\\1\\2\\\3') # bedden -> bed, rennen -> ren
        stemming$set('a$', 'um') # musea -> museum
        stemming$set('i$', 'us') # alumni -> alumnus
        stemming$set('e[rln]en$', '') # eieren -> ei, wandelen -> wandel
    }

    # verbs
    if (verbs) {
        stemming$set('en$', 'e') # douchen -> douche
        stemming$set('te$', '') # hoogte -> hoog
        stemming$set('t$', '') # loopt -> loop
    }

    # adjectives
    if (adjectives) {
        # vieze -> vies
        # gekke -> gek
        stemming$set('([aeiuo])ne$', '') # schone -> schoon
        stemming$set('e$', '') # goede -> goed
        stemming$set('\'s$', '') # foto's -> foto
    }
    # NOTE: '-er' and '-ste' were left out intentionally as a greater degree is often used in different sentiment

    # diminutives
    if (diminutives) {
        stemming$set('tjes?$', '') # banaantje ->  banaan, banaantjes ->  banaan
        stemming$set('etjes?$', '') # dingetje ->  ding
        stemming$set('pjes?$', '') # boompje ->  boom
        stemming$set('kjes?$', 'g') # beloninkje ->  beloning
        stemming$set('jes?$', '') # knoopje -> knoop
    }

    return(stemming)
}

stem_word <- function (word, operations = stem_list(verbs = FALSE), dictionary = create_dictionary()) {
    word_substitute <- word

    # for each defined stemming operation 'op'
    for(op in seq_along(operations$as_list())) {
        pattern <- names(operations$as_list())[op]
        replacement <- operations$as_list()[op]

        # attemp substitution
        word_substitute <- sub(pattern, replacement, word_substitute)

        if (nchar(word_substitute) > 1 && (word_substitute != word || replacement == word) && dictionary$has(word_substitute))
            break; # if it exists, it can be combined

        # if no subsitute is found, reset 'word_substitute'
        word_substitute <- word
    }

    # return the result
    return(word_substitute)
}

# if run as a test
if (!interactive()) {
    print(get_stemming_list())
}

