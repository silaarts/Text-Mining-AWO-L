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
library(stopwords)

# ---------------------------------
# load sentiment from Python output
# ---------------------------------

# load the file into a variable result
results <- fromJSON(file = "fig_5.json")

# init the chart data points
chartPointsR <- list();
chartPointsF <- list();
chartPointsC <- list();

groupName <- function (filename) {
    if (length(grep("[RB]-", filename)) > 0)
        return('R')
    if (length(grep("F-", filename, fixed = TRUE)) > 0)
        return('F')
    else # it's a care professional
        return('C')
}

# for each transcript file, write down the distribution of positive, neutral and negative sentiment
for(result in results) {

  # file name
  filename <- result$filename

  # result vector
  sentiment <- c(result$neutral, result$negative, result$positive)

  # add this analysis the points for the chart
  if (groupName(filename) == 'R')
    chartPointsR[[filename]] <- sentiment
  else if (groupName(filename) == 'F')
    chartPointsF[[filename]] <- sentiment
  else
    chartPointsC[[filename]] <- sentiment
}

# ---------------------------------
# draw the chart
# ---------------------------------

# set font family
par(family = 'serif')

TernaryPlot(#atip='Neutral (%)', btip='Negative (%)', ctip='Positive (%)',
            alab="Neutral (%)", blab="Negative (%)", clab="Positive (%)",
            lab.cex=0.8, grid.minor.lines=0,
            axis.rotate = FALSE,
            axis.cex = 0.5,
            grid.lty='solid', col=rgb(0.9, 0.9, 0.9), grid.col='white',
            axis.col=rgb(0.6, 0.6, 0.6), ticks.col=rgb(0.6, 0.6, 0.6),
            padding=0.08)


# Colour the background:
FunctionToContour <- function (a, b, c) {
    return(rgb(a * 0 + 223,223, 223, 255, maxColorValue = 255))
}

# values <- TernaryPointValues(FunctionToContour, resolution = 20)
# ColourTernary(values)

# Add data points
AddToTernary(points, chartPointsR, pch=21, cex=0.75,
             col=rgb(255, 0, 0,255, maxColorValue=255),
             bg=vapply(lapply(names(chartPointsR), groupName),
                       function (x) {
                            if (x == 'R') {
                                return(rgb(255, 0, 0,255, maxColorValue=255));
                            } else if (x == 'F') {
                                return(rgb(0, 0, 255,255, maxColorValue=255));
                            } else {
                                return(rgb(0, 255, 0,255, maxColorValue=255));
                            }
                       },
                       character(1))
             )
AddToTernary(points, chartPointsF, pch=4, cex=0.75,
             col=vapply(lapply(names(chartPointsF), groupName),
                       function (x) {
                            if (x == 'R') {
                                return(rgb(255, 0, 0,255, maxColorValue=255));
                            } else if (x == 'F') {
                                return(rgb(0, 0, 255,255, maxColorValue=255));
                            } else {
                                return(rgb(0, 255, 0,255, maxColorValue=255));
                            }
                       },
                       character(1)),
             bg=vapply(lapply(names(chartPointsF), groupName),
                       function (x) {
                            if (x == 'R') {
                                return(rgb(255, 0, 0,255, maxColorValue=255));
                            } else if (x == 'F') {
                                return(rgb(0, 0, 255,255, maxColorValue=255));
                            } else {
                                return(rgb(0, 255, 0,255, maxColorValue=255));
                            }
                       },
                       character(1))
             )
AddToTernary(points, chartPointsC, pch=2, cex=0.75,
             col=vapply(lapply(names(chartPointsC), groupName),
                       function (x) {
                            if (x == 'R') {
                                return(rgb(255, 0, 0,255, maxColorValue=255));
                            } else if (x == 'F') {
                                return(rgb(0, 0, 255,255, maxColorValue=255));
                            } else {
                                return(rgb(0, 200, 0,255, maxColorValue=255));
                            }
                       },
                       character(1)),
             bg=vapply(lapply(names(chartPointsC), groupName),
                       function (x) {
                            if (x == 'R') {
                                return(rgb(255, 0, 0,255, maxColorValue=255));
                            } else if (x == 'F') {
                                return(rgb(0, 0, 255,255, maxColorValue=255));
                            } else {
                                return(rgb(0, 200, 0,255, maxColorValue=255));
                            }
                       },
                       character(1))
             )

legend('topright',
       legend=c('Resident', 'Family of Resident', 'Care Professional'),
       cex=0.8, bty='n', pch=c(21, 4, 2), pt.cex=1.0,
       col=c(rgb(255,   0,   0, 255, NULL, 255),
               rgb(0,   0,   255, 255, NULL, 255),
               rgb(0,     255, 0, 255, NULL, 255)),
       )