pisa2012 <- read.csv("F:/Agnes/bigdata/scripts/pisa.reduced.csv")
summary(pisa2012)

#######################  bigvis ################################################
devtools::install_github("hadley/bigvis")
library(ggplot2)
library(bigvis)

default.theme <- theme_bw() + theme(text = element_text(face = "bold", size = 14))

math.score.condensed <- condense(bin(pisa2012$Math.Score))
autoplot.condensed(math.score.condensed) + default.theme +
  ggtitle("Distribution of math score, default binning")

autoplot.condensed(condense(bin(pisa2012$OutOfSchoolStudyTime, 10),
                            bin(pisa2012$Reading.Score, 30))) +
  scale_fill_gradient(low = "blue", high = "red") +
  default.theme + ggtitle("Out of school study time vs reading score")

## one with values, one with a descriptive statistics

math.and.reading.score.condensed <- condense(bin(pisa2012$Math.Score, 1),
                                             z = pisa2012$Reading.Score)
autoplot.condensed(math.and.reading.score.condensed) + default.theme +
  scale_color_gradient(low = "blue", high = "red") +
  ggtitle("Mathematics and reading scores")

math.and.reading.score.condensed <- condense(bin(pisa2012$Math.Score, 1),
                                             z = pisa2012$Reading.Score,
                                             summary = "sd")
autoplot.condensed(math.and.reading.score.condensed) + default.theme +
  scale_color_gradient(low = "blue", high = "red") +
  ggtitle("Mathematics and reading scores")


## animation for binwidth change

library(animation)
PlotABinwidth <- function(binwidth) {
  reading.score.condensed <- condense(bin(pisa2012$Reading.Score, binwidth))
  print(
    autoplot.condensed(reading.score.condensed) + default.theme +
      ggtitle(paste("Distribution of reading score with binning", binwidth)))
}

saveGIF({
  for (i in seq(0.1, 5, 0.1)) PlotABinwidth(i)
}, movie.name = "bigdata_pisa_animation.gif",
img.name = "test",
ani.width = 800,
ani.height = 600)

## smoothing
reading.score.condensed <- condense(bin(pisa2012$Reading.Score, 0.2))
autoplot(reading.score.condensed) + default.theme +
  ggtitle(paste("Distribution of reading score without smoothing"))

smoothingFunction <- function(type) {
  sm <- smooth(reading.score.condensed, 3.5, type = type)
  autoplot(sm) + default.theme +
    ggtitle(paste("Distribution of reading score with smooth type:\n", type))
}

smoothingFunction("mean")
smoothingFunction("regression")
smoothingFunction("robust_regression")

## best bandwidth choice
h <- best_h(reading.score.condensed, h_init = 0.5, tol = 1,
            control = list(trace = 3, REPORT = 1))
autoplot(smooth(reading.score.condensed, h))

