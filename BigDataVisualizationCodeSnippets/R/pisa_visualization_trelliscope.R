pisa2012 <- read.csv("F:/Agnes/bigdata/scripts/pisa.reduced.csv")
summary(pisa2012)

#######################  Trelliscope  ##########################################
install.packages("devtools") # if not already installed
##remove.packages("Rcpp")
devtools::install_github("tesseradata/datadr")
devtools::install_github("tesseradata/trelliscope")
library(datadr)
library(trelliscope)
str(pisa2012)

## visual display initialization
conn <- vdbConn("vdb", name = "tesseraTutorial")

byCountry <- divide(pisa2012, by = c("CNT"))
getKeys(byCountry)
byCountry[[1]]

## general visualization function for math and reading score
read.and.math.Panel <- function(x){
  ggplot(x) + geom_point(aes(x = Math.Score, y = Reading.Score,
                             col = Gender), size = 4) +
    scale_color_brewer(palette = "Set1") +
    theme_bw() +
    theme(text = element_text(size = 14, face = "bold")) +
    xlim(c(0, 1000)) + ylim(c(0, 1000))
}

## testing the function on one concrete df
read.and.math.Panel(byCountry[[39]]$value)  ## Macao-China

## creating display
makeDisplay(byCountry,
            name = "Reading and math scores of children by gender",
            panelFn = read.and.math.Panel,
            width = 400, height = 400,
            lims = list(x = "same"))
view()

## cognostics
differenceBetweenGirlsAndBoys <- function(x){
  list(medianDiff =
         median(x[x$Gender == "Female", "Math.Score"]) -
         median(x[x$Gender == "Male", "Math.Score"]),
       desc = "Performance difference between girls and boys")

}
## testing the function
differenceBetweenGirlsAndBoys(byCountry[[1]]$value) ## Albania
differenceBetweenGirlsAndBoys(byCountry[[59]]$value) ## Sweden

## creating display with cognostics
makeDisplay(byCountry,
            name = "Reading and math scores of children by gender",
            panelFn = read.and.math.Panel,
            cogFn = differenceBetweenGirlsAndBoys,
            width = 400, height = 400,
            lims = list(x = "same"))
view()

################## second visualization
read.and.outofschool.Panel <- function(x) {
  ggplot(x) + geom_point(aes(x = OutOfSchoolStudyTime,
                             y = Reading.Score,
                             col = Gender),
                         size = 4) +
    scale_color_brewer(palette = "Set1") +
    theme_bw() +
    theme(text = element_text(size = 14, face = "bold"))
}

read.and.outofschool.Panel(byCountry[[65]]$value)

## scagnostics mesures
library(scagnostics)
scagMat <- function(x){
  scagnostics(na.omit(x[, c("OutOfSchoolStudyTime", "Reading.Score")]))
}

scagMat(byCountry[[39]]$value)

makeDisplay(byCountry,
            name = "Reading scores and out of school study time",
            cogFn = scagMat,
            panelFn = read.and.outofschool.Panel,
            lims = list(x = "same"))
view()