scoreOne = vector()
scoreTwo = vector()
scoreThree = vector()
scoreFour = vector()
scoreFive = vector()
scoreMax = vector()
scoreMean = vector()
scoreTail = vector()

twoTests = function (graphType) {
  for (i in 1:500) {
    scoreOne[i] = round(rnorm(1, mean = 1200, sd = 30), digits = -1)
    scoreTwo[i] = round(rnorm(1, mean = 1200, sd = 30), digits = -1)
    scoreMax[i] = max(scoreOne[i], scoreTwo[i])
    scoreMean[i] = mean(scoreOne[i], scoreTwo[i])
    scoreTail[i] = scoreTwo[i]
  }
  if (graphType == "max") {
    hist(scoreMax, main = "Superscore for 2 Testings")
    summary(scoreMax)
  } else if (graphType == "mean") {
    hist(scoreMean, main = "Average of 2 Testings")
    summary(scoreMean)
  } else if (graphType == "tail") {
    hist(scoreTail, main = "Most Recent of 2 Testings")
    summary(scoreTail)
  } else {
    return(FALSE)
  }
}

fiveTests = function(graphType) {
  for (i in 1:500){
    scoreOne[i] = round(rnorm(1, mean = 1200, sd = 30), digits = -1)
    scoreTwo[i] = round(rnorm(1, mean = 1200, sd = 30), digits = -1)
    scoreThree[i] = round(rnorm(1, mean = 1200, sd = 30), digits = -1)
    scoreFour[i] = round(rnorm(1, mean = 1200, sd = 30), digits = -1)
    scoreFive[i] = round(rnorm(1, mean = 1200, sd = 30), digits = -1)
    scoreMax[i] = max(scoreOne[i], scoreTwo[i], scoreThree[i], scoreFour[i], scoreFive[i])
    scoreMean[i] = mean(scoreOne[i], scoreTwo[i], scoreThree[i], scoreFour[i], scoreFive[i])
    scoreTail[i] = scoreFive[i]
  }
  if (graphType == "max") {
    hist(scoreMax, main = "Superscore for 5 Testings")
    summary(scoreMax)
  } else if (graphType == "mean") {
    hist(scoreMean, main = "Average of 5 Testings")
    summary(scoreMean)
  } else if (graphType == "tail") {
    hist(scoreTail, main = "Most Recent of 5 Testings")
    summary(scoreTail)
  } else {
    return(FALSE)
  }
  
}

