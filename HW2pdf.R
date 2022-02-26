---
title: "HW2"
author: "Michelle Pan"
date: "2/23/2022"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
source("HW2v2.R")
```

Summarize Data

1. Hospitals may submit 1996 versions of HCRIS reports in 2010 because they may take longer to switch over and/or their fiscal year may end at a different time than the calendar year.
```{r Summary1}
answerForOne
```

2.
```{r Summary2}
answerForTwo
summarizeGraphForTwo
```

3. 
```{r Summary3}
summarizeGraphForThree
```

4. 
```{r Summary 4}
summarizeGraphForFour
```

5. 
```{r Summary 5}
summarizeGraphForFive
```

6. 
```{r Summary 6}
summarizeGraphForSix
```
Estimate ATEs
1.
```{r ATE1}
avgByPenalty
```
2.
```{r ATE2}
summary(byQuartile$quartile)
```
3.
```{r ATE3}
summary(nnInverseVariance)
```
4.
```{r ATE4}
summary(nnMahalanobis)
```
5.
```{r ATE5}
IPWresult
summary(Ipw)
```
6.
```{r ATE6}
regression
```

7. The results are very different. The nearest neighbor by inverse variance and nearest neighbor by Mahalanobis estimates are similar to each other, but the inverse propensity weight and regression results are very different from both the matching and each other.
8. No, because we only matched by bed size, but there are other variables that may affect the relationship between penalty and price.