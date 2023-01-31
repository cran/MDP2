## ----setup, include=FALSE---------------------------------------------------------------
library(knitr)
options(rmarkdown.html_vignette.check_title = FALSE,
        # formatR.arrow = TRUE, 
        # scipen=999, 
        # digits=5,
        width=90) 
#thm <- knit_theme$get("edit-kwrite")   # whitengrey, bright, print, edit-flashdevelop, edit-kwrite
#knit_theme$set(thm)
knit_hooks$set(
   par = function(before, options, envir) {
      if (before && options$fig.show != 'none')
         par(mar = c(0, 0, 0, 0), # bottom, left, top, and right
             oma = c(0, 0, 0, 0))}
)
knitr::opts_chunk$set(
   # collapse = TRUE,
   comment = "#>",
   fig.align = 'center',
   fig.width = 9,
   fig.height = 5,
   fig.show = 'hold',
   out.extra = 'style="max-width:100%;"',
   # tidy = TRUE,
   # prompt=T,
   # comment=NA,
   cache = F
   # background = "red"
)
library(magrittr)
library(dplyr)

## ---------------------------------------------------------------------------------------
library(MDP2)

## ---- par=TRUE--------------------------------------------------------------------------
prefix <- paste0(system.file("models", package = "MDP2"), "/machine1_")
mdp <- loadMDP(prefix)
getInfo(mdp, withList = F, dfLevel = "action", asStringsActions = TRUE)  

## ---- par=TRUE--------------------------------------------------------------------------
plot(mdp, hyperarcColor = "label", radx = 0.06, marX = 0.065, marY = 0.055)

## ----solve3-----------------------------------------------------------------------------
scrapValues <- c(30, 10, 5, 0)   # scrap values (the values of the 4 states at the last stage)
runValueIte(mdp, "Net reward", termValues = scrapValues)

## ---- par=TRUE--------------------------------------------------------------------------
pol <- getPolicy(mdp)
tail(pol)
plot(mdp, hyperarcShow = "policy", nodeLabel = "weight", 
     radx = 0.06, marX = 0.065, marY = 0.055)

## ----Set policy (machine rep),echo=TRUE,eval=TRUE---------------------------------------
policy<-data.frame(sId=c(8,11), aIdx=c(0,0)) # set the policy for sId 8 and 11 to mt
setPolicy(mdp, policy)
getPolicy(mdp)

## ----Calc reward (machine rep),echo=TRUE------------------------------------------------
runCalcWeights(mdp, "Net reward", termValues = scrapValues)
tail(getPolicy(mdp))    

