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

## ----parameters,  include=FALSE---------------------------------------------------------
N<-5; Cf<- -10; Cp<-c(0,-7,-7,-5) # use negative numbers since the MDP optimize based on rewards
Q <- matrix(c(
   0.90, 0.10, 0, 0, 0,
   0, 0.80, 0.10, 0.05, 0.05,
   0, 0, 0.70, 0.10, 0.20,
   0, 0, 0, 0.50, 0.50), nrow=4, byrow=T) 

## ----Qtable, results='asis', echo=FALSE-------------------------------------------------
rownames(Q)<-1:4
colnames(Q)<-1:5
knitr::kable(Q, row.names = T)

## ---------------------------------------------------------------------------------------
prefix <- paste0(system.file("models", package = "MDP2"), "/hct611-1_")
mdp <- loadMDP(prefix)

## ---------------------------------------------------------------------------------------
mdp

## ---------------------------------------------------------------------------------------
getInfo(mdp, withList = F, dfLevel = "action", asStringsActions = TRUE)  

## ----plotHgf, par=TRUE------------------------------------------------------------------
plot(mdp, hyperarcColor = "label", nodeLabel = "sId:label")

## ----solve1_ave, par=TRUE---------------------------------------------------------------
runPolicyIteAve(mdp,"Net reward","Duration")
getPolicy(mdp)
plot(mdp, hyperarcShow = "policy")

## ---- par=TRUE--------------------------------------------------------------------------
runPolicyIteDiscount(mdp,"Net reward","Duration", discountFactor = 0.5)
getPolicy(mdp)
plot(mdp, hyperarcShow = "policy")

## ---------------------------------------------------------------------------------------
runValueIte(mdp,"Net reward","Duration", discountFactor = 0.5, eps = 1e-10, maxIte = 1000)
getPolicy(mdp)

