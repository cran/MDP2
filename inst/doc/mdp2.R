## ----setup, include=FALSE---------------------------------------------------------------
library(knitr)
options(rmarkdown.html_vignette.check_title = FALSE,
        # formatR.arrow = TRUE, 
        # scipen=999, 
        # digits=5,
        width=90) 
#thm <- knit_theme$get("edit-kwrite")   # whitengrey, bright, print, edit-flashdevelop, edit-kwrite
#knit_theme$set(thm)
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

