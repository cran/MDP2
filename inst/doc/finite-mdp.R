## ----setup, include=FALSE---------------------------------------------------------------
library(knitr)
options(
  rmarkdown.html_vignette.check_title = FALSE,
  # formatR.arrow = TRUE,
  # scipen=999,
  # digits=5,
  width = 90
)
# thm <- knit_theme$get("edit-kwrite")   # whitengrey, bright, print, edit-flashdevelop, edit-kwrite
# knit_theme$set(thm)
knit_hooks$set(
  par = function(before, options, envir) {
    if (before && options$fig.show != "none") {
      par(
        mar = c(0, 0, 0, 0), # bottom, left, top, and right
        oma = c(0, 0, 0, 0)
      )
    }
  }
)
knitr::opts_chunk$set(
  # collapse = TRUE,
  comment = "#>",
  fig.align = "center",
  fig.width = 9,
  fig.height = 5,
  fig.show = "hold",
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

## ----par=TRUE---------------------------------------------------------------------------
prefix <- paste0(system.file("models", package = "MDP2"), "/machine1_")
mdp <- load_mdp(prefix)
get_info(mdp, with_list = F, df_level = "action", as_strings_actions = TRUE)

## ----par=TRUE---------------------------------------------------------------------------
plot(mdp, action_color = "label", radx = 0.06, mar_x = 0.065, mar_y = 0.055)

## ----solve3-----------------------------------------------------------------------------
scrapValues <- c(30, 10, 5, 0) # scrap values (the values of the 4 states at the last stage)
run_value_ite(mdp, "Net reward", term_values = scrapValues)

## ----par=TRUE---------------------------------------------------------------------------
pol <- get_policy(mdp)
tail(pol)
plot(mdp,
  actions_visible = "policy", state_label = "weight",
  radx = 0.06, mar_x = 0.065, mar_y = 0.055
)

## ----Set policy (machine rep),echo=TRUE,eval=TRUE---------------------------------------
policy <- data.frame(s_id = c(8, 11), a_idx = c(0, 0)) # set the policy for s_id 8 and 11 to mt
set_policy(mdp, policy)
get_policy(mdp)

## ----Calc reward (machine rep),echo=TRUE------------------------------------------------
run_calc_weights(mdp, "Net reward", term_values = scrapValues)
tail(get_policy(mdp))

