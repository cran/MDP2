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

## ----echo=FALSE, fig.cap="The state-expanded hypergraph of the first stage of a hierarchical MDP. Level 0 indicate the founder level, and the nodes indicates states at the different levels. A child process (oval box) is represented using its state-expanded hypergraph (hyperarcs not shown) and is uniquely defined by a given state and action of its parent process."----
knitr::include_graphics("vignette_files/hmdp_index.png")

## ----par=TRUE---------------------------------------------------------------------------
prefix <- paste0(system.file("models", package = "MDP2"), "/cow_")
mdp <- load_mdp(prefix)
mdp

## ----plotHMDP, message=FALSE, par=TRUE--------------------------------------------------
hgf <- get_hypergraph(mdp)
## Rename labels
dat <- hgf$nodes %>%
  dplyr::mutate(label = dplyr::case_when(
    label == "Low yield" ~ "L",
    label == "Avg yield" ~ "A",
    label == "High yield" ~ "H",
    label == "Dummy" ~ "D",
    label == "Bad genetic level" ~ "Bad",
    label == "Avg genetic level" ~ "Avg",
    label == "Good genetic level" ~ "Good",
    TRUE ~ "Error"
  ))
## Set grid id
dat$g_id[1:3] <- 85:87
dat$g_id[43:45] <- 1:3
get_g_id <- function(process, stage, state) {
  if (process == 0) start <- 18
  if (process == 1) start <- 22
  if (process == 2) start <- 26
  return(start + 14 * stage + state)
}
idx <- 43
for (process in 0:2) {
  for (stage in 0:4) {
    for (state in 0:2) {
      if (stage == 0 & state > 0) break
      idx <- idx - 1
      # cat(idx,process,stage,state,get_g_id(process,stage,state),"\n")
      dat$g_id[idx] <- get_g_id(process, stage, state)
    }
  }
}
hgf$nodes <- dat
## Rename labels
dat <- hgf$hyperarcs %>%
  dplyr::mutate(
    label = dplyr::case_when(
      label == "Replace" ~ "R",
      label == "Keep" ~ "K",
      label == "Dummy" ~ "D",
      TRUE ~ "Error"
    ),
    col = dplyr::case_when(
      label == "R" ~ "deepskyblue3",
      label == "K" ~ "darkorange1",
      label == "D" ~ "black",
      TRUE ~ "Error"
    ),
    lwd = 0.5,
    label = ""
  )
hgf$hyperarcs <- dat
## Make the plot
plot_hypergraph(hgf, grid_dim = c(14, 7), cex = 0.8, radx = 0.02, rady = 0.03)

## ----Optimize (cow)---------------------------------------------------------------------
w_lbl <- "Net reward" # the weight we want to optimize (net reward)
dur_lbl <- "Duration" # the duration/time label
run_policy_ite_discount(mdp, w_lbl, dur_lbl, rate = 0.1)

## ----plotPolicy, results='hide', message=FALSE, par=TRUE--------------------------------
hgf$hyperarcs <- right_join(hgf$hyperarcs, get_policy(mdp), by = c("s_id", "a_idx"))
plot_hypergraph(hgf, grid_dim = c(14, 7), cex = 0.8, radx = 0.02, rady = 0.03)

## ----eval=FALSE, include=FALSE----------------------------------------------------------
# # get_policy(mdp)
# # rpo<-calcRPO(mdp, w_lbl, i_a=rep(0,42), criterion="discount", dur=dur_lbl, rate=rate, rate_base=rate_base)
# # policy<-merge(policy,rpo)
# # policy

## ----avePerLac, tidy.opts=list(comment=FALSE)-------------------------------------------
w_lbl <- "Net reward" # the weight we want to optimize (net reward)
dur_lbl <- "Duration" # the duration/time label
run_policy_ite_ave(mdp, w_lbl, dur_lbl)
get_policy(mdp)

## ----echo=TRUE--------------------------------------------------------------------------
run_calc_weights(mdp, w = w_lbl, criterion = "average", dur = "Yield")

## ----Reward/piglet (sow rep), echo=TRUE-------------------------------------------------
run_calc_weights(mdp, w = "Yield", criterion = "average", dur = dur_lbl)

## ----Delete bin, include=FALSE----------------------------------------------------------
do.call(file.remove, list(list.files(pattern = ".bin")))

