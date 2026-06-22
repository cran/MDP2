## ----setup, include=FALSE---------------------------------------------------------------
library(knitr)
options(
  rmarkdown.html_vignette.check_title = FALSE,
  # formatR.arrow = TRUE,
  # scipen=999,
  # digits=5,
  width = 90
) #
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

## ---------------------------------------------------------------------------------------
library(MDP2)

## ----states-----------------------------------------------------------------------------
N <- 5
states <- tibble::tibble(
  idx = 1:N - 1,
  label = paste0("i = ", 1:N)
)
states

## ---------------------------------------------------------------------------------------
Cf <- -10
Cp <- c(0, -7, -7, -5)

## ---------------------------------------------------------------------------------------
Q <- matrix(
  c(
    0.90, 0.10, 0, 0, 0,
    0, 0.80, 0.10, 0.05, 0.05,
    0, 0, 0.70, 0.10, 0.20,
    0, 0, 0, 0.50, 0.50
  ),
  nrow = 4, byrow = T
)

## ----trans_pr---------------------------------------------------------------------------
#' Transition probabilities
#' @param i State (1 <= i <= N).
#' @param a Action (`nr`, `pr` or `fr`).
#' @return A list with non-zero transition probabilities and state index of the transitions.
trans_pr <- function(i, a) {
  pr <- NULL
  idx <- NULL
  if (a == "nr") {
    pr <- Q[i, ]
    idx <- which(pr > 0) # only consider trans pr > 0
    pr <- pr[idx]
    idx <- idx - 1 # since state index is state-1
  }
  if (a == "pr" | a == "fr") {
    pr <- 1
    idx <- 0
  }
  return(list(pr = pr, idx = idx))
}

## ---------------------------------------------------------------------------------------
trans_pr(1, "nr")

## ----buildMDP1, include=FALSE-----------------------------------------------------------
labels <- states$label
w <- binary_mdp_writer("hct611-1_") # use prefix hct611-1_ to the files
w$set_weights(c("Duration", "Net reward"))
w$process() # founder process
w$stage() # a stage with states
w$state(label = labels[1]) # state 1
lst <- trans_pr(1, "nr")
w$action(label = "nr", weights = c(1, 0), pr = lst$pr, id = lst$id, end = TRUE)
w$end_state() # end state 1
for (i in 2:(N - 1)) { # states 2 to N-1
  w$state(label = labels[i])
  lst <- trans_pr(i, "nr")
  w$action(label = "nr", weights = c(1, 0), pr = lst$pr, id = lst$id, end = TRUE)
  lst <- trans_pr(i, "pr")
  w$action(label = "pr", weights = c(1, Cp[i]), pr = lst$pr, id = lst$id, end = TRUE)
  w$end_state()
}
w$state(label = labels[N])
lst <- trans_pr(N, "fr")
w$action(label = "fr", weights = c(2, Cf), pr = lst$pr, id = lst$id, end = TRUE)
w$end_state()
w$end_stage() # end stage
w$end_process() # end process
w$close_writer() # close the binary files

## ----semi-mdp, echo=FALSE, results='hide', message=FALSE, fig.cap="Figure 1: The state-expanded hypergraph for the semi-MDP.", par=TRUE----
mdp <- load_mdp("hct611-1_")
plot(mdp, action_color = "label", mar_y = 0.06)

## ----view_buldMDP1, ref.label='buildMDP1'-----------------------------------------------
labels <- states$label
w <- binary_mdp_writer("hct611-1_") # use prefix hct611-1_ to the files
w$set_weights(c("Duration", "Net reward"))
w$process() # founder process
w$stage() # a stage with states
w$state(label = labels[1]) # state 1
lst <- trans_pr(1, "nr")
w$action(label = "nr", weights = c(1, 0), pr = lst$pr, id = lst$id, end = TRUE)
w$end_state() # end state 1
for (i in 2:(N - 1)) { # states 2 to N-1
  w$state(label = labels[i])
  lst <- trans_pr(i, "nr")
  w$action(label = "nr", weights = c(1, 0), pr = lst$pr, id = lst$id, end = TRUE)
  lst <- trans_pr(i, "pr")
  w$action(label = "pr", weights = c(1, Cp[i]), pr = lst$pr, id = lst$id, end = TRUE)
  w$end_state()
}
w$state(label = labels[N])
lst <- trans_pr(N, "fr")
w$action(label = "fr", weights = c(2, Cf), pr = lst$pr, id = lst$id, end = TRUE)
w$end_state()
w$end_stage() # end stage
w$end_process() # end process
w$close_writer() # close the binary files

## ---------------------------------------------------------------------------------------
get_bin_info_states("hct611-1_")
get_bin_info_actions("hct611-1_")

## ----buildMDP2--------------------------------------------------------------------------
## Define probability matrices
P <- list()
# a = nr (no repair)
P[[1]] <- as.matrix(rbind(Q, 0))

# a = pr (preventive repair)
Z <- matrix(0, nrow = N, ncol = N)
Z[2, 1] <- Z[3, 1] <- Z[4, 1] <- 1
P[[2]] <- Z

# a = fr (forced repair)
Z <- matrix(0, nrow = N, ncol = N)
Z[5, 1] <- 1
P[[3]] <- Z

## Rewards, a 5x3 matrix with one column for each action
R <- matrix(0, nrow = N, ncol = 3)
R[2:4, 2] <- Cp[2:4]
R[5, 3] <- Cf

## State lengths, a 5x3 matrix with one column for each action
D <- matrix(1, nrow = N, ncol = 3)
D[5, 3] <- 2

## Build model using the matrix specification
w <- binary_mdp_writer("hct611-2_")
w$set_weights(c("Duration", "Net reward"))
w$process(P, R, D)
w$close_writer()

## ----buildMDP3--------------------------------------------------------------------------
prefix <- "machine1_"
w <- binary_mdp_writer(prefix)
w$set_weights(c("Net reward"))
w$process()
w$stage() # stage n=0
w$state(label = "dummy")
w$action(label = "buy", weights = -100, pr = c(0.7, 0.3), id = c(0, 1), end = TRUE)
w$end_state()
w$end_stage()
w$stage() # stage n=1
w$state(label = "good")
w$action(label = "mt", weights = 55, pr = 1, id = 0, end = TRUE)
w$action(label = "nmt", weights = 70, pr = c(0.6, 0.4), id = c(0, 1), end = TRUE)
w$end_state()
w$state(label = "average")
w$action(label = "mt", weights = 40, pr = 1, id = 0, end = TRUE)
w$action(label = "nmt", weights = 50, pr = c(0.6, 0.4), id = c(1, 2), end = TRUE)
w$end_state()
w$end_stage()
w$stage() # stage n=2
w$state(label = "good")
w$action(label = "mt", weights = 55, pr = 1, id = 0, end = TRUE)
w$action(label = "nmt", weights = 70, pr = c(0.5, 0.5), id = c(0, 1), end = TRUE)
w$end_state()
w$state(label = "average")
w$action(label = "mt", weights = 40, pr = 1, id = 0, end = TRUE)
w$action(label = "nmt", weights = 50, pr = c(0.5, 0.5), id = c(1, 2), end = TRUE)
w$end_state()
w$state(label = "not working")
w$action(label = "mt", weights = 30, pr = 1, id = 0, end = TRUE)
w$action(label = "rep", weights = 5, pr = 1, id = 3, end = TRUE)
w$end_state()
w$end_stage()
w$stage() # stage n=3
w$state(label = "good")
w$action(label = "mt", weights = 55, pr = 1, id = 0, end = TRUE)
w$action(label = "nmt", weights = 70, pr = c(0.2, 0.8), id = c(0, 1), end = TRUE)
w$end_state()
w$state(label = "average")
w$action(label = "mt", weights = 40, pr = 1, id = 0, end = TRUE)
w$action(label = "nmt", weights = 50, pr = c(0.2, 0.8), id = c(1, 2), end = TRUE)
w$end_state()
w$state(label = "not working")
w$action(label = "mt", weights = 30, pr = 1, id = 0, end = TRUE)
w$action(label = "rep", weights = 5, pr = 1, id = 3, end = TRUE)
w$end_state()
w$state(label = "replaced")
w$action(label = "dummy", weights = 0, pr = 1, id = 3, end = TRUE)
w$end_state()
w$end_stage()
w$stage() # stage n=4
w$state(label = "good", end = TRUE)
w$state(label = "average", end = TRUE)
w$state(label = "not working", end = TRUE)
w$state(label = "replaced", end = TRUE)
w$end_stage()
w$end_process()
w$close_writer()

## ----plotHgf3, echo=FALSE, fig.cap="Figure 2: A finite-horizon MDP", par=TRUE-----------
scrapValues <- c(30, 10, 5, 0) # scrap values (the values of the 4 states at stage 4)
mdp <- load_mdp("machine1_", get_log = FALSE)
plot(mdp, action_color = "label", radx = 0.06, mar_x = 0.065, mar_y = 0.055, state_label = "s_idx|label")

## ----echo=FALSE, fig.cap="Figure 3: The state-expanded hypergraph of the first stage of a hierarchical MDP. Level 0 indicate the founder level, and the nodes indicates states at the different levels. A child process (oval box) is represented using its state-expanded hypergraph (hyperarcs not shown) and is uniquely defined by a given state and action of its parent process."----
knitr::include_graphics("vignette_files/hmdp_index.png")

## ----Generate cow MDP functions,echo=TRUE-----------------------------------------------
library(magrittr)
cow_df <- readr::read_csv("vignette_files/cow.csv")
cow_df

# Weights given a state at level 2
lev1_w <- function(s0Idx, n1Idx, s1Idx, a1Lbl) {
  return(cow_df %>%
    dplyr::filter(s0 == s0Idx & n1 == n1Idx & s1 == s1Idx & label == a1Lbl) %>%
    dplyr::select(Duration, Reward, Output) %>% as.numeric())
}
lev1_w(2, 2, 1, "Keep") # good genetic merit, lactation 2, avg yield, keep action

# Trans pr given a state at level 2
lev1_pr <- function(s0Idx, n1Idx, s1Idx, a1Lbl) {
  return(cow_df %>%
    dplyr::filter(s0 == s0Idx & n1 == n1Idx & s1 == s1Idx & label == a1Lbl) %>%
    dplyr::select(scp0:last_col()) %>% as.numeric())
}
lev1_pr(2, 2, 1, "Replace") # good genetic merit, lactation 2, avg yield, replace action

## ----Generate cow MDP,echo=TRUE, tidy=FALSE---------------------------------------------
lblS0 <- c('Bad genetic level', 'Avg genetic level', 'Good genetic level')
lblS1 <- c('Low yield', 'Avg yield', 'High yield')
prefix<-"cow_"
w<-binary_mdp_writer(prefix)
w$set_weights(c("Duration", "Net reward", "Yield"))
w$process()
  w$stage()   # stage 0 at founder level
    for (s0 in 0:2) {
      w$state(label=lblS0[s0+1])   # state at founder
        w$action(label="Keep", weights=c(0,0,0), prob=c(2,0,1))   # action at founder
          w$process()
            w$stage()   # dummy stage at level 1
               w$state(label="Dummy")
                 w$action(label="Dummy", weights=c(0,0,0), 
                          prob=c(1,0,1/3, 1,1,1/3, 1,2,1/3), end=TRUE)
               w$end_state()
            w$end_stage()
            for (d1 in 1:4) {
              w$stage()   # stage at level 1
                for (s1 in 0:2) {
                  w$state(label=lblS1[s1+1])
                    if (d1!=4) {
                      w$action(label="Keep", weights=lev1_w(s0,d1,s1,"Keep"), 
                               prob=lev1_pr(s0,d1,s1,"Keep"), end=TRUE)
                    }
                    w$action(label="Replace", weights=lev1_w(s0,d1,s1,"Replace"), 
                             prob=lev1_pr(s0,d1,s1,"Replace"), end=TRUE)
                  w$end_state()
                }
              w$end_stage()
            }
          w$end_process()
        w$end_action()
      w$end_state()
    }
  w$end_stage()
w$end_process()
w$close_writer()

## ----plotHMDP, echo=FALSE, message=FALSE, par=TRUE--------------------------------------
mdp <- load_mdp(prefix)
# hgf <- list(nodes = NULL, hyperarcs = NULL)
# hgf %>% plot_hypergraph(grid_dim=c(14,7), cex = 0.8, show_grid = T)
hgf <- get_hypergraph(mdp)
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
plot_hypergraph(grid_dim = c(14, 7), hgf, cex = 0.8, radx = 0.02, rady = 0.03)

## ----Delete bin, include=FALSE----------------------------------------------------------
do.call(file.remove, list(list.files(pattern = ".bin")))

