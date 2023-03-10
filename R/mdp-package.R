# Create and optimize MDPs or hierarchical MDPs with discrete time steps and state space.
#
# @section To do:
#
# Nested loading in memory (specify a HMDP with special actions containing child + father jump actions)
# Idea when define the proc with an external nested process use
# w$includeProcess(prefix, transPr, index) (specify the child jump action)
#   w$stage()
#     w$state()
#       w$action(...) (specify father jump action scope must be zeor)
#       w$action(...) (specify father jump action scope must be zeor)
#     w$endState()
#   w$endStage()
# w$endIncludeProcess()
#
# The hgf then is formed with a subprocess mimic the 1. and last stage of the external proc, i.e. we include the jump pr in the hgf
# We need a new binary file "externalProcess.bin" for storing the nested process in the format "n0 s0 a0 n1 s1 prefix -1 ..." which specify which stage contain the states corresponding to the 1. stage of the nested process (it is here the nested hfg must be loaded and calculated
#
#
# @name MDP2
# @aliases MDP-package MDP
# @docType package
# @title Markov Decision Processes (MDPs) in R
# @useDynLib MDP2
# @importClassesFrom Rcpp "C++Object"
# NA



#' @seealso [loadMDP()].
#' @keywords internal
#' @useDynLib MDP2
#' @importClassesFrom Rcpp "C++Object"
"_PACKAGE"
