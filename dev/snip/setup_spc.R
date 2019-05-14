####
# setup script
####

setwd("C:\USR\Downloads\split")

install.packages("installr")
library(installr)
updateR()

install.packages("pacman")
library(pacman)
p_load(tidyverse, entropy, multidplyr, parallel)


####
# functions
####
vdiff <- function(x,n,fun) sapply(n, function(i) fun(x,i))
vlag  <- function(x,n) vdiff(x,0:n,dplyr::lag)
vlead <- function(x,n) vdiff(x,0:n,dplyr::lead)

novelty2 <- function(w, cl, env) {
  # produce the lags (same shape as document)
  vlag(1:nrow(env$mat), w) %>%          
    parApply(cl = cl, X = ., MARGIN = 1,
             function(idx, env) {
               # then for each row (document)
               #print(env)
               mean(unlist(lapply(idx[2:length(idx)], function(i) {
                 #for each lag
                 #check if it's na (we're at beginning / end of data)
                 if (is.na(i)) return(NA)             
                 ## calculate surprise from past to present
                 KL.plugin(env$mat[i,], env$mat[idx[1],], unit = "log2")
                 
               })))}, env = env)}


transience2 <- function(w, cl, env) {
  # produce the lags (same shape as document)
  vlag(1:nrow(env$mat), w) %>%          
    parApply(cl = cl, X = ., MARGIN = 1,
             function(idx, env) {
               # then for each row (document)
               #print(env)
               mean(unlist(lapply(idx[2:length(idx)], function(i) {
                 #for each lag
                 #check if it's na (we're at beginning / end of data)
                 if (is.na(i)) return(NA)             
                 ## calculate surprise from present to future
                 KL.plugin(env$mat[idx[1],], env$mat[i,], unit = "log2")
                 
               })))}, env = env)}


z <- function(d) (d - mean(d)) / sd(d)