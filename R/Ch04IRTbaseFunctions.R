#' @title Two-Parameter Logistic Model
#' @description
#' The two-parameter logistic model is a classic model that defines
#' the probability of a student with ability theta successfully
#' answering item j, using both a slope parameter and a
#' location parameter.
#' @param a slope parameter
#' @param b locaiton parameter
#' @param theta ability parameter

TwoPLM <- function(a,b,theta){
  p = 1/(1+exp(-a*(theta - b)))
  return(p)
}

#' @title Three-Parameter Logistic Model
#' @description
#' The three-parameter logistic model is a model where the lower
#' asymptote parameter c is added to the 2PLM
#' @param a slope parameter
#' @param b locaiton parameter
#' @param c lower asymptote parameter
#' @param theta ability parameter

ThreePLM <- function(a,b,c,theta){
  p = c + (1-c)/(1+exp(-a*(theta - b)))
  return(p)
}

#' @title Four-Parameter Logistic Model
#' @description
#' The four-parameter logistic model is a model where one additional
#' parameter d, called the upper asymptote parameter, is added to the
#' 3PLM.
#' @param a slope parameter
#' @param b locaiton parameter
#' @param c lower asymptote parameter
#' @param d upper asymptote parameter
#' @param theta ability parameter

ThreePLM <- function(a,b,c,d,theta){
  p = c + (d-c)/(1+exp(-a*(theta - b)))
  return(p)
}

#' @title Rasch Model
#' @description
#' The one-parameter logistic model is a model with only one parameter b.
#' This model is a 2PLM model in which a is constarind to 1.
#' This model is also called the Rasch model.
#' @param b slope parameter
#' @param theta ability parameter

ThreePLM <- function(b,theta){
  p = 1/(1+exp(-1 * (theta - b)))
  return(p)
}

