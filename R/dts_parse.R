
#' @title Parse the output of an object returned by deSolve
#' @param pars a [list]
#' @param dts_out a [matrix] of orbits returned by deSolve
#' @return varslist a [list]
#' @export
parse_dts_out <- function(dts_out, pars){
  varslist = list()

  s = length(pars$Lpar)
  if(s>0)
    for(ix in 1:s)
      varslist$L[[ix]]= parse_dts_out_L(dts_out, pars, ix)

  s = length(pars$MYZpar)
  if(s>0)
    for(ix in 1:s)
      varslist$MYZ[[ix]]= parse_dts_out_MYZ(dts_out, pars, ix)

  s = length(pars$Xpar)
  if(s>0)
    for(ix in 1:s)
      varslist$XH[[ix]]= parse_dts_out_X(dts_out, pars, ix)

  varslist$terms = compute_terms(varslist, dts_out, pars, 1, 1)
  varslist$dts_out = dts_out
  return(varslist)
}

#' @title Parse the output of an object returned by deSolve
#' @param vec a [vector] with the variables, as returned by rootsolve
#' @param pars a [list]
#' @return varslist a [list]
#' @export
parse_dts_out_vec <- function(vec, pars){
  dts_out = rbind(c(0,vec), c(0,vec))
  varslist = parse_dts_out(dts_out, pars)

  for(i in 1:length(varslist$XH))
    varslist$XH[[i]] = tail(varslist$XH[[i]],1)
  for(i in 1:length(varslist$MYZ))
    varslist$MYZ[[i]] = tail(varslist$MYZ[[i]],1)
  for(i in 1:length(varslist$L))
    varslist$L[[i]] = tail(varslist$L[[i]],1)

  #varslist$terms = compute_terms_steady(varslist, vec, pars)
  return(varslist)
}

#' @title Set the initial values to the last values of the last simulation
#' @param pars a [list]
#' @return y a [numeric] vector
#' @export
dts_last_to_inits <- function(pars){
  y0 <- tail(pars$orbits$dts_out, 1)[-1]
  pars <- update_inits(y0, pars)
  return(pars)
}
