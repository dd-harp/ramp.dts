
#' @title Modify parameters due to exogenous forcing by all kinds of control
#' @description Implements [EIP] for the fixed_dts model (the EIP is constant)
#' @inheritParams EIP
#' @return the EIP maturation vector G [numeric]
#' @export
EIP.fixed_dts <- function(t, MYZpar){
  G <- rep(0, MYZpar$max_eip)
  G[MYZpar$eip] <- 1
  return(G)
}

#' @title Set up a fixed_dts model for the EIP
#' @inheritParams setup_EIP
#' @return [list]
#' @export
setup_EIP.fixed_dts <- function(EIPname, MYZpar, MYZopts=list()){
  setup_eip_fixed_dts(MYZopts, MYZpar)
}

#' @title Set up a fixed_dts model for the EIP
#' @param MYZopts a [list]
#' @param MYZpar the MYZ parameters
#' @return [list]
#' @export
setup_eip_fixed_dts = function(MYZopts=list(), MYZpar){
  with(MYZpar,
    with(MYZopts,{
      EIPmod <- list()
      class(EIPmod) <- 'fixed_dts'
      MYZpar$EIPmod <- EIPmod
      MYZpar$eip <- eip
      MYZpar$max_eip <- eip
      return(MYZpar)
}))}
