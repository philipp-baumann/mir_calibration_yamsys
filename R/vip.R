# https://gist.github.com/antoinestevens/4a38f334ce9dcb92232d

VIP <- function(object) {
  ### VIP.R: Implementation of VIP (variable importance in projection)(*) for the 'pls' package.
  ### $Id: VIP.R,v 1.2 2007/07/30 09:17:36 bhm Exp $
  
  ### Copyright ? 2006,2007 Bjorn-Helge Mevik
  ### This program is free software; you can redistribute it and/or modify
  ### it under the terms of the GNU General Public License version 2 as
  ### published by the Free Software Foundation.
  ###
  ### This program is distributed in the hope that it will be useful,
  ### but WITHOUT ANY WARRANTY; without even the implied warranty of
  ### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  ### GNU General Public License for more details.
  
  ### A copy of the GPL text is available here:
  ### http://www.gnu.org/licenses/gpl-2.0.txt
  
  ### Contact info:
  ### Bjorn-Helge Mevik
  ### bhx6@mevik.net
  ### Rodtvetvien 20
  ### N-0955 Oslo
  ### Norway
  
  ### (*) As described in Chong, Il-Gyo & Jun, Chi-Hyuck, 2005, Performance of
  ### some variable selection methods when multicollinearity is present,
  ### Chemometrics and Intelligent Laboratory Systems 78, 103--112.
  
  ## VIP returns all VIP values for all variables and all number of components,
  ## as a ncomp x nvars matrix.
  if (object$method != "oscorespls")
    stop("Only implemented for orthogonal scores algorithm.  Refit with 'method=\"oscorespls\"'")
  if (nrow(object$Yloadings) > 1)
    stop("Only implemented for single-response models")
  
  SS <- c(object$Yloadings)^2 * colSums(object$scores^2)
  Wnorm2 <- colSums(object$loading.weights^2)
  SSW <- sweep(object$loading.weights^2, 2, SS / Wnorm2, "*")
  sqrt(nrow(SSW) * apply(SSW, 1, cumsum) / cumsum(SS))
}