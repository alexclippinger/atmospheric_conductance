
#' Compute Atmospheric Conductance
#'
#' @param windspeed  cm/s
#' @param windspeed_ht  cm
#' @param k_d 
#' @param k_0 
#' @param vegetation_ht  cm
#'
#' @return numeric cm/s
#' @export
#'
#' @examples
compute_atmospheric_conductance <- function(windspeed = 250, 
                                            windspeed_ht = NULL,  
                                            k_d = 0.7,
                                            k_0 = 0.1,
                                            vegetation_ht = 1000  
                                            ) {
  
  if (is.null(windspeed_ht)) {
    windspeed_ht = vegetation_ht + 200
  }
  
  z_d = k_d * vegetation_ht
  z_0 = k_0 * vegetation_ht
  
  if (windspeed_ht < z_d) {
    stop("Windspeed height too low")
  }
  
  conductance = windspeed / (6.25 * log((windspeed_ht - z_d)/z_0)**2)  
  
  return(list(conductance = conductance))
}