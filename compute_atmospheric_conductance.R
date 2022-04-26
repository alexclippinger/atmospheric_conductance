
#' Compute Atmospheric Conductance
#' 
#' This function computes atmospheric conductance (how easily water diffuses into the air) as a function of vegetation height and wind speed.
#'
#' @param windspeed Wind speed (cm/s). Default = 250.
#' @param windspeed_ht  Height at which wind speed is measured (cm). Must be higher than the vegetation height. Default = vegetation height (cm) + 200.
#' @param k_d Default = 0.7.
#' @param k_0 Default = 0.1.
#' @param vegetation_ht  Vegetation height (cm). Default = 1000.
#'
#' @return Atmospheric conductance (cm/s). Returned as list (length 1).
#' @export
#'
#' @examples

# compute_atmospheric_conductance(wind_speed=250, wind_speed_ht=NULL, k_d=0.7, k_0=0.1, vegetation_ht=1000)
compute_atmospheric_conductance <- function(wind_speed = 250, 
                                            wind_speed_ht = NULL,  
                                            k_d = 0.7,
                                            k_0 = 0.1,
                                            vegetation_ht = 1000  
                                            ) {
  
  # Set wind speed height equal to vegetation height + 200 if no value provided
  if (is.null(wind_speed_ht)) {
    wind_speed_ht = vegetation_ht + 200
  }
  
  # Stop if user provided wind speed height is lower than vegetation
  if (wind_speed_ht <= vegetation_ht) {
    stop("Windspeed height must be greater than vegetation height.")
  }
  
  z_d = k_d * vegetation_ht
  z_0 = k_0 * vegetation_ht
  
  # Compute atmospheric conductance (how easily vapor diffuses from vegetation)
  conductance = wind_speed / (6.25 * log((wind_speed_ht - z_d)/z_0)**2)  
  
  return(list(conductance = conductance))
}
