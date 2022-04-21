
compute_atmospheric_conductance <- function(windspeed = 250,  # cm/s
                                            windspeed_ht = NULL,  # cm
                                            k_d = 0.7,
                                            k_0 = 0.1,
                                            vegetation_ht = 1000  # cm
                                            ) {
  if (is.null(windspeed_ht)) {
    windspeed_ht = vegetation_ht + 200  
  }
  
  z_d = k_d*vegetation_ht
  z_0 = k_0*vegetation_ht
  
  conductance = windspeed_ht / (6.25 * log((windspeed_ht - z_d)/z_0)**2)  

}