################# Cement timescale calculations #################

## This script calculates the timescale for the growth of cements
## in section 4.4 in the main manuscript. 

## The constants from Table 1 are given below. The references for
## each can be found in the main text. 

  k = 12.9*10^(-6)*24          # rate constant (mol m-2 day-1)
  n = 2.26                     # power law constant (dimensionless)
  M = .1                       # molar mass of aragonite (kg mol-1)
  rho_c = 2850                 # density of aragonite (kg m-3)

## User-defined parameters, omega and dx
  
  omega = 3                    # The aragonite saturation state. 
                               # Range of values in GSL is 2-4 
                               # as given by Ingalls et al., 2020
  
  dx = 5e-6                    # The length of the cements. Units 
                               # are converted to meters using a  
                               # factor of 1m = 10e-6 microns. 
  
## Plug everything into Eqn. 2 in the main text. Dimensional 
## analysis shows that the output is in days. 
  
  days = (rho_c*dx)/(M*k*(omega-1)^n)

