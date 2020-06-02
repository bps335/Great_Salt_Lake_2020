###################### Bioturbation energy calculation ###########################

## This script performs the energy-balance between cement growth
## and bioturbation in section 4.5 and generates Figure 8.


########################### Load data and packages ###############################

## Clear any variables from the global environment.

  library(ggplot2)

## Load the following packages

  remove(list = ls())

############################## Define constants ##################################
  
## The constants from Table 1 are given below. The references for each can be 
## found in the main text. 

  k = 12.9*10^(-6)*24*365                    # rate constant (mol m-2 yr-1)
  
  n = 2.26                                   # power law constant (-)
  
  M = .1                                     # molar mass of aragonite (kg mol-1)
  
  rho_c = 2850                               # density of aragonite (kg m-3)
  
  sigma = 80e-3                              # calcite/water free energy (J m-2)
  
  roughness = 11.6                           # Surface area roughness (m2 kg-1)
  
  v_low = 87.6                               # Minimum burrowing velocity for 
                                             # deposit-feeding bivalves
                                             # and arthropods (m yr-1)
  
  v_high = 2540.4                            # Minimum burrowing velocity for 
                                             # deposit-feeding bivalves
                                             # and arthropods (m yr-1)
  
  C = .064                                   # External work perfomed on sediments
                                             # burrowing animals (J g-1 m-1)
  
  GBR_low = .94                              # Minimum infaunal density behind the
                                             # Great Barrier Reef (g m-2)
  
  GBR_high = 3.37                            # Maximum infaunal density behind the
                                             # Great Barrier Reef
  
  PG_low = .778                              # Minimum infaunal density behind in
                                             # the Persian Gulf (g m-2)
  
  PG_high = 1.706                            # Maximum infaunal density behind in
                                             # the Persian Gulf (g m-2) 
  
  GBR_omega_low = 3                          # Low end of saturation state 
                                             # estimated for Great Barrier Reef
  
  GBR_omega_high = 5                         # High end of saturation state 
                                             # estimated for Great Barrier Reef
  
  PG_omega_low = 5.5                         # Low end of saturation state 
                                             # estimated for the Persian Gulf
  
  PG_omega_high = 10.7                       # High end of saturation state 
                                             # estimated for the Persian Gulf
  
## User-defined parameters. The default volume, .02 m^3, represents  1m x 1m x 
## 2 cm volume, i.e., we are interested in energy transfer within the upper 2 cm 
## of the sediments. A porosity of 40% (phi = .4) is reasonable for barely 
## consolidated coarse sediments. 

  Vt = .02                                   # volume sediment + porewater (m3)
  phi = .40                                  # porosity(m3 m-3)
  Vc = Vt*(1-phi)                            # Carbonate volume (m3)
  
############# Calculate energy change associated with cement growth ##############

  ## Calculate the initial surface area 

  S_0 = roughness*Vc*rho_c                   # Initial reactive surface area (m2)

## Create vectors with omega values 

  omega_1 <- seq(from = 2,                   # two sets of omega values to contour
               to = 10, by = 1)     
  omega_2 <-c(5, 10)

## Combine the constants for the magnitude of the energy change in Eqn. 4. 

  temp = sigma*2/3*S_0^2*k*M/(rho_c*Vt)    

## Multiply constants by the rate law for the omega vectors

  omega_contour_1 <- temp*(omega_1-1)^n      # calculates horizontal contours for 
  omega_contour_2 <- temp*(omega_2-1)^n      # different omega values

  Chem_E <-seq(from = 1, to = 600, by = 1)   # sets limits and spacing for y axis
  K_E <-seq(from =1, to = 600, by = 1)       # sets limits and spacing for x axis
  
############# Calculate energy change associated with bioturbation ###############  

  PG_min = C*v_low*PG_low                    # Minimum energy for the Persian Gulf
  PG_max = C*v_high*PG_high                  # Maximum energy for the Persian Gulf
  
  GBR_min = C*v_low*GBR_low                  # Maximum energy behind the Great 
                                             # Barrier Reef
  
  GBR_max = C*v_high*GBR_high                # Maximum energy behind the Great 
                                             # Barrier Reef

########################## Collect and plot results ##############################

  data <- data.frame(Chem_E,K_E)

  ggplot(data, aes(x = K_E, y = Chem_E)) + 
    theme_classic() +
    scale_x_log10() +
    scale_y_log10() +
    
## Plot horizontal dashed lines corresponding energy change evaluated at different 
## saturation states. Red lines mark omega = 5 and omega = 10. 
  
  geom_hline(yintercept = c(omega_contour_1), linetype = 'dashed')+
    
  geom_hline(yintercept = c(omega_contour_2), color = 'red') +
    
## Add a box that represents the range of conditions in the Persian Gulf. 
  
  annotate(geom = "rect", 
          xmin = PG_min, ymin = temp*(PG_omega_low-1)^n, 
          xmax = PG_max, ymax = temp*(PG_omega_high-1)^n,
          fill = "red", alpha = 0.4) +
  
## Add a box that represents the range of conditions behind the Great Barrier Reef. 
    
  annotate(geom = "rect", 
           xmin = GBR_min, ymin = temp*(GBR_omega_low-1)^n, 
           xmax = GBR_max, ymax = temp*(GBR_omega_high-1)^n,
           fill = "dark blue", alpha = 0.4) +
    
## Add a 1:1 line on the plot

  geom_line(data = data, aes(x = K_E, y = 1*Chem_E), color = 'black') +

## Add labels and resize fonts for export
  labs(x = 'bioturbation (J/yr)', y = 'Cement growth (J/yr)', 
       title = 'Rate of Interfacial Work') +
    
  theme(plot.title = element_text(size = rel(1.5), hjust = .5)) +
    
  theme(axis.title  = element_text(size = rel(1.2), hjust = .5)) 
