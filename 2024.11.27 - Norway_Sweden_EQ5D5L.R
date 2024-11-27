
# Install packages if missing
if(class(try(find.package("eq5dsuite"), silent = TRUE)) == "try-error") install.packages("eq5dsuite")
if(class(try(find.package("valueSetCompare"), silent = TRUE)) == "try-error") install.packages("valueSetCompare")

# Attach libraries
library(eq5dsuite)
library(valueSetCompare)

# Add value sets for Norway and Sweden if missing
if(any(!c("NO", "SE") %in% names(.Options$eq.env$vsets5L_combined))) { # At least one of them is missing
  
  # Generate data.frame with all  3125 EQ-5D-5L health states
  allst <- make_all_EQ_states()
  
  # Add dummies
  allst <- cbind(allst, make_dummies(allst, incremental = F))
  
  # Add 5-digit state vector
  allst$state <- toEQ5Dindex(allst)
  
  
  
  pkgenv <- .Options$eq.env
  
  
  # Sweden
  if(!"SE" %in% names(.Options$eq.env$vsets5L_combined)) { # Sweden missing
    
    swevals <- c(MO2 = 0.022, MO3 = 0.026, MO4 = 0.087, MO5 = 0.132,
                 SC2 = 0.019, SC3 = 0.056, SC4 = 0.133, SC5 = 0.159,
                 UA2 = 0.022, UA3 = 0.048, UA4 = 0.131, UA5 = 0.187,
                 PD2 = 0.010, PD3 = 0.047, PD4 = 0.319, PD5 = 0.442,
                 AD2 = 0.031, AD3 = 0.122, AD4 = 0.277, AD5 = 0.394)
    
    # Make predictions for all 3125 health states
    allst$SE <- 1 - (as.matrix(allst[,tolower(names(swevals))]) %*% swevals)[,1]
    # Add value sets to EQ5Dsuite package
    eqvs_add(allst[, c("state", "SE")], '5L', "Sweden", code2L = "SE", code3L = "SWE", description = "Swedish -5L value set")
    
    pkgenv$vsets5L_combined$SE <- pkgenv$uservsets5L$SE <- allst$SE
    
    pkgenv$country_codes[['5L']] <- rbind(pkgenv$country_codes[['5L']], 
                                          data.frame(Name = "Sweden", 
                                                     Name_short = "Sweden", 
                                                     ISO3166Alpha2 = "SE",
                                                     ISO3166Alpha3 = "SWE"))
    
    rm(swevals)
  }
  
  
  
  
  # Norway
  if(!"NO" %in% names(.Options$eq.env$vsets5L_combined)) { # Norway missing
    
    Ls <- c("2" = 0.152186325,
            "3" = 0.317050211,
            "4" = 0.774596154,
            "5" = 1)
    Ds <- c(MO = 0.205157888,
            SC = 0.205830944,
            UA = 0.179131251,
            PD = 0.390902789,
            AD = 0.472330989) 
    
    # Transform to 20-parameter coefficient
    norvals <- structure(.Data = as.vector(Ls %o% Ds), .Names = as.vector(outer(names(Ls), names(Ds), \(x,y) paste0(y, x))))
    
    
    # Make predictions for all 3125 health states
    allst$NO <- 1 - (as.matrix(allst[,tolower(names(norvals))]) %*% norvals)[,1]
    
    
    
    # Add value sets to EQ5Dsuite package
    eqvs_add(allst[, c("state", "NO")], '5L', "Norway", code2L = "NO", code3L = "NOR", description = "Norwegian -5L value set")
    
    
    pkgenv$vsets5L_combined$NO <- pkgenv$uservsets5L$NO <- allst$NO
    
    
    pkgenv$country_codes[['5L']] <- rbind(pkgenv$country_codes[['5L']], 
                                          data.frame(Name = "Norway", 
                                                     Name_short = "Norway", 
                                                     ISO3166Alpha2 = "NO",
                                                     ISO3166Alpha3 = "NOR"))
    
    rm(norvals, Ls, Ds)
  }
  rm(pkgenv, allst)
  
  
  
  
  
}

# Example: get values for states 11234 and 44444 for Norway, Sweden, US

eq5d5l(c(11234, 44444), country = c("NO", "SE", 'US'))

# Example 2: get values for the same states using a data.frame with levels for each dimension

example_data <- toEQ5Ddims(c(11234, 44444))

eq5d5l(example_data, country = c("NO", "SE", 'US'))










# Plot distributions of EQ-5D-5L values for Norway, Sweden, US, Denmark, and UK crosswalk
(dplot <- density_plot_theorical(value_sets_5L = c("NO", "SE", "US", "DK"), 
                                 value_sets_XW = "UK", 
                                 line_types = c("solid", "dotted", "dashed", "longdash", "twodash"),
                                 color_palette = c("#374E55FF", "#DF8F44FF", "#00A1D5FF", "#B24745FF", "#6A6599FF")
                                 ))


# (dplot <- density_plot_theorical(value_sets_5L = c("NO"), value_sets_XW = "UK", line_types = c("solid", "dotted", "dashed", "longdash")))


# This takes some time, perhaps a minute or three, therefore commented out
# Severity ribbon plots for the same set of countries


# cdta[, c("NO", "SE", "US", "DK")] <- eq5d5l(cdta$profile5L, country = c("NO", "SE", "US", "DK"))
# cdta$UKXW <- eq5d(x = cdta$profile5L,country = "UK", version = 'xw' )
# 
# 
# splot <- severity_ribbon_plot(df = cdta, utility_columns = c("NO", "SE", "US", "DK", "UKXW"), 
#                                color_palette = c(
#                                                  "#B24745FF", "#374E55FF", "#DF8F44FF","#6A6599FF", "#00A1D5FF", "#79AF97FF","#80796BFF", 
#                                "#fccde5", "#ffff67", "#80b1d3"))
# 
# splot$plot + ylab('EQ-5D-5L Utility value') + xlab('VAS score')



# Theoretical properties
Comp_utils <- t(compute_utility_stats(value_sets_5L = c("NO", "SE", "US", "DK"), value_sets_XW = "UK", format_results = T))

Comp_utils <- Comp_utils[, c(1, 2, 4, 3, 5)]

colnames(Comp_utils) <- c("Norway", "Sweden", "Denmark", "US", "UK crosswalk")

Comp_utils










