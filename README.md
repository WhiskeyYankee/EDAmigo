# EDAmigo
## What does EDAmigo do?
Welcome to the EDAmigo package! EDAmigo is an assistant for conducting Exploratory Data Analysis (EDA). Users can fully automate the cleaning, processing, and visualization of their data, or use the interactive features to assist in better understanding their data.

## How to install EDAmigo?
This package is currently not in CRAN; however, you can easily download the package to your local machine for use. Simply click the green 'Code' button on the Git webpage, select 'Download Zip.' Once the package is downloaded, unzip the files to your R directory. Then, call 'library(EDAmigo)' to load the package into your R session!



## EDAmigo Vignette
EDAmigo is a package designed to make Exploratory Data Analysis (EDA) simple. It provides novice users with streamlined options for automatically cleaning, processing, and viewing their data. However, it also provides advanced users with the ability to perform EDA functions in an interactive manner.

Begin by loading the EDAmigo package. The package comes with two example datasets: Fires and Finance. We will use the fires data for this example

library(EDAmigo)

# Define the dataframe
df <- fires
The package requires all dates and times to be properly classed as ‘Date’, ‘POSIXlt’, or ‘POSIXct.’ Let’s check the classes of our dataframe columns.

str(df)
#> 'data.frame':    623 obs. of  30 variables:
#>  $ Lat                     : num  39.8 39.8 39.5 39.5 39.5 ...
#>  $ Long                    : num  -106 -106 -106 -106 -106 ...
#>  $ Region                  : chr  "D" "D" "D" "D" ...
#>  $ UniqueFireIdentifier    : chr  "2022-COCCX-000284" "2019-COCCX-000008" "2021-COPSF-000130" "2023-COPSF-000123" ...
#>  $ ContainmentDateTime     : POSIXct, format: "2022-06-15 14:00:00" NA ...
#>  $ ControlDateTime         : POSIXct, format: "2022-06-15 14:00:00" NA ...
#>  $ IncidentSize            : num  0.2 0.1 40 1 0.5 0.25 0.1 0.1 1 0.1 ...
#>  $ DispatchCenterID        : chr  "COFTC" "COFTC" "COPBC" "COPBC" ...
#>  $ FireCause               : chr  "Undetermined" "Human" "Unknown" "Undetermined" ...
#>  $ FireCauseGeneral        : chr  "" "" "" "" ...
#>  $ FireCauseSpecific       : chr  "" "" "" "" ...
#>  $ FireDepartmentID        : int  NA NA NA NA NA NA NA NA NA NA ...
#>  $ FireDiscoveryDateTime   : POSIXct, format: "2022-06-13 21:39:00" "2019-01-25 21:49:42" ...
#>  $ FireOutDateTime         : POSIXct, format: "2022-06-15 14:00:00" "2019-01-25 21:52:00" ...
#>  $ IncidentName            : chr  "Mule" "I-70 MM238" "Harris Park RX- South Platte RD" "Harris Park RX" ...
#>  $ IncidentShortDescription: chr  "" "" "" "" ...
#>  $ IncidentTypeCategory    : chr  "WF" "WF" "RX" "RX" ...
#>  $ InitialResponseAcres    : num  NA 0.1 70 100 0.5 NA 0.1 NA NA 0.1 ...
#>  $ InitialResponseDateTime : POSIXct, format: NA NA ...
#>  $ IsMultiJurisdictional   : int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ IsReimbursable          : int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ LocalIncidentIdentifier : int  284 8 130 123 1523 1541 565 269 1809 461 ...
#>  $ POOCounty               : chr  "Clear Creek" "Clear Creek" "Park" "Park" ...
#>  $ POODispatchCenterID     : chr  "COFTC" "COFTC" "COPBC" "COPBC" ...
#>  $ POOFips                 : int  8019 8019 8093 8093 8093 8093 8019 8019 8093 8047 ...
#>  $ POOJurisdictionalUnit   : chr  "" "COARF" "COPSF" "COPSF" ...
#>  $ POOLandownerCategory    : chr  "Private" "" "" "USFS" ...
#>  $ POOLandownerKind        : chr  "Private" "" "" "Federal" ...
#>  $ POOProtectingUnit       : chr  "COCCX" "COCCX" "COPSF" "COPSF" ...
#>  $ POOState                : chr  "US-CO" "US-CO" "US-CO" "US-CO" ...
All of our date columns are type ‘Date.’ However, there are several special characters, improperly typed columns, and missing values. We can either use the autoClean() function to handle all of these situations at one time, or we can use handleSpecial(), detectTypes(), and handleMissing() individually. Let’s use the autoClean() function.

# Store output for future use.
# Pass in:  
  # the dataframe, 
  # special_user_level set to 0 indicating automatic special character removal with no user interaction
  # a factor tolerance of 1% to automatically coerce values with 1% or less unique values to factors
  # a user tolerance of 20%, this will prompt the user for direction when a column has <= 30% unique values
  # drop_user_level of 1, indicating that the user prefers to interact with the handleMissing() function

result <- autoClean(df, special_user_level = 0, factor_tol = 10, type_user_tol = 10, drop_user_level = 0, impute_user_level = 0, impute_factors = TRUE)
Now, let’s examine the output of autoClean(). The function returned a list of 6 elements.

clean_df
This is the resulting dataframe with special characters removed, columns coerced to an appropriate class, and missing values handled. Compared to our original dataframe, we see that 5 columns were removed and that many columns have been coerced to a different class.

# Store our clean dataframe for future use
cleaned_fires <- result$clean_df
str(cleaned_fires)
#> 'data.frame':    623 obs. of  25 variables:
#>  $ Lat                    : num  39.8 39.8 39.5 39.5 39.5 ...
#>  $ Long                   : num  -106 -106 -106 -106 -106 ...
#>  $ Region                 : Factor w/ 2 levels "C","D": 2 2 2 2 2 2 2 2 2 2 ...
#>  $ UniqueFireIdentifier   : chr  "2022-COCCX-000284" "2019-COCCX-000008" "2021-COPSF-000130" "2023-COPSF-000123" ...
#>  $ ContainmentDateTime    : chr  "2022-06-15 140000" NA NA NA ...
#>  $ ControlDateTime        : chr  "2022-06-15 140000" NA NA NA ...
#>  $ IncidentSize           : num  0.2 0.1 40 1 0.5 0.25 0.1 0.1 1 0.1 ...
#>  $ DispatchCenterID       : Factor w/ 5 levels "","COFTC","COPBC",..: 2 2 3 3 3 3 2 2 3 2 ...
#>  $ FireCause              : Factor w/ 5 levels "","Human","Natural",..: 4 2 5 4 3 2 2 2 4 2 ...
#>  $ FireDiscoveryDateTime  : chr  "2022-06-13 213900" "2019-01-25 214942" "2021-03-01 160136" "2023-03-03 192717" ...
#>  $ FireOutDateTime        : chr  "2022-06-15 140000" "2019-01-25 215200" NA NA ...
#>  $ IncidentName           : chr  "Mule" "I-70 MM238" "Harris Park RX- South Platte RD" "Harris Park RX" ...
#>  $ IncidentTypeCategory   : Factor w/ 2 levels "RX","WF": 2 2 1 1 2 2 2 2 1 2 ...
#>  $ InitialResponseAcres   : num  0.1 0.1 70 100 0.5 0.1 0.1 0.1 0.1 0.1 ...
#>  $ IsMultiJurisdictional  : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ IsReimbursable         : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ LocalIncidentIdentifier: num  284 8 130 123 1523 ...
#>  $ POOCounty              : Factor w/ 15 levels "Adams","Arapahoe",..: 5 5 12 12 12 12 5 5 12 9 ...
#>  $ POODispatchCenterID    : Factor w/ 5 levels "","COFTC","COPBC",..: 2 2 3 3 3 3 2 2 3 2 ...
#>  $ POOFips                : Factor w/ 15 levels "39009","39045",..: 10 10 15 15 15 15 10 10 15 13 ...
#>  $ POOJurisdictionalUnit  : Factor w/ 17 levels "","COARF","COBLDS",..: 8 2 8 8 8 8 2 8 8 8 ...
#>  $ POOLandownerCategory   : Factor w/ 10 levels "","BLM","City",..: 7 9 9 9 9 9 9 7 9 7 ...
#>  $ POOLandownerKind       : Factor w/ 4 levels "","Federal","Other",..: 4 2 2 2 2 2 2 4 2 4 ...
#>  $ POOProtectingUnit      : Factor w/ 22 levels "COADX","COAPX",..: 5 5 15 15 15 15 3 5 15 10 ...
#>  $ POOState               : Factor w/ 2 levels "US-CO","US-OH": 1 1 1 1 1 1 1 1 1 1 ...
Now, let’s look at the remaining output, which are derived from other functions.

handleSpecial() output
This shows the special characters that were found, versus the special characters that were actually removed.

# Examine results of the handleSpecial() function
result$special_found_replaced
#>                          special_found special_replaced
#> Lat                                                    
#> Long                                                   
#> Region                                                 
#> UniqueFireIdentifier                                   
#> ContainmentDateTime                  :                :
#> ControlDateTime                      :                :
#> IncidentSize                                           
#> DispatchCenterID                                       
#> FireCause                                              
#> FireCauseGeneral                     /                /
#> FireCauseSpecific                    /                /
#> FireDepartmentID                                       
#> FireDiscoveryDateTime                :                :
#> FireOutDateTime                      :                :
#> IncidentName                       / #              / #
#> IncidentShortDescription             /                /
#> IncidentTypeCategory                                   
#> InitialResponseAcres                                   
#> InitialResponseDateTime              :                :
#> IsMultiJurisdictional                                  
#> IsReimbursable                                         
#> LocalIncidentIdentifier                                
#> POOCounty                                              
#> POODispatchCenterID                                    
#> POOFips                                                
#> POOJurisdictionalUnit                                  
#> POOLandownerCategory                                   
#> POOLandownerKind                                       
#> POOProtectingUnit                                      
#> POOState
detectTypes() output
This output is a record of what the detectTypes() function changed. The first column indicates the % of unique values contained within the named column of the original dataframe. Next, we see the original column class, and the resulting column class.

# Examine the results of the detectTypes() function
result$type_stats
#>                          percent_unique original_class new_class
#> Lat                          97.2712681      character   numeric
#> Long                         94.5425361      character   numeric
#> Region                        0.3210273      character    factor
#> UniqueFireIdentifier         99.8394864      character character
#> ContainmentDateTime          99.5215311      character character
#> ControlDateTime              99.1957105      character character
#> IncidentSize                 19.3605684      character   numeric
#> DispatchCenterID              0.8025682      character    factor
#> FireCause                     0.8025682      character    factor
#> FireCauseGeneral              1.9261637      character    factor
#> FireCauseSpecific             2.5682183      character    factor
#> FireDepartmentID             37.5000000      character   numeric
#> FireDiscoveryDateTime        99.3579454      character character
#> FireOutDateTime              98.4018265      character character
#> IncidentName                 91.9743178      character character
#> IncidentShortDescription      1.7656501      character    factor
#> IncidentTypeCategory          0.3210273      character    factor
#> InitialResponseAcres         14.6572104      character   numeric
#> InitialResponseDateTime     100.0000000      character character
#> IsMultiJurisdictional         0.3311258      character    factor
#> IsReimbursable                0.3311258      character    factor
#> LocalIncidentIdentifier      88.1219904      character   numeric
#> POOCounty                     2.4077047      character    factor
#> POODispatchCenterID           0.8025682      character    factor
#> POOFips                       2.4077047      character    factor
#> POOJurisdictionalUnit         2.7287319      character    factor
#> POOLandownerCategory          1.6051364      character    factor
#> POOLandownerKind              0.6420546      character    factor
#> POOProtectingUnit             3.5313002      character    factor
#> POOState                      0.3210273      character    factor
handleMissing() output
Lastly, the handleMissing output includes information about which columns are dropped, and at what point in the process. ‘missing_stats’ includes the percent of missing values for each column throughout each step in the process. It is important to note that this output may change, depending upon the options the user inputs and/or selects during interactive sections of the function.

The final list of dropped columns and dropped rows are also provided, so the user knows which rows and columns are no longer contained in the final dataframe.

# Examine the results of the handleMissing() function
result$missing_stats
#>                    variable initial_percent_missing after_col_drop_percent
#> 1       ContainmentDateTime                    32.9                   32.9
#> 2           ControlDateTime                    40.1                   40.1
#> 3          DispatchCenterID                     7.9                    7.9
#> 4                 FireCause                     3.9                    3.9
#> 5          FireCauseGeneral                    83.6                dropped
#> 6         FireCauseSpecific                    94.1                dropped
#> 7          FireDepartmentID                    94.9                dropped
#> 8     FireDiscoveryDateTime                     0.0                      0
#> 9           FireOutDateTime                    29.7                   29.7
#> 10             IncidentName                     0.0                      0
#> 11 IncidentShortDescription                    98.4                dropped
#> 12             IncidentSize                     9.6                    9.6
#> 13     IncidentTypeCategory                     0.0                      0
#> 14     InitialResponseAcres                    32.1                   32.1
#> 15  InitialResponseDateTime                    89.4                dropped
#> 16    IsMultiJurisdictional                     3.0                      3
#> 17           IsReimbursable                     3.0                      3
#> 18                      Lat                     0.0                      0
#> 19  LocalIncidentIdentifier                     0.0                      0
#> 20                     Long                     0.0                      0
#> 21                POOCounty                     0.0                      0
#> 22      POODispatchCenterID                    33.7                   33.7
#> 23                  POOFips                     0.0                      0
#> 24    POOJurisdictionalUnit                    37.2                   37.2
#> 25     POOLandownerCategory                    38.4                   38.4
#> 26         POOLandownerKind                    38.4                   38.4
#> 27        POOProtectingUnit                     0.0                      0
#> 28                 POOState                     0.0                      0
#> 29                   Region                     0.0                      0
#> 30     UniqueFireIdentifier                     0.0                      0
#>           impute method end_percent_missing
#> 1      None_Not Numeric                32.9
#> 2      None_Not Numeric                40.1
#> 3  Most Frequent Factor                   0
#> 4  Most Frequent Factor                   0
#> 5               dropped             dropped
#> 6               dropped             dropped
#> 7               dropped             dropped
#> 8      None_Not Numeric                   0
#> 9      None_Not Numeric                29.7
#> 10     None_Not Numeric                   0
#> 11              dropped             dropped
#> 12               Median                   0
#> 13 Most Frequent Factor                   0
#> 14               Median                   0
#> 15              dropped             dropped
#> 16 Most Frequent Factor                   0
#> 17 Most Frequent Factor                   0
#> 18    No Missing Values                   0
#> 19    No Missing Values                   0
#> 20    No Missing Values                   0
#> 21 Most Frequent Factor                   0
#> 22 Most Frequent Factor                   0
#> 23 Most Frequent Factor                   0
#> 24 Most Frequent Factor                   0
#> 25 Most Frequent Factor                   0
#> 26 Most Frequent Factor                   0
#> 27    No Missing Values                   0
#> 28    No Missing Values                   0
#> 29 Most Frequent Factor                   0
#> 30     None_Not Numeric                   0
result$dropped_cols
#> [1] "FireCauseGeneral"         "FireCauseSpecific"       
#> [3] "FireDepartmentID"         "IncidentShortDescription"
#> [5] "InitialResponseDateTime"
result$dropped_rows
#> NULL
