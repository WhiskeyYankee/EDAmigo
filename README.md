# EDAmigo

## Table of Contents
- [What EDAmigo Does](#what-edamigo-does)
- [How to install EDAmigo](#how-to-install-edamigo)
- [Contents of EDAmigo](#contents-of-edamigo)
- [Example Use](#example-use)
  - [Step 1: Load Package and Import Data](#step-1-load-package-and-import-data)
  - [Step 2: Manually Set Date Fields](#step-2-manually-set-date-fields)
  - [Step 3: Clean Data](#step-3-clean-data)
  - [Step 4: Transform the Data](#step-4-transform-the-data)
  - [Step 5: Visualize the Data](#step-5-visualize-the-data)

   <br><br>
# What EDAmigo Does
Welcome to the EDAmigo package! EDAmigo is an assistant for conducting Exploratory Data Analysis (EDA). Users can fully automate the cleaning, processing, and visualization of their data, or use the interactive features to assist in better understanding their data. EDAmigo incorporates some dependencies which aid in the visualization of data. We would like to thank the creators of the DescTools package.

Andri Signorell et mult. al. (2017). DescTools: Tools for descriptive statistics. R package version 0.99.23.


 <br><br>
# How to install EDAmigo

This package is currently not in CRAN; however, you can easily download the package to your local machine using the following code
```
remotes::install_git("https://github.com/WhiskeyYankee/EDAmigo")
```

 <br><br>
 
# Contents of EDAmigo

EDAmigo contains a comprehensive vignette, two datasets, and the functions listed below. All dates/times should be properly defined as 'Date', 'POSIXct', or 'POSIXlt' class prior to using EDAmigo functions. <br> <br>

**handleSpecial()** Finds and removes special characters from a dataframe. The function defaults to user interaction.

**detectTypes()** Takes a dataframe and coerces columns to the appropriate class. 

**handleMissing()** Fills or removes missing values. 

**autoClean()** Automatically calls the three functions listed above to clean a dataframe of mixed data types and classes.

**boxCox()** Computes and recommends box cox transformations for the data.

**boxCox_Vis()** Visualizes the results from the boxCox() function.

**amigoPlot()** Visualizes the highest correlated pairs.

**EDAmigo()** Automatically cleans, processes, transforms, and visualizes user data.

 <br><br>
# Example Use


## Step 1: Load Package and Import Data

The first step in the EDA process is to import a dataset and load the EDAmigo library. The package comes with two example datasets: Fires and Finance. We will use the fires data for this example.

```{r setup}
library(EDAmigo)
```

## Step 2: Manually Set Date Fields

Dates can be stored in a wide variety of ways and automatically processing all possible date storage types is outside of the scope of the EDAmigo package. EDAmigo requires all dates and times to be properly classed as 'Date', 'POSIXlt', or 'POSIXct.' If your dataset contains dates, you will need to set manually set them to 'Date', 'POSIXlt', or 'POSIXct' prior to proceeding. Let's check the classes of the columns of the fires dataframe using the str() function:

```{r}
# Define the dataframe
df <- fires
str(df)

```
By running the code above, we can see that all of our date columns are type 'Date.' However, some columns contain special characters, are missing values, or have a type class that does not accurately represent the data. These can be handled using a variety of methods in EDAmigo.

## Step 3: Clean Data

We can either use the autoClean() function to handle special characters, improperly typed columns, and missing values at the same time or handle each separately using handleSpecial(), detectTypes(), and handleMissing() individually. Let's use the individual functions to provide more in depth detail about the abilities of each function. In the example provided, interaction is turned off to so the code can be run without input from the user. The videos included throughout this readme demonstrate how the code functions when interaction is turned on.

### Handle Special Characters with handleSpecial()
```{r }
# Store output for future use.
# Pass in:  
  # the dataframe, 
  # a special_user_level set to zero indicates no user interaction with the process

special_results <- handleSpecial(df, special_user_level = 0)

# Store the returned dataframe for future use
no_special_df <- special_results$df 

# handleSpecial() also outputs the list of all found and replaced special characters    
special_results$found_replaced

```

https://github.com/WhiskeyYankee/EDAmigo/assets/111311631/fdec724a-a4dc-4bef-9d30-5abfde4fb6c0 

### Coerce Columns to Appropriate Classes Using detectTypes()

```{r}
# Pass in: 
  # dataframe from handleSpecial() output
  # factor_tol, the % of unique values (or less) in a column to automatically coerce to a factor
  # type_user_tol, the % of unique values (or less) which the user would like to provide input on proper class
# Store output for future use
typed_results <- detectTypes(no_special_df, factor_tol = 10, type_user_tol = 10)

```

By setting factor_tol and type_user_tol to the same value, we avoid user interaction with the detectTypes() function. In the video below, we have left factor_tol = NULL. This improves the EDA process by allowing for user interaction.

https://github.com/WhiskeyYankee/EDAmigo/assets/111311631/39e5369f-48e9-45b9-b141-13a560b47bdf


The detectTypes() function returns 6 elements. The first is the coerced dataframe, followed by the following lists of column names: date_times, numbers, characters, and factors. The function also outputs the type_stats dataframe. This output is a record of what the detectTypes() function changed. The first column indicates the % of unique values contained within the named column of the original dataframe. Next, we see the original column class, and the resulting column class.

```{r}
# Store the properly typed dataframe for future use
typed_df <- typed_results$df

# Examine the results of the detectTypes() function
typed_results$type_stats
```

### Drop and Impute Using handleMissing()

```{r}
# Pass in dataframe from detectTypes() output
# Store output for future use
filled_results <- handleMissing(typed_df, drop_col_tol = 60, drop_row_tol = 80, drop_user_level = 0, impute_user_level = 0)

```

Setting both drop_user_level and impute_user_level to 0 will remove all user interaction. In our example, we have opted to remove all columns with 60% or more missing values, and all rows that exceed 80% missing values. The function will default to median imputation for numeric columns only. Users can opt to include most frequent factor imputation for factor columns using the impute_factor boolean parameter. Below is a clip that demonstrates the behavior of the function when both drop and impute user levels are set to 1.

https://github.com/WhiskeyYankee/EDAmigo/assets/111311631/826b1085-94ca-4a01-a737-f4b7100c1937

The handleMissing() output includes information about which columns are dropped, and at what point in the process. 'missing_stats' includes the percent of missing values for each column throughout each step in the process. It is important to note that this output may change, depending upon the options the user inputs and/or selects during interactive sections of the function.

The final list of dropped columns and dropped rows are also provided, so the user knows which rows and columns are no longer contained in the final dataframe.

```{r}
# Examine the results of the handleMissing() function
filled_results$missing_stats
filled_results$dropped_cols
filled_results$dropped_rows
```

## Step 4: Transform the Data

### Data Transformations Using BoxCox()

Often times it is desirable to use a power transformation to normalize data and/or reign outliers to improve downstream model performance. This can be done using box cox transformations on each numeric column and evaluating the results to see if a transformation would be helpful. The boxCox function differs from implementations in other packages in that it performs transformations on multiple columns at the same time and offers more output to help aid the EDA process. When the FILTER parameter is set to TRUE, the function only outputs results for numeric columns where using a power transformation makes the data more normal and reduces outliers. The transformation are sorted to indicate which ones do the best at achieving normality so the user can prioritize which ones to review and potentially use.

Let's use boxCox to evaluate potential transformations on the cleaned fire data. The autoClean function results in data set that has 4 numeric columns and 1 integer column. To illustrate how the boxCox function can ease the EDA process, lets evaluate the data first using FLITER = FALSE

```{r}
# Clean Data using autoClean function
fires_cleaned = autoClean(fires, special_user_level = 0, factor_tol = 10, type_user_tol = 10
                    , drop_user_level = 0, impute_user_level = 0, impute_factors = TRUE)

# Run the boxCox function
fires_boxCox_Trans = boxCox(fires_cleaned$clean_df , FILTER = FALSE)

# Evaluate the results
fires_boxCox_Trans$boxCox_Results
```

In the results above we get a warning that one or more of the variables did not converge in the specified lambda_1 range. Looking at the function output we see that the Lat variable does not have a lower bound and that the selected lambda_1 value is equal to -3. This is the variable that did not converge in the specified range. We can also see that the Anderson Darling statistic for the transformed Lat variable did not change by much. In fact, as we expand the lambda range, the Anderson Darling statistic for Latitude doesn't improve by more than a few points. This is a variable that doesn't benefit much from a power transformation.

We also observe in the function output that while the LocalIncidentIdentifier becomes more normal with a power transformation, it does so at the cost of increasing the number of number of outliers in the transformed data. Here we consider any values that fall outside the whiskers of a box plot to be outliers. This is indication that we might not want to transform this value either.

In fact, the only variables that have a converging lambda within the specified range, are more normal in shape after the transformation, and don't have an increase in the number of outliers as a result of the transformation are the IncidentSize and InitialResponseAcres variables. Lets see how the boxCox function works when FILTER is set to true:

```{r}
# Run the boxCox function
fires_boxCox_Trans = boxCox(fires_cleaned$clean_df , FILTER = TRUE)

# Evaluate the results
fires_boxCox_Trans$boxCox_Results
```

This time, we see that only the IncidentSize and InitialResponseAcres variables are kept in the output. The warning about convergence is only with regards to the Lat variable and does let us know that we may consider increasing the range and we could do that be looking at the unfiltered data to get an idea of what isn't converging. Our trimmed down results make it easier to determine which transformations we might consider.

## Step 5: Visualize the Data


### Visualizing Results Using boxCox_Vis() and amigoPlot()

Users can visualize the box cox results using the boxCox_Vis() function. Each recommended transformation will be displayed with key statistics and before-after plots of the data. By default, the function requires input from the user to move on to the next plot. This is so the user can easily consume the charts one by one. Users can also disable the interactive functionality and the output will be a list containing each of the plots created by the function.

```{r}
boxCox_Vis(fires_cleaned$clean_df)

```
https://github.com/WhiskeyYankee/EDAmigo/assets/111311631/6e8c0068-e684-4334-92bc-1b509d7b5c02

<br>
Finally, users can visualize the correlations between their numeric variables using the amigoPlot() function. This function outputs both  plots and a dataframe containing the correlations, slopes, intercepts, and r^2 for each of the n_top pairs.
<br>

```{r}
# Plot the top 2 correlation pairs
amigoPlot(fires_cleaned$clean_df, n_top = 2)
```
<br>

![amigoPlot Output](https://github.com/WhiskeyYankee/EDAmigo/assets/111311631/9ef11dd2-7265-4f35-833e-e5e12b23ab6c)


