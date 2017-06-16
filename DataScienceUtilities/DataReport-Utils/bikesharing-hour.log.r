
#' ---
#' title: 'Data Quality Report'
#' author: 'Team Data Science Process by Microsoft'
#' output: 
#'  html_document:
#'    toc: yes
#' ---
#+ echo=FALSE

options(warn=-1)

# install required packages
options(repos='http://cran.rstudio.com/')
list.of.packages <- c('Hmisc', 'psych', 'corrgram', 'yaml', 'entropy', 'vcd', 'ggvis', 'shiny', 'corrplot', 'scatterplot3d', 'DescTools', 'xtable', 'shinyjs', 'RODBC','parallel','doSNOW','foreach', 'dplyr')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,'Package'])]
if(length(new.packages))
  install.packages(new.packages)

# intall knitr version 1.12 
if (!'knitr' %in% installed.packages()[,'Package']){
  knitrurl <- 'http://cran.r-project.org/src/contrib/Archive/knitr/knitr_1.12.tar.gz'
  install.packages(knitrurl, repos=NULL, type='source')
} else if ('1.12' != installed.packages()['knitr','Version']){
  remove.packages('knitr')
  knitrurl <- 'http://cran.r-project.org/src/contrib/Archive/knitr/knitr_1.12.tar.gz'
  install.packages(knitrurl, repos=NULL, type='source')
}

library(yaml)
library(RODBC)
library(foreach)

# yaml
yaml_file <- "C:/Users/kschmit1/Documents/GitHub/Azure-TDSP-Utilities/DataScienceUtilities/DataReport-Utils/para-bike-rental-hour_updated.yaml"
config <- yaml.load_file(yaml_file)

# data source
if(is.null(config$DataSource) || config$DataSource == 'local'){
  data <- read.csv(config$DataFilePath, header = config$HasHeader, sep =  config$Separator)
} else {
  dbhandle <- odbcDriverConnect(paste0('driver={ODBC Driver 11 for SQL Server};server=',config$Server,';database=',config$Database,';Uid=',config$Username,';Pwd=',config$Password))
  data <- sqlQuery(dbhandle, config$Query)
  odbcClose(dbhandle)
}

# add datetime columns
library(lubridate)

autogen_datetime_columns <- character()
if(!is.null(config$DateTimeColumns)){
  for (dt in names(config$DateTimeColumns)) {
    data[[dt]] <- as.POSIXct(data[[dt]], format = config$DateTimeColumns[[dt]])
    
    new_col_name <- paste0(dt, '_autogen_year')
    data[[new_col_name]] <- year(data[[dt]])
    if (length(unique(na.omit(data[[new_col_name]]))) == 1){
      data[[new_col_name]] <- NULL
    } else{
      autogen_datetime_columns <- c(autogen_datetime_columns, new_col_name)
    }
    

    new_col_name <- paste0(dt, '_autogen_month')
    data[[new_col_name]] <- month(data[[dt]]) 
    if (length(unique(na.omit(data[[new_col_name]]))) == 1){
      data[[new_col_name]] <- NULL
    } else{
      autogen_datetime_columns <- c(autogen_datetime_columns, new_col_name)
    }
    
    new_col_name <- paste0(dt, '_autogen_week')
    data[[new_col_name]] <- week(data[[dt]])
    if (length(unique(na.omit(data[[new_col_name]]))) == 1){
      data[[new_col_name]] <- NULL
    } else{
      autogen_datetime_columns <- c(autogen_datetime_columns, new_col_name)
    }
    
    new_col_name <- paste0(dt, '_autogen_day')
    data[[new_col_name]] <- day(data[[dt]])
    if (length(unique(na.omit(data[[new_col_name]]))) == 1){
      data[[new_col_name]] <- NULL
    } else{
      autogen_datetime_columns <- c(autogen_datetime_columns, new_col_name)
    }
    
    new_col_name <- paste0(dt, '_autogen_wday')
    data[[new_col_name]] <- wday(data[[dt]])
    if (length(unique(na.omit(data[[new_col_name]]))) == 1){
      data[[new_col_name]] <- NULL
    } else{
      autogen_datetime_columns <- c(autogen_datetime_columns, new_col_name)
    }
    
    new_col_name <- paste0(dt, '_autogen_hour')
    data[[new_col_name]] <- hour(data[[dt]])
    if (length(unique(na.omit(data[[new_col_name]]))) == 1){
      data[[new_col_name]] <- NULL
    } else{
      autogen_datetime_columns <- c(autogen_datetime_columns, new_col_name)
    }
    
    new_col_name <- paste0(dt, '_autogen_minute')
    data[[new_col_name]] <- minute(data[[dt]])
    if (length(unique(na.omit(data[[new_col_name]]))) == 1){
      data[[new_col_name]] <- NULL
    } else{
      autogen_datetime_columns <- c(autogen_datetime_columns, new_col_name) 
    }
    
    new_col_name <- paste0(dt, '_autogen_second')
    data[[new_col_name]] <- second(data[[dt]])
    if (length(unique(na.omit(data[[new_col_name]]))) == 1){
      data[[new_col_name]] <- NULL
    } else{
      autogen_datetime_columns <- c(autogen_datetime_columns, new_col_name)
    }
    config$ColumnsToExclude <- c(config$ColumnsToExclude, dt)
  }
}

# Add datetime components to conf$CategoricalColumns
CategoricalColumns <- config$CategoricalColumns
config$CategoricalColumns <- c(config$CategoricalColumns, autogen_datetime_columns)

# detect data types
isNumerical <- sapply(data, is.numeric)
isCategorical <- sapply(data,function(x)length(unique(na.omit(x)))<=nrow(data)/500||length(unique(na.omit(x)))<=5)
isNumerical <- isNumerical & !isCategorical
colNames <- colnames(data)

# override auto-detected isCategorical with the specified categorical variables in yaml
if(!is.null(config$CategoricalColumns)){
  config$CategoricalColumns <- make.names(config$CategoricalColumns, unique=TRUE)
  for(v in config$CategoricalCoumns){
    isCategorical[v] <- TRUE
    isNumerical[v] <- FALSE
  }
}
# override auto-detected isNumerical with the specified numerical variables in yaml
if(!is.null(config$NumericalColumns)){
  config$NumericalColumns <- make.names(config$NumericalColumns, unique = TRUE)
  for(v in config$NumericalColumns){
    isNumerical[v] <- TRUE
    isCategorical[v] <- FALSE
    }
}

# populate config$CategoricalColumns and config$NumericalColumns with detected and specified variables
config$CategoricalColumns <- colNames[isCategorical[colNames] == TRUE]
config$NumericalColumns <- colNames[isNumerical[colNames] == TRUE]

for(v in config$CategoricalColumns)
{
   data[,v] <- as.factor(data[,v])
} 


# exclude columns from the report
if(!is.null(config$ColumnsToExclude)){
  config$CategoricalColumns <- config$CategoricalColumns[!config$CategoricalColumns %in% config$ColumnsToExclude]
  config$NumericalColumns <- config$NumericalColumns[!config$NumericalColumns %in% config$ColumnsToExclude]
}

# replace missing values
if(!is.null(config$MissingValueReplaceWith)){
  missingValueReplacement <- config$MissingValueReplaceWith
} else {
  missingValueReplacement <- 0
}

# detect task type
if(is.null(config$Target)){
  taskType <- 'data_exploration'
} else if(isCategorical[config$Target]==FALSE){
  taskType <- 'regression'
} else {
  taskType <- 'classification'
}

#' # Task Summary
#+ echo=FALSE
#' - The metadata (location, numerical columns, target, etc.) is - *"C:/Users/kschmit1/Documents/GitHub/Azure-TDSP-Utilities/DataScienceUtilities/DataReport-Utils/para-bike-rental-hour_updated.yaml"*
#' - The data location is - *`r config$DataFilePath`*
#' - The target is - *`r config$Target`*
#' - The task type is - *`r taskType`*.
#' - The numerical variables are - *`r config$NumericalColumns`*
#' - The categorical variables are - *`r config$CategoricalColumns`*
#+ echo=FALSE



#+ echo=FALSE

if(nrow(data)>50000) {
    library(dplyr)
    set.seed(9805)
    data <- sample_n(data, min(50000, nrow(data)))
}

library(scatterplot3d)
data[is.na(data)] <- missingValueReplacement
x <- apply(data[,config$NumericalColumns],2,min)
y <- apply(data[,config$NumericalColumns],2,max)
index <- x == y
nonConstantNames <- config$NumericalColumns[!index]

x <- data[,nonConstantNames]
sigma <- cor(x)
sigma_eigen <- eigen(sigma)
sigma_values <- sigma_eigen$values
index <- sigma_values < 0 
if (sum(index) > 0)
{
  sigma_values[index] <- 0
}
sum_variance <- sum(sigma_values^2)
x <- scale(x)
loadings <- x %*% sigma_eigen$vectors
p.variance.explained <- sigma_values^2/sum_variance
p.variance.cumsum <- cumsum(p.variance.explained)*100

num_numericvars <- length(nonConstantNames)

