**Date of release**

* 12/22/2016

**Content of release**

* [Interactive Data Exploration, Analysis, and Reporting (IDEAR) in Jupyter Notebooks (Python 2.7)](../DataScienceUtilities/DataReport-Utils/Python2)
* Interactive Data Exploration, Analysis, and Reporting (IDEAR) in R with New and Enhanced Features

**Version of release**

* 0.11

**Where to get the release**

* [https://github.com/Azure/Azure-TDSP-Utilities/tree/master/DataScienceUtilities/DataReport-Utils](https://github.com/Azure/Azure-TDSP-Utilities )

**New features**

* IDEAR in Jupyter Notebooks (Python 2.7). Python users can get interactive data exploration, analysis, visualization, and reporting capabilities similar to IDEAR in R. 
* Automatic datetime fields featurization in IDEAR in R. This feature automatically extracts datetime components such as year, month, day, and day of week from user-specified datetime columns. The extracted datetime components are added as extra columns in the original dataset for analysis and explore. If the data source is local file, the augmented dataset is saved in the same directory of the original data file.

**Enhanced features**

* Updated pie chart visualization. In IDEAR in R, when visualizing individual categorical variables in piechart, the pie slices are ordered by the frequencies of the categorical variable levels.
* Updated the generated html report file path to make it work in R Tools for Visual Studio ([RTVS](http://microsoft.github.io/RTVS-docs/))
* Unified coding style in IDEAR in R.
* Adopted change requests in pull requests from data science community.

**Related resources**

* TDSP Project Template Repository:
[https://github.com/Azure/Azure-TDSP-ProjectTemplate](https://github.com/Azure/Azure-TDSP-ProjectTemplate)
* TDSP Instructions Repository:
[https://github.com/Azure/Microsoft-TDSP](https://github.com/Azure/Azure-TDSP-ProjectTemplate)
* TDSP AMAR Utilities 
[https://github.com/Azure/Azure-TDSP-Utilities/blob/master/DataScienceUtilities/Modeling](https://github.com/Azure/Azure-TDSP-ProjectTemplate)