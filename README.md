# Analyzing Spatial Autocorrelation in R (GEOG 418 - Lab 3)
## Introduction

Spatial autocorrelation is a helpful tool to assist in any spatial analysis you may seek to do. It works to assess whether a point pattern is clustered, random, or dispersed by investigating a set of features with a specific attribute (ESRI, n.d.). Spatial autocorrelation is related to Tobler’s Law of geography which states that “Everything is related to everything else, but near things are more related than distant things” (Tobler, 1970). The pattern that Tobler mentions would mean it is positively autocorrelated and exhibits a clustered pattern. This tends to be the most common type of point pattern seen in nature. In contrast, the opposite of Tobler’s law would mean that near things are less related than distant things. This pattern would mean it is negatively autocorrelated and exhibits a dispersed pattern. It is not common to see a pattern that is significantly dispersed in nature. Patterns that are not clustered or dispersed are therefore random and exhibit no spatial autocorrelation. 

The methods for determining spatial autocorrelation are the Local and Global Moran's I calculations which will be further explained later in this tutorial. The concept of spatial autocorrelation can be greatly useful when attempting to assess where and why phenomena are happening in space to reveal what variables may be driving it. For example, law enforcement in a given city may seek to determine whether crime is clustered in a specific area in order to better understand socio-economic distributions and implement new policies.

The purpose of this tutorial is to guide readers in performing their own spatial autocorrelation analyses using R language. Readers should have a basic understanding of R code and R Markdown files. The census data used in this tutorial was retrieved from UCGS (.csv file) and Statistics Canada (.shp file). These data are appropriate for this type of analysis because the variables of “Median total income” and “Percentage of respondents with French language knowledge” in the comma separated values file can be used in tandem with the shapefile of Canadian dissemination areas to examine their autocorrelation in a geographical context. 

The first step in doing any analysis in R is to install packages and open the libraries that are necessary to perform the tasks you wish to do. For example, the “tinytex” package allows analysts to produce .pdf files of their R Markdown projects. Without this package installed and its library opened, this task cannot be performed. Packages only need to be installed once on your system and can be commented out of your final code to keep everything clean. However, libraries must be run every time you open R.

```{r Libraries, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}
#Install packages if not already installed:
install.packages("raster")
install.packages("spatstat")
install.packages("tmap")
install.packages("sf")
install.packages("st")
install.packages("e1071")
install.packages("spdep")
install.packages("tinytex")
install.packages("evaluate")
install.packages("rmarkdown")
install.packages("knitr")

#Load in libraries:
library("raster")
library("spatstat")
library("tmap")
library("sf")
library("st")
library("e1071")
library("spdep")
library("tinytex")
library("evaluate")
library("rmarkdown")
library("knitr")
```

You will now want to set the working directory and read in the files you will be using. Setting the working directory tells R where to look for the files you will be using. It is good practice to check that this is done correctly by quickly running the code that is commented out below. Reading in the files works by creating new data frames in your R project that will show up in the “environment” pane. The two files that we will be reading in are the comma separated value (csv) file which contains our income and french knowledge data, and the shapefile (shp) which contains our dissemination areas data. If you are using a dataset that was collected in a coordinate reference system (crs) that does not align with the geographic region that you are examining, you will need to transform the crs of the shapefile.

```{r Read in data, echo=TRUE, eval=TRUE, warning=FALSE}

#Setting working directory
dir <- "C:/Users/blorr/OneDrive/Documents/GEOG418/Hicks_GEOG418_Assignment3"

setwd(dir)

#If you would like to check if your working directory was set correctly, run the following line:
#getwd()

#From the working dir read in the csv
csv <- read.csv("ucgsJQnBVLvP_data.csv") 

#Data source is the working dir (where the layer is), layer is the name of the file (without .shp)
shp <- st_read("lda_000a16a_e.shp") 


#Transform crs according to location, crs for New Brunswick is used in this example
shp_t <- st_transform(shp, crs = 2953)

#If you would like to check if your crs was set correctly, run the following line:
#st_crs(shp_t)
```
Using these data for our analysis without cleaning it can be confusing and ultimately slow, which is why this next step works to extract only the data we are interested in. Firstly, the column names are converted to vectors and then the rows we do not want are removed to maintain simplicity. The csv and shp datasets are then merged and sub-setted to our area of interest (Fredericton in this example). You will want to be sure to transform your absolute count variable into a rate (percentage) for ease of analysis.

```{r Clean data, echo=TRUE, eval=TRUE, warning=FALSE}
#New column names
cols <- c("GEO UID", "Province code", "Province name", "CD code",
        "CD name", "DA name", "Population", "Land area", 
        "Median total income", "Income Sample Size", "French Knowledge", 
        "Language Sample Size")

#Apply those names to dataframe
colnames(csv) <- cols

#Add column to count number of ID charactors
csv$len <- nchar(csv$`GEO UID`)

#Remove IDs with less than 8 numbers
csv_clean <- subset(csv, csv$len == 8)

#Merge spatial and aspatial data
census_DAs <- merge(shp_t, csv_clean, 
                    by.x = "DAUID", 
                    by.y = "GEO UID", 
                    all.x = TRUE)

#Subset for Fredericton
Municp <- subset(census_DAs, census_DAs$CMANAME == "Fredericton")

#Convert to rate
Municp$PercFrench <- (Municp$`French Knowledge`/Municp$`Language Sample Size`)*100
```
Our last step before beginning analysis is to remove any N/A or 0 values from both our French knowledge and income variables. This is done to ensure consistency in our results.

```{r NA Remove, echo=TRUE, eval=TRUE, warning=FALSE}
#Remove Income NA
Income_noNA <- Municp[which(!is.na(Municp$`Median total income`)),]

#Remove French NA
French_noNA <- Municp[which(!is.na(Municp$`PercFrench`)),]
```

Next, we will take a closer look at the two variables we are interested in: Median total income and Percentage of respondents with French language knowledge. We will look at some descriptive stats and do a final check for NA values in the data.
```{r NA Remove, echo=TRUE, eval=TRUE, warning=FALSE}
#Remove Income NA
Income_noNA <- Municp[which(!is.na(Municp$`Median total income`)),]

#Remove French NA
French_noNA <- Municp[which(!is.na(Municp$`PercFrench`)),]
```
Next, we will take a closer look at the two variables we are interested in: Median total income and Percentage of respondents with French language knowledge. We will look at some descriptive stats and do a final check for NA values in the data.

```{r DescriptiveStats, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate descriptive stats for Income
meanIncome <- mean(Income_noNA$`Median total income`)
stdevIncome <- sd(Income_noNA$`Median total income`)
skewIncome <- skewness(Income_noNA$`Median total income`)

#Calculate descriptive stats for French
meanFrench <- mean(French_noNA$`PercFrench`)
stdevFrench <- sd(French_noNA$`PercFrench`)
skewFrench <- skewness(French_noNA$`PercFrench`)

#Create dataframe for display in table
data <- data.frame(Variable = c("Income", "French Language"),
                   Mean = c(round(meanIncome,2), round(meanFrench,2)),
                   StandardDeviation = c(round(stdevIncome,2), round(stdevFrench,2)),
                   Skewness = c(round(skewIncome,2), round(skewFrench,2)))

#Produce table
kable(data, caption = paste0("Descriptive statistics for Income and French Knowledge ", 2016, " Fredericton, NB"))
```
![image](https://github.com/user-attachments/assets/c2b10386-d5d6-4716-9b9e-ed0569870784)

Describe how the map is created.

```{r StudyArea, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Fredericton census dissemination areas showing median total income (left) and percentage of respondants with knowledge of french (right)."}
#Choose a pallete
# tmaptools::palette_explorer() #Tool for selecting pallettes

#Map median Income
map_Income <- tm_shape(Income_noNA) + 
  tm_polygons(col = "Median total income", 
              title = "Median Total Income", 
              style = "jenks", 
              palette = "BuGn", n = 6,
              border.alpha = 0,
              colorNA = "grey") +
  tm_layout(legend.position = c("LEFT", "TOP"))


#Map French Knowledge
map_French <- tm_shape(French_noNA) + 
  tm_polygons(col = "PercFrench", 
              title = "Percentage with \n French Knowledge", 
              style = "jenks", 
              palette = "BuGn", n = 6,
              border.alpha = 0,
              colorNA = "grey") +
  tm_layout(legend.position = c("LEFT", "TOP"))

#Print maps side by side
tmap_arrange(map_Income, map_French, ncol = 2, nrow = 1)
```
![image](https://github.com/user-attachments/assets/40b4942c-cb78-4051-95b1-ffc5a4560887)

## Neighbourhood matrix

Describe the concept of a weighted neighbourhood matrix.

The code to create a list of neighbours in R is very simple thanks to the poly2nb() function in the ‘spdep’ package. If we want to change from default queen weighting to rook weighting in our selection, we simply change the ‘queen = TRUE’ to ‘queen = FALSE’.

```{r Neighbours, echo=TRUE, eval=TRUE, warning=FALSE}

#Income Neighbours - Queens weight
Income.nb <- poly2nb(Income_noNA)
# Use st_coordinates to get the coordinates
Income.net <- nb2lines(Income.nb, coords=st_coordinates(st_centroid(Income_noNA)))
crs(Income.net) <- crs(Income_noNA)                       

#Income Neighbours - Rooks weight
Income.nb2 <- poly2nb(Income_noNA, queen = FALSE)
Income.net2 <- nb2lines(Income.nb2, coords=st_coordinates(st_centroid(Income_noNA)))
crs(Income.net2) <- crs(Income_noNA)

#French Neighbours - Queens weight
French.nb <- poly2nb(French_noNA)
French.net <- nb2lines(French.nb, coords=st_coordinates(st_centroid(French_noNA)))
crs(French.net) <- crs(French_noNA)

#French Neighbours - Rooks weight
French.nb2 <- poly2nb(French_noNA, queen = FALSE)
French.net2 <- nb2lines(French.nb2, coords=st_coordinates(st_centroid(French_noNA)))
crs(French.net2) <- crs(French_noNA)
```

Explain how the maps below are created and what they show.

```{r Neighboursmap, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Fredericton census dissemination areas showing median total income neighbours queens weight (left)  rooks weight (middle) and the combination of the two (right)."}

#Make queens map
IncomeQueen <- tm_shape(Income_noNA) +
  tm_borders(col = 'lightgrey') +
  tm_shape(Income.net) +
  tm_lines(col = 'blue') +
  tm_layout(
    main.title = "Income Distribution using Queen Neighbourhood",
    main.title.size = 0.5,  # Adjust the size of the title
  )

#Make rooks map
IncomeRook <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
              tm_shape(Income.net2) + tm_lines(col='blue', lwd = 2)

IncomeRook <- tm_shape(Income_noNA) +
  tm_borders(col = 'lightgrey') +
  tm_shape(Income.net2) +
  tm_lines(col='red', lwd = 1) +
  tm_layout(
    main.title = "Income Distribution using Rook Neighbourhood",
    main.title.size = 0.5,  # Adjust the size of the title
  )

#Make combined map
IncomeBoth <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
               tm_shape(Income.net) + tm_lines(col='blue', lwd = 2) +
               tm_shape(Income.net2) + tm_lines(col='red', lwd = 1) +
tm_layout(
    main.title = "Combined Queen and Rook Income Distribution",
    main.title.size = 0.5,  # Adjust the size of the title
  )

#Print maps in a three pane figure
tmap_arrange(IncomeQueen, IncomeRook, IncomeBoth, ncol = 3, nrow = 1)
```

![image](https://github.com/user-attachments/assets/ab246416-840b-4849-82cd-7c58534f6f55)

Describe the code for the weighted matrix file.

Weights are defined by “style” (ie. type), and can include “B”, “W”, and “C”. The B weights matrix is the most basic of the three, as it employs a binary weighting scheme, whereby each neighbour is given a weight of 1, and all other polygons are given a weight of 0 (see figures above). A W weights matrix employs a row standardized weighting scheme, with each neighbour given equal weights that sum to 1 [11]. Comparatively, a C weights matrix is a globally standardized method of weighting, with all neighbours given equal weight across the entire study area [13].

Creating a weights matrix in R uses the “nb2listw” function from the “spdep” library. We can apply this function to the vri.nb variable created above, as it contains all of the neighbour links to which we want to assign weights. Additionally, if there are any polygons in our file with zero neighbour links, we still want the program to run. Therefore, we define “zero.policy” as equal to “TRUE”, which assigns weights vectors of zero length for regions with no neighbours [13]. Subsequently, we can print off our list of weights matrices (“print.listw”) in order to assess the distribution of weights for each observation (i) and its neighbours (j). The example of code below is using a weights matrix of type W. You can read more about the different styles of spatial weighting [here](https://r-spatial.github.io/spdep/reference/nb2listw.html).

```{r Final weights, echo=TRUE, eval=TRUE, warning=FALSE}
#Create Income weights matrix
Income.lw <- nb2listw(Income.nb, zero.policy = TRUE, style = "W")

#Create French weights matrix
French.lw <- nb2listw(French.nb, zero.policy = TRUE, style = "W")

head(Income.lw[["weights"]])[c(1:3)]
```

![image](https://github.com/user-attachments/assets/17f064e3-6b22-43dd-a890-cd385ddd1496)

## Global Moran’s I

Now that we have determined how to choose and weight our neighbours, we can calculate the Global Moran’s I statistic. This method of testing for spatial autocorrelation looks across the entire study area for every location simultaneously [14]. The equation for this statistic is

$$
I = \frac{\sum_{i=1}^n\sum_{j=1}^nW_{i,j}(x_i - \bar{x})(x_j - \bar{x})}{(\sum_{i=1}^n\sum_{j=1}^nW_{i,j})\sum_{i=1}^n(x_i - \bar{x})^2}
$$

Here, if $x$ is the variable being assessed, $x_i$ is the variable value at a point of interest (i) and $x_j$ represents a neighbour to $x_i$ (here determined by the queen weighting scheme). The spatial weighting applied to the weighting matrix $W_{i,j}$ is multiplied by both the differences of $x_i$ and the mean value of variable $x$, and $x_j$ and the mean value of variable $x$.

The denominator in this case is used to standardize our values, and therefore relatively high values of I correspond with positive spatial autocorrelation, and relatively low values of I correspond with negative spatial autocorrelation. Remember that the global Moran’s I statistic provides an indication of how spatially autocorrelated our data is over the entire dataset, thus representing a spatial pattern at the global scale [15].

```{r Global Morans I, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate Global Moran's I for Income
miIncome <- moran.test(Income_noNA$`Median total income`, Income.lw, zero.policy = TRUE)

#Extract Global Moran's I results for Income
mIIncome <- miIncome$estimate[[1]]
eIIncome <- miIncome$estimate[[2]]
varIncome <- miIncome$estimate[[3]]

#Calculate Global Moran's I for French
miFrench <- moran.test(French_noNA$PercFrench, French.lw, zero.policy = TRUE)

#Extract Global Moran's I results for French
mIFrench <- miFrench$estimate[[1]]
eIFrench <- miFrench$estimate[[2]]
varFrench <- miFrench$estimate[[3]]
```

Describe the results

```{r Global Morans Range, echo=TRUE, eval=TRUE, warning=FALSE}
#Function to calculate the range of global Moran's I
moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}

#Calculate the range for the Income variable
range <- moran.range(Income.lw)
minRange <- range[1]
maxRange <- range[2]
```

Describe what the results indicate

However, we can still go a step further and figure out whether these patterns are statistically significant. To do so, we can use a Z-test. Here our null hypothesis is ?, and the alternate hypothesis is ?. Using an $\alpha$ value of 0.05, if our Z-score falls above or below 1.96, we can say ?. A value greater than +1.96 would imply ?, and a value less than -1.96 would imply ?.

We can calculate a Z-test using the following code:

```{r Global Morans ZScore, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate z-test for Income
zIncome <- (mIIncome - eIIncome) / (sqrt(varIncome))

#Calculate z-test for French
zFrench <- (mIFrench - eIFrench) / (sqrt(varFrench))
```

The zscores for both variable confirm that ?

## Local spatial autocorrelation

Explain local spatial autocorrelation

The calculation for Local Moran’s I has many of the same features as our global calculation, although arranged in a different way.

$$
I_i = \frac{x_i - \bar{x}}{S_i^2}\sum{_{j=1}^n}W_{i,j}(x_j - \bar{x})\space \space where \space \space S_i^2 = \frac{\sum_{i=1}^n (x_i - \bar{x})^2}{n-1} 
$$

Again, instead of typing out these calculations, we can use the localmoran() function to deal with all of the messy calculations for us, as long as we input our variable and weighting scheme.

```{r Local Morans I, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate LISA test for Income
lisa.testIncome <- localmoran(Income_noNA$`Median total income`, Income.lw)

#Extract LISA test results for Income
Income_noNA$Ii <- lisa.testIncome[,1]
Income_noNA$E.Ii<- lisa.testIncome[,2]
Income_noNA$Var.Ii<- lisa.testIncome[,3]
Income_noNA$Z.Ii<- lisa.testIncome[,4]
Income_noNA$P<- lisa.testIncome[,5]

#Calculate LISA test for Income
lisa.testFrench <- localmoran(French_noNA$PercFrench, French.lw)

#Extract LISA test results for Income
French_noNA$Ii <- lisa.testFrench [,1]
French_noNA$E.Ii<- lisa.testFrench [,2]
French_noNA$Var.Ii<- lisa.testFrench [,3]
French_noNA$Z.Ii<- lisa.testFrench [,4]
French_noNA$P<- lisa.testFrench [,5]
```

Now going back to our basic mapping template we can visualize some of these results to understand what this test is doing.

```{r MappingLocalMoransI, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Kamloops census dissemination areas showing LISA z-scores for median total income (left) and percentage of respondants with knowledge of french (right)."}
#Map LISA z-scores for Income
map_LISA_Income <- tm_shape(Income_noNA) +
  tm_polygons(col = "Z.Ii",
              title = "Local Moran's I Z-Scores",
              style = "fixed",
              border.alpha = 0.1,
              midpoint = NA,
              colorNA = NULL,
              breaks = c(min(Income_noNA$Z.Ii),-1.96,1.96,max(Income_noNA$Z.Ii)),
              palette = "-RdBu", n = 3)+
  tm_compass(position=c("left", "top"))+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_layout(main.title = "LISA z-scores for Income", main.title.size = 0.6,) +
  tm_legend(position = c("right", "top"))


#Map LISA z-scores for French
map_LISA_French <- tm_shape(French_noNA) +
  tm_polygons(col = "Z.Ii",
              title = "Local Moran's I Z-Scores",
              style = "fixed",
              border.alpha = 0.1,
              midpoint = NA,
              colorNA = NULL,
              breaks = c(min(French_noNA$Z.Ii),-1.96,1.96,max(French_noNA$Z.Ii)),
              palette = "-RdBu", n = 3)+
  tm_compass(position=c("left", "top"))+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_layout(main.title = "LISA z-scores for French", main.title.size = 0.6,) +
  tm_legend(position = c("right", "top"))


#Plot maps in a 2 pane figure
tmap_arrange(map_LISA_Income, map_LISA_French, ncol = 2, nrow = 1)
```

![image](https://github.com/user-attachments/assets/97e3b71b-3e00-4810-bcc2-f52e38283d81)

Explain the results.

While these maps are great for visualizing where the data is and getting a rough idea of how many polygons are significantly positively or negatively spatially autocorrelated, it can be even more informative to graph these trends.

```{r MoransIScatter, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap= "Moran's I scatter plot for median total income."}
#Create Moran's I scatter plot for Income
moran.plot(Income_noNA$`Median total income`, Income.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Median Total Income ($)", 
           ylab="Spatially Lagged Median Total Income ($)", quiet=NULL)
```

![image](https://github.com/user-attachments/assets/90774b92-0058-45ce-aee8-9c522e140d17)

```{r MoransIScatter2, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap= "Moran's I scatter plot for percentage of respondants with knowledge of french."}
#Create Moran's I scatter plot for French
moran.plot(French_noNA$PercFrench, French.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Respondants with knowledge of French (%)", 
           ylab="Spatially Lagged knowledge of French (%)", quiet=NULL)
```

![image](https://github.com/user-attachments/assets/d52ecade-d3ef-490a-8e6e-5bbf994d14b7)

In these plots, the points with diamonds are considered statistically significant, and the regression line shows the overall trend. For both plots we can see that the trend shows?

## Summary

Provide a brief summary.

## References

ESRI. (n.d.). How spatial autocorrelation (Global Moran’s I) works. ArcGIS Pro. https://pro.arcgis.com/en/pro-app/latest/tool-reference/spatial-statistics/h-how-spatial-autocorrelation-moran-s-i-spatial-st.htm

Tobler, W. R. (1970). A computer movie simulating urban growth in the Detroit Region. Economic Geography, 46, 234. https://doi.org/10.2307/143141 
