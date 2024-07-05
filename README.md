# Spatio-temporal modelling of the Brazilian fire spots: Codes and Datasets

Directory created to share the main files and the codes used in the construction of the data tables of the article "Bayesian spatio-temporal modeling of the Brazilian fire spots: The influence of human and meteorological variables" of the journal "Scientific Reports."

Abstract - Wildfires are among the most common natural disasters in many world regions and actively impact life quality. These events have become frequent due to climate change, other local policies, and human behavior. Fire spots are areas where the temperature is significantly higher than in the surrounding areas, and they are often used to identify wildfires. This study considers the historical data with the geographical locations of all the ``fire spots'' detected by the reference satellites covering the Brazilian territory between January 2011 and December 2022, comprising more than 2.2 million fire spots. This data was modeled with a spatio-temporal generalized linear model for areal unit data, whose inferences about its parameters are made in a Bayesian framework and use meteorological variables (precipitation, air temperature, humidity, and wind speed) and a human variable  (land-use transition and occupation) as covariates. The change in land use from green areas to farming significantly impacts the number of fire spots for all six Brazilian biomes.

## Files

### Control.csv

The data table "Control.csv" is a control file used to link the databases. It contains the name of the Brazilian municipalities (with and without accents and ç), the respective IBGE code for each municipality, the state to which each municipality belongs, and the biome present in each of the municipalities. In the case of biomes, the same municipality may have two or more different biomes in its territory, and for this reason, some municipalities appear more than once in the base.
