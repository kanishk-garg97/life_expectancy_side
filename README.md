# life_expectancy_side

1. Exploratory Data Analysis: describe tha data and deal with missing values
(a) Analyse using descriptive statistics (both graphical and numerical representations) and R
the Life Expectancy data1.csv dataset.
(b) Many predictors in the dataset contain missing values. Is deleting predictor variables with
many missing values an appropriate method to deal with missing values? Choose a method
to deal with the missing values and then employ this method to the life expectancy data.
Justify your choice. Additionally, there are some countries (cases) where the value of Life
expectancy is missing. Explain how you will handle this problem.
2. Collinearity increases the variance of the estimators and hence, reduces the adequacy of the
model. When collinearity is present, how do you solve this problem? Investigate collinearity
between the predictor variables in the LifeExpectancyData1.csv dataset.
3. To understand better life expectancy and the factors that affect it, suggest the best linear
model which predicts life expectancy in 2020. Interpret and evaluate the suggested model.
1 of 4
4. Using the same dataset and using the additional feature Continent, employ an appropriate
experimental design to study differences of average life expectancies across the continents :
Asia, Europe, North America, South America, Africa, Australia/Oceania. Justify your choice
of experimental design and methods. [13 marks]
The dataset includes the following worldbank indicator variables:
Code Indicator Name
SP.DYN.LE00.IN Life expectancy at birth, total (years)
EG.ELC.ACCS.ZS Access to electricity (\% of population)
NY.ADJ.NNTY.KD.ZG Adjusted net national income (annual \% growth)
NY.ADJ.NNTY.PC.KD.ZG Adjusted net national income per capita (annual \% growth)
SH.HIV.INCD.14 Children (ages 0-14) newly infected with HIV
SE.PRM.UNER Children out of school, primary
SE.PRM.CUAT.ZS Educational attainment, at least completed primary,
population 25+ years, total (\%) (cumulative)
SE.TER.CUAT.BA.ZS Educational attainment, at least Bachelor’s or equivalent,
population 25+, total (\%) (cumulative)
SP.DYN.IMRT.IN Mortality rate, infant (per 1,000 live births)
SE.PRM.CMPT.ZS Primary completion rate, total (\% of relevant age group)
SE.ADT.LITR.ZS Literacy rate, adult total (\% of people ages 15 and above)
FR.INR.RINR Real interest rate (\%)
SP.POP.GROW Population growth (annual \%)
EN.POP.DNST Population density (people per sq. km of land area)
SP.POP.TOTL Population, total
SH.XPD.CHEX.PP.CD Current health expenditure per capita,
PPP (current international \$)
SH.XPD.CHEX.GD.ZS Current health expenditure (\% of GDP)
SL.UEM.TOTL.NE.ZS Unemployment, total (\% of total labor force) (national estimate)
NY.GDP.MKTP.KD.ZG GDP growth (annual \%)
NY.GDP.PCAP.PP.CD GDP per capita, PPP (current international \$)
SP.DYN.CBRT.IN Birth rate, crude (per 1,000 people)
EG.FEC.RNEW.ZS Renewable energy consumption (\% of total final energy consumption)
SH.HIV.INCD Adults (ages 15-49) newly infected with HIV
SH.H2O.SMDW.ZS People using safely managed drinking water services
(\% of population)
SI.POV.LMIC Poverty headcount ratio at \$3.20 a day (2011 PPP)
(\% of population)
SE.COM.DURS Compulsory education, duration (years)
