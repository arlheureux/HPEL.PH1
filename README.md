
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

# HPEL.PH1

Ce package est encore en cours de développement.

The goal of HPEL.PH1 is to provide code for the MFSD Descriptor
D1-Biodiversity PH1 indicators in the French Mediterranean from pygments
or abundance data <!-- badges: end -->

<!-- badges: start -->

## Installation

You can install the development version of HPEL.PH1 from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("arlheureux/HPEL.PH1")
library(HPEL.PH1)
```

<!-- badges: end -->
<!-- badges: start -->

## Example

``` r
data("data")
head(data)
#>       Date Site Param      Val Latitude Longitude Month Year
#> 1 1980.000    a     A 25.07829       49         0     1 1980
#> 2 1980.042    a     A 18.29783       49         0     1 1980
#> 3 1980.083    a     A 24.12932       49         0     1 1980
#> 5 1980.167    a     A 17.19343       49         0     3 1980
#> 6 1980.208    a     A 27.56338       49         0     3 1980
#> 7 1980.250    a     A 20.94301       49         0     4 1980
tail(data)
#>           Date Site Param      Val Latitude Longitude Month Year
#> 47943 2019.625    e     J 21.60963     49.2      -0.2     8 2019
#> 47944 2019.667    e     J 13.62583     49.2      -0.2     9 2019
#> 47945 2019.708    e     J 19.94012     49.2      -0.2     9 2019
#> 47946 2019.750    e     J 17.18765     49.2      -0.2    10 2019
#> 47949 2019.875    e     J 17.91984     49.2      -0.2    11 2019
#> 47950 2019.917    e     J 21.89476     49.2      -0.2    12 2019
```

``` r
# Set path to save images
path <- "Your path"

# year min, year max, range of years and evaluation period duration
ymi = 1980
yma = 2020
ry = 1+diff(c(ymi, yma))
duree.eval <- 6

# In order to aggregate data when more than one per season (understand month in this case)
ts <- TS.agreg.UT(Data_TS = data, agg.func = 'mean', season = 1:12, year.min = ymi, year.max = yma)

# Find out what year for what parameters have less than nb.mois.min data
y <- id.year.enlever(Data_TS_month = ts, nb.mois.min = 4, nb.cores = 10)

# Remove these years from the data
rey <- remove.years(Data_TS_month = ts, Year.a.enlever = y, prop.annee.mini = .5, nb.cores = 10, range.year = ry)

# Interpolate with climatology in order to have continuous time series
int <- interpo(rey)

# Plot these TS
interpo.plot(int, name = "test", path = path)
```

<figure id="id">
<img src="data/TS_interpolees_test.png" class="class"
style="width:100.0%;height:50.0%"
alt="Results from interpo.plot(). Black points are data, red points are interpolated data with climatology." />
<figcaption aria-hidden="true">Results from interpo.plot(). Black points
are data, red points are interpolated data with
climatology.</figcaption>
</figure>

``` r
# Get anoamlies of the times series
ga <- get.anomalies(data = int, year.max = yma, duree.eval = duree.eval, Ref_or_TS = "Ref)

# Plot them
anomalies.plot(Anomalies.R = ga$Anomalies.R, Anomalies.E = ga$Anomalies.E,
               quantiles = ga$quantiles, year.min = ymi, year.max = yma, duree.eval = duree.eval, 
               Freq.ano = ga$Freq.ano, chi2 = ga$chi2, path = path)
```

<figure id="id">
<img src="data/Anomalies_C_a.png" class="class"
style="width:100.0%;height:50.0%"
alt="Results from anomalies.plot(). Blue bars are positive anomalies and red bars are negative anomalies. background is coloured based in the anomalies intensity. The two tables present the frequency of each class of anomalies in the data and expected if no differences between the reference and evaluation periods." />
<figcaption aria-hidden="true">Results from anomalies.plot(). Blue bars
are positive anomalies and red bars are negative anomalies. background
is coloured based in the anomalies intensity. The two tables present the
frequency of each class of anomalies in the data and expected if no
differences between the reference and evaluation periods.</figcaption>
</figure>

``` r
anomalies.plot.freq(Freq.ano.an = ga$Freq.ano.an, path = path)
```

<img src="data/Anomalies_frequence_C.png" id="id" class="class"
style="width:100.0%;height:50.0%"
alt="Frequence of each type of anomalies" />
<img src="data/Anomalies_indice_C.png" id="id" class="class"
style="width:100.0%;height:50.0%"
alt="Frequence of each type of anomalies" />

``` r
# Get seasonnal mann kendall and sen slope estimages
mks <- get.mks(Data_TS_ok = int)

# Plot them
mks.plot(Data_full = data, Data_TS = int, mks = mks, Data_full_UT = data, Data_TS_UT_ok = int, res.mks.UT = mks,
         col.param = 4, path = path, melt = F, ds = "test", groupe = "test", 
         year.max = yma)
```

<figure id="id">
<img src="data/C_a_mksm.png" class="class"
style="width:100.0%;height:50.0%"
alt="Mann-Kendall plot of data. In black are all the data availables. The orange line is the monthly mean and the green dots are the Sen line when the seasonal Mann-Kendall revealed a monotonous trend in the monthly data." />
<figcaption aria-hidden="true">Mann-Kendall plot of data. In black are
all the data availables. The orange line is the monthly mean and the
green dots are the Sen line when the seasonal Mann-Kendall revealed a
monotonous trend in the monthly data.</figcaption>
</figure>

``` r
# Calculate and plot Plankton Index
global.PI(Data_TS_ok = int, GF1 = "A", GF2 = "B", duree.eval = duree.eval, 
          year.max = yma, path = path)
```

<figure id="id">
<img src="data/PI_Anomalies.png" class="class"
style="width:100.0%;height:50.0%"
alt="PI plots based on anomalies detected over" />
<figcaption aria-hidden="true">PI plots based on anomalies detected
over</figcaption>
</figure>

<figure id="id">
<img src="data/PI_Month.png" class="class"
style="width:100.0%;height:50.0%"
alt="PI plots colored based on month of sampling" />
<figcaption aria-hidden="true">PI plots colored based on month of
sampling</figcaption>
</figure>

<img src="data/PI_Year.png" id="id" class="class"
style="width:100.0%;height:50.0%"
alt="PI plots colored based on year of sampling" /> <!-- badges: end -->
