The aim of TestPoissonity is to determine if a sample follows a Poisson distribution using goodness-of-fit tests.
To find the p-value, TestPoissonity usually uses a parametric boostrap method. 
 


## Instalation

```{r }
#Previous packages
install.packages("cli")
install.packages("pkgload")
install.packages("remotes")

#Install devtools from CRAN

install.packages("devtools")
library(devtools)

#Install TestPoissonity from github
install_github('MMH1997/TestPoissonity')
library(TestPoissonity)

```

