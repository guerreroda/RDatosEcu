# RDatosEcu

To find tickets and variables see dict.xlsx

## Installation

```
install.packages("devtools")
install.packages("zoo")
install.packages("Rcurl")
install.packages("readr")
devtools::install_github("guerreroda/RDatosEcu")
```

### Example

```
library(RDatosEcu)
df <- RDatosEcu("RGDP0000 UNTL1007")
```
