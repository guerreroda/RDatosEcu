# RDatosEcu

This package allows for downloading updated statistics for Ecuador.

Tickets and variables are described in dict.xlsx

## Installation

```
install.packages("devtools")
devtools::install_github("guerreroda/RDatosEcu")
```

### Example

```
library(RDatosEcu)
df <- RDatosEcu("RGDP0000 UNTL1007")
```
