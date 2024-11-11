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

df <- RDatosEcu("RGDP0000 UNTL1007", retry=10)

df <- RDatosEcu("RGDP0000 UNTL1007", retry=10, export.path="C://user/documents/FileName.csv")
```

## Last edits:

- 2024.11.11: fixed error in max.attempts default. 
- 2024.11.01: RDatsEcu() includes tryCatch with max attempts. Default is retry=10.
- 2024.11.01: RDatosEcu() includes export.path function. Saves csv/xlsx/txt.
