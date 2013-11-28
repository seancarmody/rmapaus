rmapaus
=======

R package to simplify visualising Australian data

Here is an example which plots all suburbs falling the in Inner Sydney (10505)
Statistical Division.

```R
poa <- get_mapaus("POA", accuracy="full")
ssd <- get_mapaus("SSD", accuracy="full")
plot(poa[poa$SSD_CODE11=="10505", ], col="grey", border="white")
with(poa@data[poa$SSD_CODE11=="10505", ], text(long, lat, labels=POA_CODE, cex=0.6))
plot(ssd[ssd$SSD_CODE11=="10505", ], add=TRUE, lwd=2)
```
