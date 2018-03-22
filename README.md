rmapaus
=======

R package to simplify visualising Australian data

Here is an example which plots all suburbs falling the in Inner Sydney (10505)
Statistical Division. Note that postcode boundaries do not always align with
Bureau of Statistics boundaries, as is evident here: the black line shows the
border of the 10505 Statistical Division. 

```
poa <- get_mapaus("POA", accuracy="full")
ssd <- get_mapaus("SSD", accuracy="full")
plot(poa[poa$SSD_CODE11=="10505", ], col="grey", border="white")
with(poa@data[poa$SSD_CODE11=="10505", ], text(long, lat, labels=POA_CODE, cex=0.6))
plot(ssd[ssd$SSD_CODE11=="10505", ], add=TRUE)
```

![Postcode Chart](http://i.imgur.com/oj53buG.png)

I am keen for feedback on the package, so feel free to:

* submit suggestions or bug-reports: https://github.com/seancarmody/rmapaus/issues
* send a pull request of fixes or enhancements on: https://github.com/seancarmody/rmapaus/
* contact me directly on seancarmody@gmail.com

