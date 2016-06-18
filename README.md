gtfsr
=====

What is it
----------

`gtfsr` is an R package for easily importing, validating, and mapping transit data that follows the [General Transit Feed Specification (GTFS)](https://developers.google.com/transit/gtfs/) format. 

It provides API wrappers for the popular public GTFS feed sharing sites [TransitFeeds](https://transitfeeds.com/) which read the feed data into a `gtfs` data objects. The `gtfsr` package also provides functions for validating data quality (i.e. if the source data is properly structured and formatted as a GTFS feed) and for mapping stops and routes.

How to install it
-----------------

You can install this package from GitHub using the devtools package:

    if (!require(devtools)) {
        install.packages('devtools')
    }
    devtools::install_github('ropenscilabs/gtfsr')

If you'd like to build the accompanying vignette, then run

```
devtools::install_github('ropenscilabs/gtfsr', build_vignettes = TRUE)
```

Todo List
-------------

### Functions to run "Common Tasks"

1.  Find service IDs
2.  Find trips departing a given stop
3.  Find trips arriving at a given stop
4.  Find trips between two stops
5.  Calculate a single trip fair
6.  Calculate trips with transfers
7.  Updating trip searches

Please feel free to contribute!

