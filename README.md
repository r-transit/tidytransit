gtfsr
=====

What is it
----------

gtfsr is an R package for working with data from the General Transit
Feed Specification (GTFS).  
It provides API wrappers for popular public GTFS feed sharing sites,
reads feed data into a gtfs data object, validates data quality, and
provides convenience functions for common tasks.

How to install it
-----------------

You can install this package from GitHub using the devtools package:

    if (!require(devtools)) {
        install.packages('devtools')
    }
    devtools::install_github('ropenscilabs/ggforce')

How to use it
-------------

### Reading feed data from an API

Request an [API key from Transitfeeds.com using your github account](http://transitfeeds.com/api/keys). 

### Validate data quality

### Common Tasks

1.  Find service IDs
2.  Find trips departing a given stop
3.  Find trips arriving at a given stop
4.  Find trips between two stops
5.  Calculate a single trip fair
6.  Calculate trips with transfers
7.  Updating trip searches
