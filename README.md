# tdggraph
A lightweight package to produce ggplot-based graphics from time by depth data
as produced in limnology and oceanography.

## Introduction
"Profile" data is common in both limnology and oceanography.  Profile data 
is data where some sort of water quality (or more rarely, biological) parameter
is measured from the water surface to depth. Often, profile data is collected
at regular intervals at the same location, producing a three dimensional 
(time by depth by value) data set.

Common examples of time by depth data include salinity, temperature and
dissolved oxygen profiles, but the general idea applies whenever a parameter is
collected periodically along a vertical line.

The goal of this package is to provide a simple interface to ggplot graphics,
with intelligent defaults for this type of data.  In particular, profile data is
usually  drawn with a reversed vertical (depth) axis, so that the surface (zero
depth) is at the top of the figure, with depth increasing towards the 
bottom. Similarly, it is common to show zero depth, even though data starts at
some small depth below the water surface.

This package aims to simplify production of graphics useful in
depicting these data.  The package is especially useful for producing
graphics from season-long series of periodic profile observations.
The three main graphics functions create `ggplot2` graphic objects with 
different strengths for emphasizing different aspects of the data. For most
users, the `tdsmooth()` function will be the primary one of interest.

## The Names
The name of the package reflects that it is intended to work with '**td**' (time
by depth) data, using '**gg**' plot **graph**ics.

The function names `ptlines()`, `ptdots()`, and `ptsmooth()` reflect that they
make graphics from '**pt**' (profile by time) data.

The purpose of `interpol()` should be obvious from its name, and the association 
with an international policing organization will make the name easier to 
remember.

# The Future
The package was constructed for Casco Bay Estuary Partnership as part of the
process of drafting the 2020 State of Casco Bay report (completed in 2021).
Thus this version of the package should not be considered stable.  Future
changes are likely, and there is no intention yet of protecting backwards
compatibility.

# TO DO
* Develop testing suite.

*  Redraft `interpol()` and `ptsmooth()` to allow selection of other smoothers.

*  Evaluate alternative selection of data from the Maine DEP sonde data.  
