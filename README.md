# profile_graphics
A lightweight package to produce ggplot-based graphics from periodic vertical 
water profile data.

## Introduction
"Profile" data is common in both limnology and oceanography.  Profile data 
is data where some sort of water quality (or more rarely, biological) parameter
is measured at either regular or irregular intervals from the water surface to
depth. Common examples are salinity, temperature and dissolved oxygen profiles,
but the general idea applies whenever a parameter is collected along
a vertical line at more or less one time.

This package aims to simplify production of a few types of graphics that are
often useful in depicting these data.  The package will be especially useful 
for producing graphics from season-long series of periodic profile observations.
Some graphics used to summarize this kind of data hide considerable complexity.

The goal is to provide a simple interface to ggplot graphics, with intelligent
defaults. We have not followed all of the  `ggplot2` calling conventions, but 
that may evolve with time. 

The package was constructed for Casco Ba yEstuary partnership  as part of the
process of drafting the 2020 State of Casco Bay report (completed in 2021).
Thus this draft version of the package should not be considered stable.  Future
changes are likely, and there is not intention yet of protecting backwards
compatibility.

