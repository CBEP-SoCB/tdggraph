# tdgraph
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
collected along a vertical line in a short period of time.

The goal is to provide a simple interface to ggplot graphics, with intelligent
defaults.

Some graphics used to summarize this kind of data hide considerable complexity.
This package aims to simplify production of graphics that are often useful in
depicting these data.  The package will be especially useful for producing
graphics from season-long series of periodic profile observations.

The package was constructed for Casco Bay Estuary Partnership as part of the
process of drafting the 2020 State of Casco Bay report (completed in 2021).
Thus this version of the package should not be considered stable.  Future
changes are likely, and there is no intention yet of protecting backwards
compatibility.

