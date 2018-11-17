# New reader retention metric analysis
Initial exploration and vetting of a new potential reader retention metric based on this last access date. 


The goal is to find a single metric that is sensitive to changes in user behavior and is robust against noise.

## Analysis approach
We explored the behavior of this new metric by reviewing the following:

Time series of the average next return time (within 7 days and 31 days) for a variety of countries and projects, using all the available data (from December 2016 to now), stacked by the following dimensions:

- Project
- Browser_family
- Os_family
- whether the counted (return) request was a main page view or not.

Daily histograms of return time for several (e.g. +/-3) days around a date where the average next return metric shows spikes or other anomalies.
