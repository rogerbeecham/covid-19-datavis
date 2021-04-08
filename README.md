Code, data and discussion to support: *On the use of ‘glyphmaps’ for
analysing the scale and temporal spread of Covid-19 reported cases*
================

*Roger Beecham* (contact <r.j.beecham@leeds.ac.uk>) <br> *Jason Dykes*
<br> *Layik Hama* <br> *Nik Lomax*

Included here is code, data and discussion to support our paper: [*On
the use of ‘glyphmaps’ for analysing the scale and temporal spread of
Covid-19 reported cases*](https://doi.org/10.3390/ijgi10040213).

In this repo is **code** for :

  - Collecting and processing Public Health England’s [daily cases
    data](https://coronavirus.data.gov.uk/) cases and geography datasets
    in [`./code/download_data.R`](./code/download_data.R)
  - Pre-computing over the cases data for charting in
    [`./code/data_staging.R`](./code/data_staging.R)
  - Helper functions for generating *ridge* and *line* charts in
    [`./code/helper_functions.R`](./code/helper_functions.R)
  - Example code for generating and parametrising full *glyphmaps* in
    [`./code/examples.R`](./code/examples.R)
  - Code for generating [our implementation](./docs/img/wp.png) of this
    [Washington Post
    graphic](https://www.washingtonpost.com/nation/2020/05/24/coronavirus-rural-america-outbreaks/?arc404=true),
    a glyphmap of Covid-19 case data by US county:
    [`./code/washington_post.R`](./code/washington_post.R)

Additionally, see:

  - This [twitter
    thread](https://threadreaderapp.com/thread/1317019462453895168.html)
    for an analysis and design exposition.
  - This web-page – <http://www.roger-beecham.com/covid-19-datavis/> –
    for an introduction to the work.

-----

  - [Roger Beecham](http://www.roger-beecham.com/), School of Geography,
    University of Leeds (github:
    [rogerbeecham](https://github.com/rogerbeecham) | twitter:
    [rjbeecham](https://twitter.com/rjbeecham))
