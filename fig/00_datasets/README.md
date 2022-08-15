# Datasets for Geissmann et al., 2022.

This entry provides the datasets for the relevant manuscript figures (Figure 4, 5, 6, S2 and S4).
Data is provided as individual CSVs. Data is often shared between different sub-figures and, sometimes, figures. 
Fields are described below.


* `fig_4ACE_meta.csv` -- describes the metadata for figure 4 A,C,E
    * id --  the unique identifier of an experimental replicate <device.start_time.end_time>
    * device -- a unique identifier of a device (one sticky pi)
    * start -- start time of the experiment
    * end -- end time of the experiment
    * condition -- experimental treatment DD or LD, see manuscript
    * ref_datetime -- The origin time of the experiment. Time is to be expressed relative to this reference. This is the start of the first experimental (subjective) day

* `fig_4AC_data.csv` -- the data for figure 4 A,C.  
  * id -- matches the metadata id (see above)
  * t -- the time, in second, using the experimental reference time as an origin (see above) 
  * N -- the number of captured insects (sub-figure A)
  * dN -- the smoothed time derivative of N, by experiment, (subfigure C)

* `fig_4E_data.csv` -- the data for figure 4 E.  
  * id -- matches the metadata id (see above)
  * period -- the period in seconds (X axis)
  * power -- Autocorrelation value, i.e. ACF, (Y axis)

* `fig_4BDF_meta.csv` --  describes the metadata for figure 4 B,D,F. Fields descriptions same as for `fig_4AC_meta.csv`.
   
* `fig_4BD_data.csv` -- the data for figure 4 B,D. Fields descriptions same as for `fig_4AC_data.csv` 

* `fig_4F_data.csv` -- the data for figure 4 E. Fields descriptions same as for `fig_4E_data.csv` 

* `fig_5_meta.csv` -- describes the metadata for figure 5
  * id -- the unique identifier of a replicate x taxon <taxon_label.device.start_time.end_time>
  * label_itc -- a numerical label from the Insect Tuboid Classifier for the taxon
  * taxon -- the name of the taxon in the manuscript/figure
  * device -- a unique identifier of a device (one sticky pi)
  * start_datetime -- start time of the experiment
  * end_datetime -- end time of the experiment
  * vinegar_bait -- "Y" or "N", whether this replicate (week x device) was baited with apple cider vinegar (see manuscript)
  * 
* `fig_5_data.csv` -- the data for figure 5
  * id -- matches the metadata id (see above)
  * wt -- Warped time, in second since 1970-01-01 (epoch). Warped time is used to account for changes in day length (see manuscript) 
  * wzt -- Warped Zeitgeber time, in seconds. Warped time since the sunrise
  * N -- The number of insect detected for this id (taxon x device x week)

  
* `fig_6A_S4_meta.csv` -- describes the metadata for figure 6A and S4 . Fields are identical to`fig_5_meta.csv` (minus `vinegar_bait`, which was only in figure 5)

* `fig_6A_S4_data.csv` -- the data for figures 6A and S4. Fields are identical to`fig_5_data.csv` 


* `fig_6B_data.csv` -- the data for figures 6A and S4. Fields are identical to`fig_5_data.csv` 
  * D1 -- the first dimension (multidimensional scaling)
  * D2 -- the second dimension
  * taxon -- the name of the taxon
  * rep -- the bootstrap replicate (see manuscript). The first replicate (rep = 1) is just the original data (not resampled)

  
* `fig_S2DE_data.csv` -- the data for figures S2 D and E. Each row is an instance (putative insect) from  given validation file
  * filename -- the validation file
  * in_gt -- whether the instance is present in the ground truth (human-annotated) image
  * in_im -- whether the same instance is present in the UID-predicted image
  * area -- the area of the instance, in pixels

