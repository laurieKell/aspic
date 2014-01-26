#C  Data file for surplus production example
#C
#C  Data are for South Atlantic albacore from
#C    Polacheck, T., R. Hilborn and A. E. Punt. 1993.
#C    Fitting surplus production models: comparing methods and measuring uncertainty.
#C    Can. J. Fish. Aquat. Sci. 50:2597-2607.

1967 #_styr
1989 #_endyr
1 #_nseas
 12 #_months/season
1 #_spawn_seas
1 #_Nfleet
0 #_Nsurveys
1 #_N_areas
FISHERY1
 0.5 #_surveytiming_in_season
 1 #_area_assignments_for_each_fishery_and_survey
 1 #_units of catch:  1=bio; 2=num
 0.01 #_se of log(catch) only used for init_eq_catch and for Fmethod 2 and 3; use -1 for discard only fleets
1 #_Ngenders
20 #_Nages
 0 #_init_equil_catch_for_each_fishery
23 #_N_lines_of_catch_to_read
#catch	year	seas
15.9	1967	1
25.7	1968	1
28.5	1969	1
23.7	1970	1
25	1971	1
33.3	1972	1
28.2	1973	1
19.7	1974	1
17.5	1975	1
19.3	1976	1
21.6	1977	1
23.1	1978	1
22.5	1979	1
22.5	1980	1
23.6	1981	1
29.1	1982	1
14.4	1983	1
13.2	1984	1
28.4	1985	1
34.6	1986	1
37.5	1987	1
25.9	1988	1
25.3	1989	1
#
23 #_N_cpue_and_surveyabundance_observations
#_Units:  0=numbers; 1=biomass; 2=F
#_Errtype:  -1=normal; 0=lognormal; >0=T
#_Fleet Units Errtype
1       1     0 # FISHERY1
#_year seas index obs se(log)
#year	seas	index	obs	se(log)
1967	1	1	61.89	0.3
1968	1	1	78.98	0.3
1969	1	1	55.59	0.3
1970	1	1	44.61	0.3
1971	1	1	56.89	0.3
1972	1	1	38.27	0.3
1973	1	1	33.84	0.3
1974	1	1	36.13	0.3
1975	1	1	41.95	0.3
1976	1	1	36.63	0.3
1977	1	1	36.33	0.3
1978	1	1	38.82	0.3
1979	1	1	34.32	0.3
1980	1	1	37.64	0.3
1981	1	1	34.01	0.3
1982	1	1	32.16	0.3
1983	1	1	26.88	0.3
1984	1	1	36.61	0.3
1985	1	1	30.07	0.3
1986	1	1	30.75	0.3
1987	1	1	23.36	0.3
1988	1	1	22.36	0.3
1989	1	1	21.91	0.3

0 #_N_fleets_with_discard
#_discard_units (1=same_as_catchunits(bio/num); 2=fraction; 3=numbers)
#_discard_errtype:  >0 for DF of T-dist(read CV below); 0 for normal with CV; -1 for normal with se; -2 for lognormal
#Fleet Disc_units err_type
0 #N discard obs
#_year seas index obs err
#
0 #_N_meanbodywt_obs
30 #_DF_for_meanbodywt_T-distribution_like

2 # length bin method: 1=use databins; 2=generate from binwidth,min,max below; 3=read vector
2 # binwidth for population size comp 
10 # minimum size in the population (lower edge of first bin and size at age 0.00) 
94 # maximum size in the population (lower edge of last bin) 

0 #_comp_tail_compression
1e-007 #_add_to_comp
0 #_combine males into females at or below this bin number
2 #_N_LengthBins
 20 24
0 #_N_Length_obs
#Yr Seas Flt/Svy Gender Part Nsamp datavector(female-male)

0 #_N_age_bins
0 #_N_ageerror_definitions
0 #_N_Agecomp_obs
1 #_Lbin_method: 1=poplenbins; 2=datalenbins; 3=lengths
0 #_combine males into females at or below this bin number
#Yr Seas Flt/Svy Gender Part Ageerr Lbin_lo Lbin_hi Nsamp datavector(female-male)

0 #_N_MeanSize-at-Age_obs
#Yr Seas Flt/Svy Gender Part Ageerr Ignore datavector(female-male)
#                                          samplesize(female-male)

0 #_N_environ_variables
0 #_N_environ_obs
0 # N sizefreq methods to read 

0 # no tag data 

0 # no morphcomp data 

999
