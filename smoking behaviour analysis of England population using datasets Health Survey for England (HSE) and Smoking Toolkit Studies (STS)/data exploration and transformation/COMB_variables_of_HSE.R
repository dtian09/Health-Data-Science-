#COM-B variables of HSE data

comb<-c(
  #GIVEUPRS01 #2013 - 2015
  #GIVEUPRS02
  #GIVEUPRS03
  #"WhyGvUp1", #2016 - 2018
  #WhyGUp1_19 #2019
  "Age",        #2011 - 13
  #"ag16g10",    #2014 - 19
  "Sex",        #2011 - 19
  #"LongIll",    #2011
  "ILL12m",     #2012 - 19
  "GVUPRS01",   #2011 - 12
  "GVUPRS02",
  "GVUPRS03",
  "GVUPRS04",
  "GVUPRS05",
  "GVUPRS06",
  "GVUPRS07",
  "GVUPRS08",
  "GVUPRS09",
  "WHENSTOP",   #2012
  #WHNSTPSK   #2013 - 15
  #GIVEUPRS01
  #GIVEUPRS02
  #GIVEUPRS03
  #GIVEUPRS04
  #GIVEUPRS05
  #GIVEUPRS06
  #GIVEUPRS07
  #GIVEUPRS08
  #GIVEUPRS09
  #"whnstpsk",   #2016 - 18
  #"WhyGvUp1",
  #"WhyGvUp2",
  #"WhyGvUp3",
  #"WhyGvUp4",
  #"WhyGvUp5",
  #whnstp_19  #2019
  #WhyGUp1_19
  #WhyGUp2_19
  #WhyGUp3_19
  #WhyGUp4_19
  #WhyGUp5_19
  "cigst2",    #2011 - 18
  "FirstCig",
  "cigdyal",
  "cigwday",
  "cigwend",
  #CIGST2_19  #2019
  #FIRSTCIG
  #CIGDYAL_19
  #CIGWDAY_19
  #CIGWEND_19
  "PASSMK1",   #2011 - 12
  "PASSMK2",
  "PASSMK3",
  "PASSMK4",
  "PASSMK5",
  "PASSMK6",
  #"passmoke1",  #2013 -18
  #"passmoke2",
  #"passmoke3",
  #"passmoke4",
  #"passmoke5",
  #"passmoke6",
  #"passmoke7",
  #PASSMOKE1_19 #2019
  #PASSMOKE2_19
  #PASSMOKE3_19
  #PASSMOKE4_19
  #PASSMOKE5_19
  #PASSMOKE6_19
  #PASSMOKE7_19
  "DRSMOKE",     #2011 - 15
  #"DrSmk12",     #2016 - 18
  #DRSMK1219   #2019
  "nssec3",      #2011 - 19
  "nssec8",
  "social_grade", #created from nssec8
  "topqual3",
  "topqual2",
  "Origin",      #2011 - 14
  #"Origin2",     #2015 - 19
  "Children",    #2011 - 14
  #"NOFCh3",      #2018 - 19
  "cigst1",      #2011 - 18
  "cigevr",
  "cignow",
  #cigevr_19   #2019
  #cignow_19
  #CIGST1_19   #2019
  #cignow_19
  #"SerQt",       #2016 - 18
  #"QuitNum",
  #SerQt_19    #2019
  #"QuitNum_19",
  #"CutDwn",     #2013 - 19
  "cigtyp"      #2011 - 18
  #CIGTYP_19   #2019
  #"ASKHLP_1",    #2013 -15
  #"ASKHLP_2",
  #"ASKHLP_3",
  #"ASKHLP_4",
  #"NRNow08"  #2013 - 18
  #NRNow8_19 #2019
)

#rename variables of 2019 to same variable names of 2016 to 2018
#WhyGUp1_19 => WhyGvUp1,
#WhyGUp2_19 => WhyGvUp2,
#WhyGUp3_19 => WhyGvUp3,
#WhyGUp4_19 => WhyGvUp4,
#WhyGUp5_19 => WhyGvUp5,
#whnstp_19=>whnstpsk,
#CIGST2_19=>cigst2,
#FIRSTCIG =>FirstCig,
#CIGDYAL_19=>cigyal,
#CIGWDAY_19=>cigwday,
#CIGWEND_19 => cigwend
#PASSMOKE1_19 => "passmoke1"
#PASSMOKE2_19 => "passmoke2"
#...
#PASSMOKE7_19 => "passmoke7"
#DRSMK1219 => DrSmk12
#cigevr_19 => cigevr
#cignow_19 => cignow
#SerQt_19 => SerQt
#QuitNum_19 => QuitNum
#CIGTYP_19 => cigtyp
#NRNow8_19 => NRNow08
