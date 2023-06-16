Import evt.gw -> Export a.gw
  $ ../distribution/gw/gwc ./assets/evt.gw
  pcnt 2 persons 3
  fcnt 1 families 1
  scnt 11 strings 15
  $ ../distribution/gw/gwu a > a.gw
  ............................................................############################################################ 

Compare files
  $ diff a.gw ./assets/evt.gw

To ged and back to gw
  $ ../distribution/gw/gwb2ged a > temp.ged
  $ ../distribution/gw/ged2gwb temp
  *** pass 1 (note)
  *** pass 2 (indi)
  *** pass 3 (fam)
  *** Trailer ok
  *** saving persons array
  *** saving ascends array
  *** saving unions array
  *** saving families array
  *** saving couples array
  *** saving descends array
  *** saving strings array
  *** create name index
  *** create strings of sname
  *** create strings of fname
  *** create string index
  *** create surname index
  *** create first name index
  *** ok

TODO input order of event is lost after gedcom export
TODO missing fevt: #resi and #pacs in fevt
TODO missing pevt: #crem #acco #acqu #adhe #awar #elec #exco
#occu #slgp #mser #adhe #awar #exco #mpro #mdis #lpas
#flkl #hosp #mobm
  $ ../distribution/gw/gwu a > b.gw
  ............................................................############################################################ 
  $ cat b.gw
  encoding: utf-8
  gwplus
  
  fam A a #occu Y 0 0 #crem +1990 B b 0
  fevt
  #anul
  #enga
  #marr 1990
  #marb
  #marc
  #marl
  #cutsomfevent
  #sep"
  #div"
  #nmen
  #nmar
  end fevt
  
  pevt A a
  #birt
  #bapt
  #bapl
  #barm
  #basm
  #bles
  #cens 1994
  #conf 1994
  #emig
  #endl
  #fcom
  #grad
  #immi
  #natu
  #ordn
  #prop
  #reti
  #resi
  #slgs
  #slgc
  #will
  #custompevent
  #circ 1994
  #chgn 1994
  #conl 1995
  #degr 1996
  #demm 1997
  #dist 1998
  #dotl 1999
  #educ 2000
  #deat
  #crem
  #buri
  end pevt
