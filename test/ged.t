Import abc.ged -> Export a.ged
  $ ../distribution/gw/ged2gwb ./assets/abc.ged
  *** pass 1 (note)
  *** pass 2 (indi)
  *** pass 3 (fam)
  *** Trailer ok
  Warning - wife with male sex: a.0 A
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
  $ ../distribution/gw/gwb2ged a > a.ged

TODO fix this
Compare; remove firsts line to ignore timestamp
  $ tail -n +10 a.ged > tail_a.ged
  $ tail -n +10 ./assets/abc.ged > tail_abc.ged
  $ diff tail_a.ged tail_abc.ged
  20,25c20,25
  < 2 CONT interface web, utilisable aussi bien sur un ordinateur non connecté à I
  < 2 CONC nternet qu'en service web. Initialement conçu en 1997 par Daniel de Rau
  < 2 CONC glaudre, il utilise des techniques de calcul de parenté et de consangui
  < 2 CONC nité innovantes, mises au point par Daniel de Rauglaudre et Didier Rémy
  < 2 CONC , directeur de recherche à l'Institut national de recherche en informat
  < 2 CONC ique et en automatique.
  ---
  > 2 CONC  interface web, utilisable aussi bien sur un ordinateur non connecté à 
  > 2 CONC Internet qu'en service web. Initialement conçu en 1997 par Daniel de Ra
  > 2 CONC uglaudre, il utilise des techniques de calcul de parenté et de consangu
  > 2 CONC inité innovantes, mises au point par Daniel de Rauglaudre et Didier Rém
  > 2 CONC y, directeur de recherche à l'Institut national de recherche en informa
  > 2 CONC tique et en automatique.
  [1]
