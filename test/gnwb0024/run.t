A base written before witness notes: format GnWb0024, with GnPa0001
patches (an occupation and a family comment changed online). Its
witnesses are pairs; they are read with an empty note.

  $ head -c 8 old.gwb/base; echo
  GnWb0024
  $ head -c 8 old.gwb/patches; echo
  GnPa0001

  $ gwu -bd . old -o old.gw
  $ cat old.gw
  encoding: utf-8
  gwplus
  
  fam Dupont Jean #occu Notaire_royal 1/1/1850 #bp Lyon 1920 #dp Paris +10/5/1875 #mp Paris Martin Marie 0
  wit m: Durand Pierre 1840
  wit f: Leroy Anne 1845
  comm Mariage en présence de la famille
  fevt
  #marr 10/5/1875 #p Paris
  wit m: Durand Pierre
  wit f: #info Leroy Anne
  end fevt
  beg
  - m Paul 1876
  end
  
  notes Dupont Jean
  beg
  Une note.
  end notes
  
  pevt Dupont Paul
  #birt 1876
  end pevt
  
  pevt Dupont Jean
  #birt 1/1/1850 #p Lyon
  wit m: Bernard Louis 1820
  wit m: #godp Durand Pierre
  #deat 1920 #p Paris
  end pevt
  
  pevt Leroy Anne
  #birt 1845
  end pevt
  
  pevt Durand Pierre
  #birt 1840
  end pevt

Recompiling the export writes the current format, with the same content.

  $ gwc -bd . old.gw -o new -f > /dev/null 2>&1
  $ head -c 8 new.gwb/base; echo
  GnWb0025
  $ gwu -bd . new -o new.gw
  $ diff old.gw new.gw
