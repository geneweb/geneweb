  $ ged2gwb -bd . SIMPLE.GED -o SIMPLE
  Migration check for SIMPLE:
    Classic .gwf exists: false
    Reorg .gwf exists: false
  Creating default configuration: SIMPLE.gwf
  
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

  $ ged2gwb -bd . ALLGED.GED -o ALLGED
  Migration check for ALLGED:
    Classic .gwf exists: false
    Reorg .gwf exists: false
  Creating default configuration: ALLGED.gwf
  
  *** pass 1 (note)
  *** pass 2 (indi)
  *** pass 3 (fam)
  Not implemented typ = SUBN
  Not implemented typ = This
  *** Trailer ok
  Other parents for given name.0 surname
  - ?.9 ?
  - x.0 Adoptive mother
  => deleted in this family
  
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
  given name.0 surname's benediction before his/her birth
  
  given name.0 surname's burial before his/her bat mitzvah
  
  given name.0 surname's benediction before his/her birth
  
  given name.0 surname's burial before his/her bat mitzvah
  
  given name.0 surname married at age 0
  

  $ ged2gwb -bd . LTERCR.GED -o LTERCR
  Migration check for LTERCR:
    Classic .gwf exists: false
    Reorg .gwf exists: false
  Creating default configuration: LTERCR.gwf
  
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

  $ ged2gwb -bd . LTERLF.GED -o LTERLF
  Migration check for LTERLF:
    Classic .gwf exists: false
    Reorg .gwf exists: false
  Creating default configuration: LTERLF.gwf
  
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

  $ ged2gwb -bd . LTERCRLF.GED -o LTERCRLF
  Migration check for LTERCRLF:
    Classic .gwf exists: false
    Reorg .gwf exists: false
  Creating default configuration: LTERCRLF.gwf
  
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

  $ ged2gwb -bd . LTERLFCR.GED -o LTERLFCR
  Migration check for LTERLFCR:
    Classic .gwf exists: false
    Reorg .gwf exists: false
  Creating default configuration: LTERLFCR.gwf
  
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

Check the support for ANSEL encoding 
  $ ged2gwb -bd . ANSEL.GED -o ANSEL
  Migration check for ANSEL:
    Classic .gwf exists: false
    Reorg .gwf exists: false
  Creating default configuration: ANSEL.gwf
  
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

Check the support for UTF-16 LE encoding without BOM mark.
  $ ged2gwb -bd . ULHCL.GED -o ULHCL
  Error: UTF-16LE encoding detected, not supported
  Convert to UTF-8 first:
  iconv -f UTF-16LE -t UTF-8 ULHCL.GED > ULHCL_UTF8.GED
  [2]

Check the support for UTF-16 LE encoding with BOM mark
  $ ged2gwb -bd . ULHBOMCL.GED -o ULHBOMCL
  Error: UTF-16LE encoding detected, not supported
  Convert to UTF-8 first:
  iconv -f UTF-16LE -t UTF-8 ULHBOMCL.GED > ULHBOMCL_UTF8.GED
  [2]

Check the support for UTF-16 BE encoding with BOM mark
  $ ged2gwb -bd . UHLBOMCL.GED -o UHLBOMCL
  Error: UTF-16BE encoding detected, not supported
  Convert to UTF-8 first:
  iconv -f UTF-16BE -t UTF-8 UHLBOMCL.GED > UHLBOMCL_UTF8.GED
  [2]
