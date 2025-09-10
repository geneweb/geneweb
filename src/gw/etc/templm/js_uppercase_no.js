/* $Id: no_uppercase.js,v 7.00 2015/04/11 00:21:40 mr Exp $ */

  function tUC(id)
  {
    return id.value;
  }
  function tUC1(id)
  {
    return id.value;
  }
  function oS3()
  {
    var e1 = document.getElementById('mS');
    var e2 = document.getElementById('mSp');
    var e3 = document.getElementById('mSn');
    var v2 = tUC1(e2);
    var v3 = tUC(e3);
    var r1 = /(\*$)/;
    var t2 = r1.test(v2);
    var t3 = r1.test(v3);
    if(t2 == true)
    { 
      var l2 = v2.length - 1;
      var s2 = v2.substr(0,l2);
      e1.value = "P"; e2.name = "k"; e2.value = s2; e3.name="";
    }
    else
    {
      if(t3 == true)
      { 
        var l3 = v3.length - 1;
        var s3 = v3.substr(0,l3);
        e1.value = "N"; e2.name=""; e3.name = "k"; e3.value = s3;
      }
      else
      {
        if(v2 != "" && v2 >= 0){e1.name = ""; e2.name = "i"; e3.name = "";}
        else{e1.value = "S"; e2.name = "p"; e3.name = "n";}
      }
    }
  }