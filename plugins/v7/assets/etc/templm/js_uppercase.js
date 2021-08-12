/* $Id: js_uppercase.js,v 7.00 2016/06/09 17:53:56 mr Exp $ */

  function tUC(id)
  {
    var v = id.value.toUpperCase();
    for (var i = 0; i < p_a.length; ++i)
    {
      var r = eval("/\\b" + p_a[i] + "\\b/g");
      var t = r.test(v);
      if (t == true)
      {
        var v1 = v.replace(r,p_a[i].toLowerCase());
        var v = v1;
      }
    }
    id.value = v;
    return id.value;
  }
  function tUC1(id)
  {
    id.value = id.value.replace( /(^|\s|-)([a-z])/g , function(m,p1,p2){ return p1+p2.toUpperCase();} );
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
