/* uppercase.js mickroue(a)yahoo.fr 20140910 templ=templm */

  function tUC(id)
  {
    id.value = id.value.toUpperCase();
    return id.value;
  }
  function tUC1(id)
  {
    id.value = id.value.replace( /(^|\s)([a-z])/g , function(m,p1,p2){ return p1+p2.toUpperCase();} );
    return id.value;
  }
  function os1()
  { 
    var e1 = document.getElementById('NGm');
    var e2 = document.getElementById('NGfn');
    var e3 = document.getElementById('NGsn');
    var e4 = document.getElementById('NGn');
    var v2 = tUC1(e2);
    var v3 = tUC(e3);
    var r1 = /(\*$)/;
    var t2 = r1.test(v2);
    var t3 = r1.test(v3);
    if(t2 == true)
    { 
      var l2 = v2.length - 1;
      var s2 = v2.substr(0,l2);
      e1.value = "P"; e2.name = "k"; e2.value = s2; e3.name=""; e4.name = "";
    }
    else
    {
      if(t3 == true)
      { 
        var l3 = v3.length - 1;
        var s3 = v3.substr(0,l3);
        e1.value = "N"; e2.name=""; e3.name = "k"; e3.value = s3; e4.name = "";
      }
      else {e1.value = "NG"; e2.name = "fn"; e3.name = "sn"; e4.name = "n";}
    }
  }
