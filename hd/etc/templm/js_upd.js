/* upd.js mickroue(a)yahoo.fr 20140923 templ=templm */
  function oKP1(event)
  {
    var key = event.keyCode ? event.keyCode : event.which ? event.which : event.charCode;
    var charKey = String.fromCharCode(key).toUpperCase();
    var r = /[MFN]/; /* lettres choisies */
    var t = r.test(charKey);
    if (t == true)
    {
      var e1 = document.getElementById("sex1");
      var e2 = document.getElementById("first_name");
      switch(charKey)
      {
        case "M": e1.value = "M"; e2.focus(); break;
        case "F": e1.value = "F"; e2.focus(); break;
        case "N": e1.value = "N"; e2.focus(); break;
      }
      return false;
    }
  }
  function oKP2(event,z1,z2,z3)
  {
    var key = event.keyCode ? event.keyCode : event.which ? event.which : event.charCode;
    var charKey = String.fromCharCode(key).toUpperCase();
    if (z1 == "ch" || z1 == "witn")
    {
      var r = /[MFNP]/; /* lettres choisies */
      var t = r.test(charKey);
      if (t == true)
      {
          var e1 = document.getElementById(z1 + z2 + "_occ");
          var e2 = document.getElementById(z1 + z2 + "_fn");
          var a1 = (e1.value == "") ? e1.getAttribute("placeHolder","false"): e1.value;
          switch(charKey)
          {
            case "M": e1.value = "M"; e2.focus(); break;/* masculin */
            case "F": e1.value = "F"; e2.focus(); break;/* féminin */
            case "N": e1.value = "N"; e2.focus(); break;/* neutre */
            case "P": if(z1 == "ch")visHid(z1,'_sn'); break;/* afficher/masquer patronyme de l'enfant*/
          }
        return false;
      }
    }
    var r = /[RDIAHB]/; /* lettres choisies */
    var t = r.test(charKey);
    if (t == true)
    {
      switch(charKey)
      {
        case "R": invertS(z1,z2,z3,0,1); break;/* remonter (permuter 3 et 2) */
        case "D": invertS(z1,z2,z3,1,1); break;/* descendre (permuter 4 et 3) */
        case "I": addS(z1,z2,0); break;/* insérer une ligne sur place */
        case "A": addS(z1,z2,1); break;/* ajouter une ligne à la fin */
        case "H": invertS(z1,z2,z3,0,0); break;/* haut (position sur la ligne précédente) */
        case "B": invertS(z1,z2,z3,1,0); break;/* bas (position sur la ligne suivante) */
      }
      return false;
    }
    if (z1 == "r")
    {
      var r = /[VWXYZ]/; /* lettres choisies */
      var t = r.test(charKey);
      if (t == true)
      {
        var n = document.getElementsByName(z1 + z2 + "_type")[0];
        switch(charKey)
        {
          case "V": n.options[0].selected = true; break;/* Parrain et Marraine */
          case "W": n.options[1].selected = true; break;/* Parents adoptifs */
          case "X": n.options[2].selected = true; break;/* Parents l’ayant reconnu */
          case "Y": n.options[3].selected = true; break;/* Parents possibles */
          case "Z": n.options[4].selected = true; break;/* Parents nourriciers */
        }
        return false;
      }
    }
  }
  function accent0()
  {
    var v1=document.getElementById('notes');
    v1.style.width="369px";
    v1.style.height="299px";
    document.getElementById('accent').style.display="none";
    document.getElementById('accent0').style.display="none";
    document.getElementById('accent1').style.display="inline";
    document.getElementById('notes1').style.width="375px";
    document.getElementById('accent2').style.width="auto";
    document.getElementById('notes2').style.width="auto";
    document.getElementById('death_src').focus();
    v1.focus();
  }
  function accent1()
  {
    var v1=document.getElementById('notes');
    v1.style.width="99%";    
    v1.style.height="400px";
    document.getElementById('accent').style.display="block";
    document.getElementById('accent0').style.display="inline";
    document.getElementById('accent1').style.display="none";
    document.getElementById('notes1').style.width="99%";
    document.getElementById('accent2').style.width="99%";
    document.getElementById('notes2').style.width="99%";
    document.getElementById('occu').focus();
    v1.focus();
  }
  function itemMaxCnt(item,cnt)
  {
    while (document.getElementById(item + cnt)){cnt++}
    return cnt;
  }
  var ldlLen = 0;
  var ldlTim;
  var ldlVal = "";
  function ldl(z1,z2,z3)
  {
    var a1 = "o" + z2;
    var a2 = z1.value.length;
    var a3 = document.getElementById(a1).value;
    var a4 = z1.value.slice(0,3);
    var key = z3.keyCode ? z3.keyCode : z3.which ? z3.which : z3.charCode;
    if(a3 > 20 && ldlVal == a4)ldlVal = "";
    if(ldlVal != a4 && a2 != ldlLen && a2 > 2 && key != "8" && key != "46")
    {
      clearTimeout(ldlTim); 
      ldlVal = a4;
      ldlLen = a2;
      ldlTim = setTimeout(
      function()
      { 
        var b1 = encodeURI(z1.value);
        var b2 = "#dl" + z2;
        var b3 = "?m=MOD_DATA;data=" + z2 + ";s=" + b1 + ";datalist=on;";
        $(b2).load(b3);
      },1000);
    }
  }
  function addItem(item,cnt,lex)
  {
    var cnt1 = itemMaxCnt(item,cnt);
    var cnt2 = cnt1 + 1;
    var id0 = item + cnt1;
    var id1 = "new_" + item;
    document.getElementById(id1).outerHTML = '\
    <label>' + lex + ' ' + cnt2 + '<input id="' + id0 + '" name="' + id0 + '" size="30" value=""\/><\/label><a href="javascript:addItem(\'' + item + '\',' + cnt2 + ',\''+ lex + '\')"> + <\/a>\n\
    <span id="' + id1 + '"><\/span>';
    for (var i = cnt1; i > cnt; i--)
    {
      invertItem(item,i);
    }
    document.getElementById(item + cnt).focus();
  }
  function invertItem(item,cnt)
  {
    if (cnt > 0)
    {
      var cnt1 = cnt - 1;
      var v1 = document.getElementById(item + cnt1).value; 
      var v2 = document.getElementById(item + cnt).value;
      document.getElementById(item + cnt1).value = v2;
      document.getElementById(item + cnt).value = v1;
    }
  }
  function addRelation(z1,z2)
  {
    var c0 = z1;
    var c1 = itemMaxCnt('r',c0);
    var v = document.getElementById("r1_fath_fn").value;
    var v1 = document.getElementById("r1_moth_fn").value;
    if(v != "" || v1 != "")
    {
      var t0 = "r" + c0;
      var t1 = "r" + c1;
      var t1f = t1 + "_fath";
      var t1m = t1 + "_moth";
      var l2 = document.getElementsByName("r1_type")[0].options[0].firstChild.data;
      var l3 = document.getElementsByName("r1_type")[0].options[1].firstChild.data;
      var l4 = document.getElementsByName("r1_type")[0].options[2].firstChild.data;
      var l5 = document.getElementsByName("r1_type")[0].options[3].firstChild.data;
      var l6 = document.getElementsByName("r1_type")[0].options[4].firstChild.data;
      var l12 = document.getElementById("rM").firstChild.data;
      var l13 = document.getElementById("rF").firstChild.data;
      document.getElementById("new_relation").outerHTML = '\
      <tr id="' + t1 + '">\n\
        <td><input type="hidden" id="' + t1f + '_p" name="' + t1f + '_p" value="create">\n\
            <input id="' + t1f + '_occ" name="' + t1f + '_occ" class="occ0" autocomplete="off" placeholder="' + l12 + '" size="3" maxlength="8" value="" onkeypress="javascript:return oKP2(event,\'r\',' + c1 + ',\'_fath_occ\')" onblur="oB3(\'' + t1f + '\')"><\/td>\n\
        <td><input id="' + t1f + '_fn" name="' + t1f + '_fn" class="ar" size="30" maxlength="200" value="" onblur="tUC1(this)" onkeydown="if(event.keyCode == 13)tUC1(this)"><\/td>\n\
        <td><input id="' + t1f + '_sn" name="' + t1f + '_sn" size="30" maxlength="200" value="" onblur="tUC(this)" onkeydown="if(event.keyCode == 13)tUC(this)"><\/td>\n\
        <td rowspan="2" class="bg7"><select id="' + t1 + '_type" name="' + t1 + '_type">\n\
        <option value="GodParent" selected="selected">' + l2 + '<\/option>\n\
        <option value="Adoption">' + l3 + '<\/option>\n\
        <option value="Recognition">' + l4 + '<\/option>\n\
        <option value="CandidateParent">' + l5 + '<\/option>\n\
        <option value="FosterParent">' + l6 + '<\/option>\n\
        <\/select><\/td>\n\
        <\/tr>\n\
        <tr>\n\
        <td><input type="hidden" id="' + t1m + '_p" name="' + t1m + '_p" value="create">\n\
            <input id="' + t1m + '_occ" name="' + t1m + '_occ" class="occ1" autocomplete="off" placeholder="' + l13 + '" size="3" maxlength="8" value="" onkeypress="javascript:return oKP2(event,\'r\',\'' + c1 + '\',\'_moth_occ\')" onblur="oB3(\'' + t1m + '\')"><\/td>\n\
        <td><input id="' + t1m + '_fn" name="' + t1m + '_fn" class="ar" size="30" maxlength="200" value="" onblur="tUC1(this)" onkeydown="if(event.keyCode == 13)tUC1(this)"><\/td>\n\
        <td><input id="' + t1m + '_sn" name="' + t1m + '_sn" size="30" maxlength="200" value="" onblur="tUC(this)" onkeydown="if(event.keyCode == 13)tUC(this)"><\/td>\n\
      <\/tr>\n\
      <tr id="new_relation"><\/tr>';
      if(z2 == 0)
      {
        for(var i = c1; i > c0; i--){invertRelation(i);}
        sIV(t0 + "_fath_occ");
      }
      else sIV(t1 + "_fath_occ");
    }
    else sIV("r1_fath_occ");
  }
  function invertRelation(cnt)
  {
    if (cnt > 0)
    {
      var t0 = "r" + (cnt - 1);
      var t1 = "r" + cnt;
      var a = new Array();
      
      a[0] = new Array();
      a[0]["e10"]= t0 + "_type";
      a[0]["e11"]= t0 + "_fath_fn";
      a[0]["e12"]= t0 + "_fath_sn";
      a[0]["e13"]= t0 + "_fath_occ";
      a[0]["e14"]= t0 + "_moth_fn";
      a[0]["e15"]= t0 + "_moth_sn";
      a[0]["e16"]= t0 + "_moth_occ";
      a[0]["e17"]= t0 + "_fath_p";
      a[0]["e18"]= t0 + "_moth_p";
      
      a[1] = new Array();
      a[1]["e10"]= t1 + "_type";
      a[1]["e11"]= t1 + "_fath_fn";
      a[1]["e12"]= t1 + "_fath_sn";
      a[1]["e13"]= t1 + "_fath_occ";
      a[1]["e14"]= t1 + "_moth_fn";
      a[1]["e15"]= t1 + "_moth_sn";
      a[1]["e16"]= t1 + "_moth_occ";
      a[1]["e17"]= t1 + "_fath_p";
      a[1]["e18"]= t1 + "_moth_p";
      
      var n0 = document.getElementsByName(a[0]["e10"])[0];
      var n1 = document.getElementsByName(a[1]["e10"])[0];
      for(var i = 0; i < n0.length; i++)
      {
        var v0 = n0.options[i].selected;
        var v1 = n1.options[i].selected;
        if (v0 == true)
          var v0true = n1.options[i];
        if (v1 == true)
          var v1true = n0.options[i];
      }
      v0true.selected = true;
      v1true.selected = true;
         
      for (var i = 11; i <= 18; i++)
      {
        var v0 = document.getElementById(a[0]["e" + i]).value; 
        var v1 = document.getElementById(a[1]["e" + i]).value;
        document.getElementById(a[0]["e" + i]).value = v1;
        document.getElementById(a[1]["e" + i]).value = v0;
      }
    }
  }
  function addTitle(z1,z2)
  {
    var c0 = z1;
    var c1 = itemMaxCnt('t',c0);
    var sc1 = "t_date_start" + c1;
    var ec1 = "t_date_end" + c1;
    var v = document.getElementById("t_ident1").value;
    if(v != "")
    {
      var dmy = document.getElementById("dmy").firstChild.data;
      if (dmy == "ddmmyyyy")
      {
        var d_start ="\
<input id=" + sc1 + "_dd name=" + sc1 + "_dd class=d autocomplete=off size=1 maxlength=2 value=\"\" onkeypress=\"javascript:return oKPdd(event,'" + sc1 + "','_dd','_mm');\"\/>\
<input id=" + sc1 + "_mm name=" + sc1 + "_mm class=m autocomplete=off size=1 maxlength=2 value=\"\" list=\"dlmonth\" onkeypress=\"javascript:return oKPmm(event,'" + sc1 + "','_mm','_yy');\" onkeyup=\"javascript:return oKU1(event,'" + sc1 + "_mm','" + sc1 + "_dd');\"\/>\
<input id=" + sc1 + "_yy name=" + sc1 + "_yy class=y autocomplete=off size=8 maxlength=12 value=\"\" onkeydown=\"if(event.keyCode == 13)javascript:evD('" + sc1 + "')\" onkeyup=\"javascript:return oKU1(event,'" + sc1 + "_yy','" + sc1 + "_mm');\" onblur=\"javascript:evD('" + sc1 + "')\"\/>"
        var d_end ="\
<input id=" + ec1 + "_dd name=" + ec1 + "_dd class=d autocomplete=off size=1 maxlength=2 value=\"\" onkeypress=\"javascript:return oKPdd(event,'" + ec1 + "','_dd','_mm');\"\/>\
<input id=" + ec1 + "_mm name=" + ec1 + "_mm class=m autocomplete=off size=1 maxlength=2 value=\"\" list=\"dlmonth\" onkeypress=\"javascript:return oKPmm(event,'" + ec1 + "','_mm','_yy');\" onkeyup=\"javascript:return oKU1(event,'" + ec1 + "_mm','" + ec1 + "_dd');\"\/>\
<input id=" + ec1 + "_yy name=" + ec1 + "_yy class=y autocomplete=off size=8 maxlength=12 value=\"\" onkeydown=\"if(event.keyCode == 13)javascript:evD('" + ec1 + "')\" onkeyup=\"javascript:return oKU1(event,'" + ec1 + "_yy','" + ec1 + "_mm');\" onblur=\"javascript:evD('" + ec1 + "')\"\/>"
      }
      if (dmy == "mmddyyyy")
      {
        var d_start ="\
<input id=" + sc1 + "_mm name=" + sc1 + "_mm class=m autocomplete=off size=1 maxlength=2 value=\"\" list=\"dlmonth\" onkeypress=\"javascript:return oKPmm(event,'" + sc1 + "','_mm','_dd');\"\/>\
<input id=" + sc1 + "_dd name=" + sc1 + "_dd class=d autocomplete=off size=1 maxlength=2 value=\"\" onkeypress=\"javascript:return oKPdd(event,'" + sc1 + "','_dd','_yy');\" onkeyup=\"javascript:return oKU1(event,'" + sc1 + "_dd','" + sc1 + "_mm');\"\/>\
<input id=" + sc1 + "_yy name=" + sc1 + "_yy class=y autocomplete=off size=8 maxlength=12 value=\"\" onkeydown=\"if(event.keyCode == 13)javascript:evD('" + sc1 + "')\" onkeyup=\"javascript:return oKU1(event,'" + sc1 + "_yy','" + sc1 + "_dd');\" onblur=\"javascript:evD('" + sc1 + "')\"\/>"
        var d_end ="\
<input id=" + ec1 + "_mm name=" + ec1 + "_mm class=m autocomplete=off size=1 maxlength=2 value=\"\" list=\"dlmonth\" onkeypress=\"javascript:return oKPmm(event,'" + ec1 + "','_mm','_dd');\"\/>\
<input id=" + ec1 + "_dd name=" + ec1 + "_dd class=d autocomplete=off size=1 maxlength=2 value=\"\" onkeypress=\"javascript:return oKPdd(event,'" + ec1 + "','_dd','_yy');\" onkeyup=\"javascript:return oKU1(event,'" + ec1 + "_dd','" + ec1 + "_mm');\"\/>\
<input id=" + ec1 + "_yy name=" + ec1 + "_yy class=y autocomplete=off size=8 maxlength=12 value=\"\" onkeydown=\"if(event.keyCode == 13)javascript:evD('" + ec1 + "')\" onkeyup=\"javascript:return oKU1(event,'" + ec1 + "_yy','" + ec1 + "_dd');\" onblur=\"javascript:evD('" + ec1 + "')\"\/>"
      }
      if (dmy == "yyyymmdd")
      {
        var d_start ="\
<input id=" + sc1 + "_yy name=" + sc1 + "_yy class=y autocomplete=off size=8 maxlength=12 value=\"\" onkeypress=\"javascript:return cF1(event,'" + sc1 + "_mm');\" onkeydown=\"if(event.keyCode == 13)javascript:evD('" + sc1 + "')\" onblur=\"javascript:evD('" + sc1 + "')\"\/>\
<input id=" + sc1 + "_mm name=" + sc1 + "_mm class=m autocomplete=off size=1 maxlength=2 value=\"\" list=\"dlmonth\" onkeypress=\"javascript:return oKPmm(event,'" + sc1 + "','_mm','_dd');\" onkeyup=\"javascript:return oKU1(event,'" + sc1 + "_mm','" + sc1 + "_yy');\"\/>\
<input id=" + sc1 + "_dd name=" + sc1 + "_dd class=d autocomplete=off size=1 maxlength=2 value=\"\" onkeypress=\"javascript:return oKPdd(event,'" + sc1 + "','_dd','_place');\" onkeyup=\"javascript:return oKU1(event,'" + sc1 + "_dd','" + sc1 + "_mm');\"\/>"
        var d_end ="\
<input id=" + ec1 + "_yy name=" + ec1 + "_yy class=y autocomplete=off size=8 maxlength=12 value=\"\" onkeypress=\"javascript:return cF1(event,'" + ec1 + "_mm');\" onkeydown=\"if(event.keyCode == 13)javascript:evD('" + ec1 + "')\" onblur=\"javascript:evD('" + ec1 + "')\"\/>\
<input id=" + ec1 + "_mm name=" + ec1 + "_mm class=m autocomplete=off size=1 maxlength=2 value=\"\" list=\"dlmonth\" onkeypress=\"javascript:return oKPmm(event,'" + ec1 + "','_mm','_dd');\" onkeyup=\"javascript:return oKU1(event,'" + ec1 + "_mm','" + ec1 + "_yy');\"\/>\
<input id=" + ec1 + "_dd name=" + ec1 + "_dd class=d autocomplete=off size=1 maxlength=2 value=\"\" onkeypress=\"javascript:return oKPdd(event,'" + ec1 + "','_dd','_place');\" onkeyup=\"javascript:return oKU1(event,'" + ec1 + "_dd','" + ec1 + "_mm2');\"\/>"
      }
      document.getElementById("new_title").outerHTML = '\
      <tr id="t' + c1 + '">\n\
      <td class="bg7"><input id="t' + c1 + '_occ" name="t' + c1 + '_occ" class="t_occ" size=1 maxlength=1 placeholder="&bull;" onkeypress="javascript:return oKP2(event,\'t\',' + c1 + ',\'_occ\')" onblur="this.value=\'\';"%/></td>\n\
      <td><input id="t_ident' + c1 + '" name="t_ident' + c1 + '" size="30" value=""<\/td>\n\
      <td><input id="t_place' + c1 + '" name="t_place' + c1 + '" size="30" value=""><\/td>\n\
      <td><input id="t_name' + c1 + '" name="t_name' + c1 + '" size="30" value=""><\/td>\n\
      <td><input autocomplete="off" class="number" id="t_nth' + c1 + '" name="t_nth' + c1 + '" size="3" value=""><\/td>\n\
      <input type=hidden id="' + sc1 + '_cal" name="' + sc1 + '_cal" value="" >\n\
      <input type=hidden id="' + sc1 + '_yyyy" name="' + sc1 + '_yyyy" value="" >\n\
      <input type=hidden id="' + sc1 + '_oryear" name="' + sc1 + '_oryear" value="" >\n\
      <input type=hidden id="' + sc1 + '_text" name="' + sc1 + '_text" value="" >\n\
      <input type=hidden id="' + sc1 + '_prec" name="' + sc1 + '_prec" value="" >\n\
      <input type=hidden id="' + ec1 + '_cal" name="' + ec1 + '_cal" value="" >\n\
      <input type=hidden id="' + ec1 + '_yyyy" name="' + ec1 + '_yyyy" value="" >\n\
      <input type=hidden id="' + ec1 + '_oryear" name="' + ec1 + '_oryear" value="" >\n\
      <input type=hidden id="' + ec1 + '_text" name="' + ec1 + '_text" value="" >\n\
      <input type=hidden id="' + ec1 + '_prec" name="' + ec1 + '_prec" value="" >\n\
      <td><span class="dmyt">' + d_start + '<\/span><span class="dmyt">' + d_end + '<\/span><\/td>\n\
      <\/tr>\n\
      <tr id="new_title"><\/tr>';
      if(z2 == 0)
      {
        for (var i = c1; i > c0; i--)
        {
          invertTitle(i);
        }
        sIV("t_ident" + c0);
      }
      else sIV("t_ident" + c1);
    }
    else sIV("t_ident1");
  }
  function invertTitle(cnt)
  {
    if (cnt > 1)
    {
     var c0 = cnt - 1;
     var sc0 = "t_date_start" + c0;
     var ec0 = "t_date_end" + c0;
     var c1 = cnt;
     var sc1 = "t_date_start" + c1;
     var ec1 = "t_date_end" + c1;
     var a = new Array();

     a[0] = new Array();
     a[0]["e10"] = "t_ident" + c0;
     a[0]["e11"] = "t_place" + c0;
     a[0]["e12"] = "t_name" + c0;
     a[0]["e13"] = "t_nth" + c0;
     a[0]["e14"] = sc0 + "_cal";
     a[0]["e15"] = sc0 + "_oryear";
     a[0]["e16"] = sc0 + "_text";
     a[0]["e17"] = sc0 + "_prec";
     a[0]["e18"] = sc0 + "_dd";
     a[0]["e19"] = sc0 + "_mm";
     a[0]["e20"] = sc0 + "_yy";
     a[0]["e21"] = sc0 + "_yyyy";
     a[0]["e22"] = ec0 + "_cal";
     a[0]["e23"] = ec0 + "_oryear";
     a[0]["e24"] = ec0 + "_text";
     a[0]["e25"] = ec0 + "_prec";
     a[0]["e26"] = ec0 + "_dd";
     a[0]["e27"] = ec0 + "_mm";
     a[0]["e28"] = ec0 + "_yy";
     a[0]["e29"] = ec0 + "_yyyy";
    
     a[1] = new Array();
     a[1]["e10"] = "t_ident" + c1;
     a[1]["e11"] = "t_place" + c1;
     a[1]["e12"] = "t_name" + c1;
     a[1]["e13"] = "t_nth" + c1;
     a[1]["e14"] = sc1 + "_cal";
     a[1]["e15"] = sc1 + "_oryear";
     a[1]["e16"] = sc1 + "_text";
     a[1]["e17"] = sc1 + "_prec";
     a[1]["e18"] = sc1 + "_dd";
     a[1]["e19"] = sc1 + "_mm";
     a[1]["e20"] = sc1 + "_yy";
     a[1]["e21"] = sc1 + "_yyyy";
     a[1]["e22"] = ec1 + "_cal";
     a[1]["e23"] = ec1 + "_oryear";
     a[1]["e24"] = ec1 + "_text";
     a[1]["e25"] = ec1 + "_prec";
     a[1]["e26"] = ec1 + "_dd";
     a[1]["e27"] = ec1 + "_mm";
     a[1]["e28"] = ec1 + "_yy";
     a[1]["e29"] = ec1 + "_yyyy";

     for (var i=10; i<=29; i++)
     {
       var v1 = document.getElementById(a[0]["e" + i]).value; 
       var v2 = document.getElementById(a[1]["e" + i]).value;
       document.getElementById(a[0]["e" + i]).value = v2;
       document.getElementById(a[1]["e" + i]).value = v1;
     }
    }
  }
  function addChild(z1,z2)
  {
    var c0 = z1;
    var c1 = itemMaxCnt('ch',c0);
    var v = document.getElementById("ch1_fn").value;
    if(v != "")
    {
      var t0 = "ch" + c0;
      var t1 = "ch" + c1;
      var v1 = document.getElementById("pa1_sn").value;
      var v2 = document.getElementById("ch1_sn").type;
      var dmy = document.getElementById("dmy").firstChild.data;
      if (dmy == "ddmmyyyy")
      {
        var d_b ="\
<input id=" + t1 + "b_dd name=" + t1 + "b_dd class=d autocomplete=off size=1 maxlength=2 value=\"\" onkeypress=\"javascript:return oKPdd(event,'" + t1 + "b','_dd','_mm');\"\/>\
<input id=" + t1 + "b_mm name=" + t1 + "b_mm class=m autocomplete=off size=1 maxlength=2 value=\"\" list=\"dlmonth\" onkeypress=\"javascript:return oKPmm(event,'" + t1 + "b','_mm','_yy');\" onkeyup=\"javascript:return oKU1(event,'" + t1 + "b_mm','" + t1 + "b_dd');\"\/>\
<input id=" + t1 + "b_yy name=" + t1 + "b_yyyy class=y autocomplete=off size=4 maxlength=12 value=\"\" onkeydown=\"if(event.keyCode == 13)javascript:evSD('" + t1 + "b')\" onkeypress=\"javascript:return cF2(event,\'ch\'," + c1 + ",\'b_yy\');\" onkeyup=\"javascript:return oKU1(event,'" + t1 + "b_yy','" + t1 + "b_mm');\" onblur=\"javascript:evSD('" + t1 + "b')\"\/>"
        var d_d ="\
<input id=" + t1 + "d_dd name=" + t1 + "d_dd class=d autocomplete=off size=1 maxlength=2 value=\"\" onkeypress=\"javascript:return oKPdd(event,'" + t1 + "d','_dd','_mm');\"\/>\
<input id=" + t1 + "d_mm name=" + t1 + "d_mm class=m autocomplete=off size=1 maxlength=2 value=\"\" list=\"dlmonth\" onkeypress=\"javascript:return oKPmm(event,'" + t1 + "d','_mm','_yy');\" onkeyup=\"javascript:return oKU1(event,'" + t1 + "d_mm','" + t1 + "d_dd');\"\/>\
<input id=" + t1 + "d_yy name=" + t1 + "d_yyyy class=y autocomplete=off size=4 maxlength=12 value=\"\" onkeydown=\"if(event.keyCode == 13)javascript:evSD('" + t1 + "d')\" onkeypress=\"javascript:return cF2(event,\'ch\'," + c1 + ",\'d_yy\');\" onkeyup=\"javascript:return oKU1(event,'" + t1 + "d_yy','" + t1 + "d_mm');\" onblur=\"javascript:evSD('" + t1 + "d')\"\/>"
      }
      if (dmy == "mmddyyyy")
      {
        var d_b ="\
<input id=" + t1 + "b_mm name=" + t1 + "b_mm class=m autocomplete=off size=1 maxlength=2 value=\"\" list=\"dlmonth\" onkeypress=\"javascript:return oKPmm(event,'" + t1 + "b','_mm','_dd');\"\/>\
<input id=" + t1 + "b_dd name=" + t1 + "b_dd class=d autocomplete=off size=1 maxlength=2 value=\"\" onkeypress=\"javascript:return oKPdd(event,'" + t1 + "b','_dd','_yy');\" onkeyup=\"javascript:return oKU1(event,'" + t1 + "b_dd','" + t1 + "b_mm');\"\/>\
<input id=" + t1 + "b_yy name=" + t1 + "b_yyyy class=y autocomplete=off size=4 maxlength=12 value=\"\" onkeypress=\"javascript:return cF2(event,\'ch\'," + c1 + ",\'b_yy\');\" onkeyup=\"javascript:return oKU1(event,'" + t1 + "b_yy','" + t1 + "b_dd');\" onkeydown=\"if(event.keyCode == 13)javascript:evSD('" + t1 + "b')\" onblur=\"javascript:evSD('" + t1 + "b')\"\/>"
        var d_d ="\
<input id=" + t1 + "d_mm name=" + t1 + "d_mm class=m autocomplete=off size=1 maxlength=2 value=\"\" list=\"dlmonth\" onkeypress=\"javascript:return oKPmm(event,'" + t1 + "d','_mm','_dd');\"\/>\
<input id=" + t1 + "d_dd name=" + t1 + "d_dd class=d autocomplete=off size=1 maxlength=2 value=\"\" onkeypress=\"javascript:return oKPdd(event,'" + t1 + "d','_dd','_yy');\" onkeyup=\"javascript:return oKU1(event,'" + t1 + "d_dd','" + t1 + "d_mm');\"\/>\
<input id=" + t1 + "d_yy name=" + t1 + "d_yyyy class=y autocomplete=off size=4 maxlength=12 value=\"\" onkeydown=\"if(event.keyCode == 13)javascript:evSD('" + t1 + "d')\" onkeypress=\"javascript:return cF2(event,\'ch\'," + c1 + ",\'d_yy\');\" onkeyup=\"javascript:return oKU1(event,'" + t1 + "d_yy','" + t1 + "d_dd');\" onblur=\"javascript:evSD('" + t1 + "d')\"\/>"
      }
      if (dmy == "yyyymmdd")
      {
        var d_b ="\
<input id=" + t1 + "b_yy name=" + t1 + "b_yyyy class=y autocomplete=off size=4 maxlength=12 value=\"\" onkeypress=\"javascript:return cF2(event,\'ch\'," + c1 + ",\'b_yy\');\" onkeydown=\"if(event.keyCode == 13)javascript:evSD('" + t1 + "b')\" onblur=\"javascript:evSD('" + t1 + "b')\"\/>\
<input id=" + t1 + "b_mm name=" + t1 + "b_mm class=m autocomplete=off size=1 maxlength=2 value=\"\" list=\"dlmonth\" onkeypress=\"javascript:return oKPmm(event,'" + t1 + "b','_mm','_dd');\" onkeyup=\"javascript:return oKU1(event,'" + t1 + "b_mm','" + t1 + "b_yy');\"\/>\
<input id=" + t1 + "b_dd name=" + t1 + "b_dd class=d autocomplete=off size=1 maxlength=2 value=\"\" onkeypress=\"javascript:return oKPdd(event,'" + t1 + "b','_dd','_pl');\" onkeyup=\"javascript:return oKU1(event,'" + t1 + "b_dd','" + t1 + "b_mm');\"\/>"
        var d_d ="\
<input id=" + t1 + "d_yy name=" + t1 + "d_yyyy class=y autocomplete=off size=4 maxlength=12 value=\"\" onkeypress=\"javascript:return cF2(event,\'ch\'," + c1 + ",\'d_yy\');\" onkeydown=\"if(event.keyCode == 13)javascript:evSD('" + t1 + "d')\" onblur=\"javascript:evSD('" + t1 + "d')\"\/>\
<input id=" + t1 + "d_mm name=" + t1 + "d_mm class=m autocomplete=off size=1 maxlength=2 value=\"\" list=\"dlmonth\" onkeypress=\"javascript:return oKPmm(event,'" + t1 + "d','_mm','_dd');\" onkeyup=\"javascript:return oKU1(event,'" + t1 + "d_mm','" + t1 + "d_yy');\"\/>\
<input id=" + t1 + "d_dd name=" + t1 + "d_dd class=d autocomplete=off size=1 maxlength=2 value=\"\"\ onkeypress=\"javascript:return oKPdd(event,'" + t1 + "d','_dd','_pl');\" onkeyup=\"javascript:return oKU1(event,'" + t1 + "d_dd','" + t1 + "d_mm');\"/>"
      }
      document.getElementById("new_child").outerHTML = '\
      <tr id="' + t1 + '">\n\
        <td>\n\
          <input type="hidden" id="' + t1 + '_sex" name="' + t1 + '_sex" value="N">\n\
          <input type="hidden" id="' + t1 + '_p" name="' + t1 + '_p" value="create">\n\
          <input id="' + t1 + '_occ" name="' + t1 + '_occ" class="occ2" autocomplete="off" size="5" maxlength="8" placeholder="N" value="" onkeypress="javascript:return oKP2(event,\'ch\',' + c1 + ',\'_occ\')" onblur="oB2(\'' + t1 + '\')">\n\
        <\/td>\n\
        <td><input id="' + t1 + '_fn" name="' + t1 + '_fn" size="30" maxlength="200" value="" onkeypress="javascript:return cF2(event,\'ch\',' + c1 + ',\'_fn\');" onblur="tUC1(this)" onkeydown="if(event.keyCode == 13)tUC1(this)"><br>\
            <input type="' + v2 + '" id="' + t1 + '_sn" name="' + t1 + '_sn" class="ar" size="30" maxlength="200" value="" placeholder="' + v1 + '" onkeypress="javascript:return cF2(event,\'ch\',' + c1 + ',\'_sn\');" onblur="tUC(this)" onkeydown="if(event.keyCode == 13)tUC(this)"><\/td>\n\
        <td><span id="dp' + t1 + '" class="vis">\n\
              <span class="dmyt">' + d_b + '<input id="' + t1 + 'b_pl" name="' + t1 + 'b_pl" class="pl" size="44" maxlength="200" value="" onblur="fillPlaceFam(this)" list="dlplace"><\/span>\n\
              <span class="dmyt">' + d_d + '<input id="' + t1 + 'd_pl" name="' + t1 + 'd_pl" class="pl" size="44" maxlength="200" value="" onblur="fillPlaceFam(this)" list="dlplace"><\/span>\n\
            <\/span><\/td>\n\
        <td><input id="' + t1 + '_occupation" name="' + t1 + '_occupation" class="vis" size="40" maxlength="200" value="" list="dloccu"><\/td>\n\
      <\/tr>\n\
      <tr id="new_child"><\/tr>';
      if(z2 == 0)
      {
        for(var i = c1; i > c0; i--)invertChild(i);
        sIV(t0 + "_occ");
      }
      else sIV(t1 + "_occ");
    }
    else sIV("ch1_occ");
  }
  function invertChild(cnt)
  {
    if (cnt > 0)
    {
      var t0 = "ch" + (cnt - 1);
      var t1 = "ch" + cnt;
      var a = new Array();
      
      a[0] = new Array();
      a[0]["e10"]= t0 + "_fn";
      a[0]["e11"]= t0 + "_sn";
      a[0]["e12"]= t0 + "_occ";
      a[0]["e13"]= t0 + "b_dd";
      a[0]["e14"]= t0 + "b_mm";
      a[0]["e15"]= t0 + "b_yy";
      a[0]["e16"]= t0 + "b_pl";
      a[0]["e17"]= t0 + "d_dd";
      a[0]["e18"]= t0 + "d_mm";
      a[0]["e19"]= t0 + "d_yy";
      a[0]["e20"]= t0 + "d_pl";
      a[0]["e21"]= t0 + "_occupation";
      a[0]["e22"]= t0 + "_sex";
      a[0]["e23"]= t0 + "_p";
      a[0]["e24"]= "dp" + t0;
      a[0]["e25"]= t0 + "_occupation";
      a[0]["e26"]= t0 + "_occ";

      a[1] = new Array();
      a[1]["e10"]= t1 + "_fn";
      a[1]["e11"]= t1 + "_sn";
      a[1]["e12"]= t1 + "_occ";
      a[1]["e13"]= t1 + "b_dd";
      a[1]["e14"]= t1 + "b_mm";
      a[1]["e15"]= t1 + "b_yy";
      a[1]["e16"]= t1 + "b_pl";
      a[1]["e17"]= t1 + "d_dd";
      a[1]["e18"]= t1 + "d_mm";
      a[1]["e19"]= t1 + "d_yy";
      a[1]["e20"]= t1 + "d_pl";
      a[1]["e21"]= t1 + "_occupation";
      a[1]["e22"]= t1 + "_sex";
      a[1]["e23"]= t1 + "_p";
      a[1]["e24"]= "dp" + t1;
      a[1]["e25"]= t1 + "_occupation";
      a[1]["e26"]= t1 + "_occ";

      for (var i = 10; i <= 23; i++)
      {
        var v0 = document.getElementById(a[0]["e" + i]).value; 
        var v1 = document.getElementById(a[1]["e" + i]).value;
        document.getElementById(a[0]["e" + i]).value = v1;
        document.getElementById(a[1]["e" + i]).value = v0;
      }
      for (var i = 24; i <= 26; i++)
      {
        var v0 = document.getElementById(a[0]["e" + i]).className;
        var v1 = document.getElementById(a[1]["e" + i]).className;
        document.getElementById(a[0]["e" + i]).className = v1;
        document.getElementById(a[1]["e" + i]).className = v0;
      }
      var v0 = document.getElementById(a[0]["e26"]).getAttribute("placeholder","false");
      var v1 = document.getElementById(a[1]["e26"]).getAttribute("placeholder","false");
      document.getElementById(a[0]["e26"]).setAttribute("placeholder",v1,"false");
      document.getElementById(a[1]["e26"]).setAttribute("placeholder",v0,"false");
    }
  }
  function addWitness(z1,z2)
  {
    var c0 = z1;
    var c1 = itemMaxCnt('witn',c0);
    var v1 = document.getElementById("witn1_fn").value;
    var v2 = document.getElementById("witn2_fn").value;
    if(v1 != "" && v2 != "")
    {
      var t0 = "witn" + c0;
      var t1 = "witn" + c1;
      document.getElementById("new_witness").outerHTML = '\
      <tr id="' + t1 + '">\n\
        <td>\n\
          <input type="hidden" id="' + t1 + '_sex" name="' + t1 + '_sex" value="N">\n\
          <input type="hidden" id="' + t1 + '_p" name="' + t1 + '_p" value="create">\n\
          <input id="' + t1 + '_occ" name="' + t1 + '_occ" class="occ2" autocomplete="off" size="5" maxlength="8" value="" placeholder="N" onkeypress="javascript:return oKP2(event,\'witn\',' + c1 + ',\'_occ\')" onblur="oB2(\'' + t1 + '\')">\n\
        <\/td>\n\
        <td><input id="' + t1 + '_fn" name="' + t1 + '_fn" class="ar" size="30" maxlength="200" value="" onblur="tUC1(this)" onkeydown="if(event.keyCode == 13)tUC1(this)"><\/td>\n\
        <td><input id="' + t1 + '_sn" name="' + t1 + '_sn" size="30" maxlength="200" value="" onblur="tUC(this)" onkeydown="if(event.keyCode == 13)tUC(this)"><\/td>\n\
      <\/tr>\n\
      <tr id="new_witness"><\/tr>';
      if(z2 == 0)
      {
        for(var i = c1; i > c0; i--){invertWitness(i);}
        sIV(t0 + "_occ");
      }
      else sIV(t1 + "_occ");
    }
    else
    {
     if(v2 == "")sIV("witn2_occ");
     if(v1 == "")sIV("witn1_occ");
    }
  }
  function invertWitness(cnt)
  {
    if (cnt > 0)
    {
      var t0 = "witn" + (cnt - 1);
      var t1 = "witn" + cnt;
      var a = new Array();
      
      a[0] = new Array();
      a[0]["e10"]= t0 + "_fn";
      a[0]["e11"]= t0 + "_sn";
      a[0]["e12"]= t0 + "_occ";
      a[0]["e13"]= t0 + "_sex";
      a[0]["e14"]= t0 + "_p";
      a[0]["e15"]= t0 + "_occ";
      
      a[1] = new Array();
      a[1]["e10"]= t1 + "_fn";
      a[1]["e11"]= t1 + "_sn";
      a[1]["e12"]= t1 + "_occ";
      a[1]["e13"]= t1 + "_sex";
      a[1]["e14"]= t1 + "_p";
      a[1]["e15"]= t1 + "_occ";

      for (var i = 10; i <= 14; i++)
      {
        var v0 = document.getElementById(a[0]["e" + i]).value;
        var v1 = document.getElementById(a[1]["e" + i]).value;
        document.getElementById(a[0]["e" + i]).value = v1;
        document.getElementById(a[1]["e" + i]).value = v0;
      }
      var v0 = document.getElementById(a[0]["e15"]).className;
      var v1 = document.getElementById(a[1]["e15"]).className;
      document.getElementById(a[0]["e15"]).className = v1;
      document.getElementById(a[1]["e15"]).className = v0;
      var v0 = document.getElementById(a[0]["e15"]).getAttribute("placeholder","false");
      var v1 = document.getElementById(a[1]["e15"]).getAttribute("placeholder","false");
      document.getElementById(a[0]["e15"]).setAttribute("placeholder",v1,"false");
      document.getElementById(a[1]["e15"]).setAttribute("placeholder",v0,"false");
    }
  }
  function cF1(event,id)
  {
    var key = event.keyCode ? event.keyCode : event.which ? event.which : event.charCode;
    var charKey = String.fromCharCode(key);
    if (charKey == "*" || charKey == "_")
    {
      document.getElementById(id).focus();
      return false;
    }
  }
  function oKPdd(event,z1,z2,z3)
  {
    var key = event.keyCode ? event.keyCode : event.which ? event.which : event.charCode;
    var charKey = String.fromCharCode(key);
    var e1 = document.getElementById(z1 + z2);
    var e2 = document.getElementById(z1 + z3);
    var e3 = document.getElementById(z1 + "_mm");
    var e4 = document.getElementById(z1 + "_yy");
    var v = e1.value;
    var v3 = e3.value;
    var v4 = e4.value;
    var l = v.length;
    if (charKey >= 0 && charKey <= 9)
    {
      var ev = eval(v + charKey);
      if (l == 1)
      {
        if (ev > 0 && ev < 32)
        {
          if (ev == 31)
          {
            var r = /02|04|06|09|11/;
            var t = r.test(v3);
            if (t == true)
            {
              e1.value = ""; return false;
            }
            else
            {
              e2.select(); return true;
            }
          }
          else
          {
            if (v3 == "02" && ev > 28)
            {
              if (ev > 29)
              {
                e1.value = ""; return false;
              }
              else
              { 
                if ((v4 != "") && ((v4 < 1582) || ((v4 % 4 != 0) || ((v4 % 100 == 0) && (v4 % 400 != 0)))))
                {
                  e1.value = ""; return false;
                }
                else
                {
                  e2.select(); return true;
                }
              }
            }
            else
            {
              e2.select(); return true;
            }
          }
        }
        else
        {
          e1.value = ""; return false;
        }
      }
    }
    else
    {
      if (key != "8" && key != "37" && key != "39"&& key != "46") return false;
    }
  }
  function oKPmm(event,z1,z2,z3)
  {
    var key = event.keyCode ? event.keyCode : event.which ? event.which : event.charCode;
    var charKey = String.fromCharCode(key);
    var e1 = document.getElementById(z1 + z2);
    var e2 = document.getElementById(z1 + z3);
    var e3 = document.getElementById(z1 + "_dd");
    var e4 = document.getElementById(z1 + "_yy");
    var v = e1.value;
    var v3 = e3.value;
    var v4 = e4.value;
    var l = v.length;
    if (l == 1)
    {
      var s = v + charKey;
      if (isNaN(s) == true)
      {
        var s1 = s.toUpperCase();
        var r1 = /VD|BR|FM|NI|PL|VT|GE|FL|PR|ME|TH|FT|JC/;
        var t1 = r1.test(s1);
        if (t1 == true)
        {
          e1.value = s1;
          e2.select();
          return false;
        }
        else
        {
          e1.value = "";
          return false;
        }
      }
      else
      {
        var ev = eval(s);
        if (ev > 0 && ev < 13)
        {
          if (v3 == 31 && ev != 12)
          {
            var r = /2|4|6|9|11/;
            var t = r.test(ev);
            if (t == true)
            {
              e1.value = ""; return false;
            }
            else
            {
              e2.select(); return true;
            }
          }
          else
          {
            if (ev == 2 && v3 > 28)
            {
              if (v3 > 29)
              {
                e1.value = ""; return false;
              }
              else
              {  
                if ((v4 != "") && ((v4 < 1582) || ((v4 % 4 != 0) || ((v4 % 100 == 0) && (v4 % 400 != 0)))))
                {
                  e1.value = ""; return false;
                }
                else
                {
                  e2.select(); return true;
                }
              }
            }
            else
            {
              e2.select(); return true;
            }
          }
        }
        else
        {
          e1.value = ""; return false;
        }
      }
    }
  }
  function oKU1(event,z1,z2)
  {
    var key = event.keyCode ? event.keyCode : event.which ? event.which : event.charCode;
    var e1 = document.getElementById(z1);
    var e2 = document.getElementById(z2);
    var v = e1.value;
    var l = v.length;
    if (l == 0 && key == "8")
    {
      e2.select();
      return false;
    }
  }
  function cF2(event,z1,z2,z3)
  {
    var key = event.keyCode ? event.keyCode : event.which ? event.which : event.charCode;
    var charKey = String.fromCharCode(key);
    if (charKey == "*" || charKey == "_")
    {
      var n = z2 + 1;
      var t = z1 + z2;
      var id = z1 + z2 + z3;
      var v1 = document.getElementById(t + "_p").value;
      var v2 = document.getElementById("pa2_fn").value;
      var s = v1 + z3;
      if (t == "pa1")
      {
        switch(s)
        {
          case "create_fn": id = "pa1_sn"; break;
          case "create_sn": id = "pa1b_yy"; break;
          case "createb_yy": id = "pa1d_yy"; break;
          default: id = "pa2_fn"; break;
        }
      }
      else
      {
        if (t == "pa2")
        {
          switch(s)
          {
            case "create_fn": id = "pa2_sn"; break;
            case "create_sn": id = "pa2b_yy"; break;
            case "createb_yy": id = "pa2d_yy"; break;
            default: id = "ach"; break;
          }
        }
        else
        {
          switch(s)
          {
            case "create_fn": id = t + "b_yy"; break;
            case "create_sn": id = t + "b_yy"; break;
            case "createb_yy": id = t + "d_yy"; break;
            default: id = "ach"; break;
          }
        }
      }
      document.getElementById(id).focus();
      document.getElementById(id).click();
      return false;
    }
  }
  function evD(z1)
  {
    var e = document.getElementById(z1 + "_yy");
    var e1 = document.getElementById(z1 + "_yyyy");
    var e2 = document.getElementById(z1 + "_prec");
    var e3 = document.getElementById(z1 + "_oryear");
    var e4 = document.getElementById(z1 + "_text");
    var e5 = document.getElementById(z1 + "_dd");
    var e6 = document.getElementById(z1 + "_mm");
    var n = document.getElementsByName("death")[0];
    var v = e.value;
    var v5 = e5.value;
    var v6 = e6.value;
    var r1 = /(^-\d*$|^\d*$)/;
    var t1 = r1.test(v);
    if ((v5 == "29" && v6 == "02") && ((v < 1582) || ((v % 4 != 0) || ((v % 100 == 0) && (v % 400 != 0)))))
    { 
      e.value = ""; return false;
    }
    if(v == "-") {e.value = ""; e1.value = ""; e2.value = ""; e3.value = ""; e4.value = ""; n.options[0].selected = true;}
    else
    {
      if(v != "" && z1 == "death") {n.options[2].selected = true;}
      if(v == "+") {e.value = ""; e1.value = ""; e2.value = ""; e3.value = ""; e4.value = ""; n.options[2].selected = true;}
      else
      {
        if(t1 == true) {e1.value = RegExp.$1; e2.value = "sure"; e3.value = ""; e4.value = "";}
        else
        {
          r1 = /(^\/)(-\d*$|\d*$)/; t1 = r1.test(v);
          if(t1 == true) {e1.value = RegExp.$2; e2.value = "before"; e3.value = ""; e4.value = "";}
          else
          {
            r1 = /(^-\d*|^\d*)(\/$)/; t1 = r1.test(v);
            if(t1 == true) {e1.value = RegExp.$1; e2.value = "after"; e3.value = ""; e4.value = "";}
            else
            {
              r1 = /(^|^\?|^\.|^\.\.)(-\d*|\d*)($|\?$|\.$|\.\.$)/; t1 = r1.test(v);
              if(t1 == true){e.value = "?" + RegExp.$2; e1.value = RegExp.$2; e2.value = "maybe"; e3.value = ""; e4.value = "";}
              else
              {
                r1 = /(^|^\/|^\/\/)(-\d*|\d*)($|\/$|\/\/$)/; t1 = r1.test(v);
                if(t1 == true){e.value = "/" + RegExp.$2 + "/"; e1.value = RegExp.$2; e2.value = "about"; e3.value = ""; e4.value = "";}
                else
                {
                  r1 = /(^-\d*|^\d*)(\+|-)(\d*$)/; t1 = r1.test(v);
                  if(t1 == true)
                  {
                    var ev = eval(v);
                    e.value = "/" + ev + "/"; e1.value = ev; e2.value = "about"; e3.value = ""; e4.value = "";}
                  else
                  {
                    r1 = /(^-\d*|^\d*)(\/)(-\d*$|\d*$)/; t1 = r1.test(v);
                    if(t1 == true){e1.value = RegExp.$1; e2.value = "oryear"; e3.value = RegExp.$3; e4.value = "";}
                    else
                    {
                      r1 = /(^-\d*|^\d*)(\.|\.\.)(-\d*$|\d*$)/; t1 = r1.test(v);
                      if(t1 == true){e.value = RegExp.$1 + ".." + RegExp.$3; e1.value = RegExp.$1; e2.value = "yearint"; e3.value = RegExp.$3; e4.value = "";}
                      else {e1.value = ""; e2.value = ""; e3.value = ""; e4.value = v;} 
                    } 
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  function evSD(z1)
  {
    var e = document.getElementById(z1 + "_yy");
    var e1 = document.getElementById(z1 + "_dd");
    var e2 = document.getElementById(z1 + "_mm");
    var v = e.value;
    var v1 = e1.value;
    var v2 = e2.value;
    var r1 = /(^-\d*$|^\d*$)/;
    var t1 = r1.test(v);
    if ((v1 == "29" && v2 == "02") && ((v < 1582) || ((v % 4 != 0) || ((v % 100 == 0) && (v % 400 != 0)))))
    { 
      e.value = ""; return false;
    }
    if(v == "-" || v == "+"){}
    else
    {
      if(t1 == true) {e.value = RegExp.$1;}
      else
      {
        r1 = /(^\/)(-\d*$|\d*$)/; t1 = r1.test(v);
        if(t1 == true) {e.value = "/" + RegExp.$2}
        else
        {
          r1 = /(^-\d*|^\d*)(\/$)/; t1 = r1.test(v);
          if(t1 == true) {e.value = RegExp.$1 + "/"}
          else
          {
            r1 = /(^|^\?|^\.|^\.\.)(-\d*|\d*)($|\?$|\.$|\.\.$)/; t1 = r1.test(v);
            if(t1 == true) {e.value = "?" + RegExp.$2}
            else
            {
              r1 = /(^|^\/|^\/\/)(-\d*|\d*)($|\/$|\/\/$)/; t1 = r1.test(v);
              if(t1 == true) {e.value = "/" + RegExp.$2 + "/"}
              else
              {
                r1 = /(^-\d*|^\d*)(\+|-)(\d*$)/; t1 = r1.test(v);
                if(t1 == true) {var ev = eval(v); e.value = "/" + ev + "/"}
                else e.value = "";
              }
            }
          }
        }
      }
    }
  }
  function visHid(z1,z2)
  {
    var e1 = document.getElementById(z1 + "_hid"); 
    var e2 = document.getElementById(z1 + "_vis");
    var s = e1.style.display;
    var i = 1;
    
    if (s == "none")
    {
      e1.style.display = "inline"; e2.style.display = "none";
      while (document.getElementById(z1 + i + z2))
      {document.getElementById(z1 + i + z2).type = "text";i++}
    }
    else
    {
      e1.style.display = "none"; e2.style.display = "inline";
      while (document.getElementById(z1 + i + z2))
      {document.getElementById(z1 + i + z2).type = "hidden";i++}
    }
  }
  function addS(z1,z2,z3)
  {
    switch(z1)
    {
      case "ch": addChild(z2,z3); break;
      case "witn": addWitness(z2,z3); break;
      case "r": addRelation(z2,z3); break;
      case "t": addTitle(z2,z3); break;
    }
  }
  function invertS(z1,z2,z3,z4,z5)
  { 
    var n = Number(z2);
    var i1 = (z4 == 0) ? n: n + 1;
    var i2 = (z4 == 0) ? n - 1: n + 1;
    var e1 = (document.getElementById(z1 + i1 + z3)) ? document.getElementById(z1 + i1 + z3) : "";
    var e2 = (document.getElementById(z1 + i2 + z3)) ? document.getElementById(z1 + i2 + z3) : "";
    if (e1 != "" && e2 != "")
    {
      if (z5 == 1)
      {
        switch(z1)
        {
          case "ch": invertChild(i1); break;
          case "witn": invertWitness(i1); break;
          case "r": invertRelation(i1); break;
          case "t": invertTitle(i1); break;
        }
      }
      document.getElementById(z1 + i2 + z3).focus();
    }
  }
  function oB1()
  {
    var e1 = document.getElementById("sex1");
    var e2 = document.getElementById("sex");
    var v = e1.value;
    switch(v)
    {
      case "M": e1.className='occ0'; e2.value = "M"; e1.setAttribute("placeholder","M","false"); break;
      case "F": e1.className='occ1'; e2.value = "F"; e1.setAttribute("placeholder","F","false"); break;
      case "N": e1.className='occ2'; e2.value = "N"; e1.setAttribute("placeholder","N","false"); break;
    }
    e1.value="";
  }
  function oB2(z1)
  {
    var e1 = document.getElementById(z1 + "_sex");
    var e2 = document.getElementById(z1 + "_p");
    var e3 = document.getElementById(z1 + "_occ");
    var e4 = (document.getElementById("dp" + z1)) ? document.getElementById("dp" + z1) : "";
    var e5 = (document.getElementById(z1 + "_occupation")) ? document.getElementById(z1 + "_occupation") : "";
    var v = e3.value;
    var r = /(^\d*$)/;
    var t = r.test(v);
    if(v != "" && t == true)
    {
      e3.className='occ'; e1.value = ""; e2.value = "link";
      if(e4 != ""){e4.className='hid'; e5.className='hid';}
    }
    else
    {
      switch(v)
      {
        case "M": e3.className='occ0'; e1.value = "M"; e3.setAttribute("placeholder","M","false"); break;
        case "F": e3.className='occ1'; e1.value = "F"; e3.setAttribute("placeholder","F","false"); break;
        case "N": e3.className='occ2'; e1.value = "N"; e3.setAttribute("placeholder","N","false"); break;
      }
      e2.value = "create"; e3.value="";
      if(e4 != ""){e4.className='vis'; e5.className='vis';}
    }
  }
  function oB3(z1)
  {
    var e1 = document.getElementById(z1 + "_p");
    var e2 = document.getElementById(z1 + "_occ");
    var v = e2.value;
    var v1 = e2.getAttribute("placeholder","false");
    var r = /(^\d*$)/;
    var t = r.test(v);
    if(v != "" && t == true)e1.value = "link"; else {e1.value = "create"; e2.value = ""; e2.setAttribute("placeholder",v1,"false");}
  }
  function oB4(z1)
  {
    var e1 = document.getElementById(z1);
    var e2 = document.getElementById(z1 + "_p");
    var e3 = document.getElementById(z1 + "_occ");
    var e4 = document.getElementById("dp" + z1);
    var e5 = document.getElementById(z1 + "_occupation");
    var v = e3.value;
    var v1 = e3.getAttribute("placeholder","false");
    var r = /(^\d*$)/;
    var t = r.test(v);
    if(v != "" && t == true)
    {
      e2.value = "link"; e4.className='hid'; e5.className='hid';
    }
    else
    {
      e2.value = "create"; e3.value = ""; e4.className='vis'; e5.className='vis';
    }
  }
  var Tim1;
  function oB5()
  {
    clearTimeout(Tim1);
  }
  function oF5(z1)
  {
    clearTimeout(Tim1);
    Tim1 = setTimeout(function(){z1.click();},500);
  }
  function fillPlaceFam(xx)
  {
    switch(xx.value)
    {
      case "1": xx.value = document.upd.pa1b_pl.value; break;
      case "2": xx.value = document.upd.pa1d_pl.value; break;
      case "3": xx.value = document.upd.pa2b_pl.value; break;
      case "4": xx.value = document.upd.pa2d_pl.value; break;
      case "*": xx.value = document.upd.marr_place.value; break;
      case "0": xx.value = document.upd.ch1b_pl.value; break;
      case "+": xx.value = document.upd.ch1d_pl.value; break;
    }
  }
  function fillPlaceInd(xx)
  {
    switch(xx.value)
    {
      case "2": xx.value = document.upd.birth_place.value;break;
      case "3": xx.value = document.upd.death_place.value;break;
    }
  }
  function dsp0(z1)
  {
    document.getElementById(z1 + "0").style.display = "none";
    document.getElementById(z1 + "1").style.display = "block";
    document.getElementById(z1 + "1a").focus();
  }
  function dsp1(z1)
  {
    document.getElementById(z1 + "0").style.display = "block";
    document.getElementById(z1 + "1").style.display = "none";
    document.getElementById(z1 + "0a").focus();
  }
  function sIV(id)
  {
    var e = document.getElementById(id);
    e.scrollIntoView("true");
    window.scrollBy(0,-100);
    e.select();
  }