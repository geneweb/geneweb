/* upd.js mickroue(a)yahoo.fr 20130211 templ=templm */
  function addRow(event,id)
  {
    var key = event.keyCode ? event.keyCode : event.which ? event.which : event.charCode;
    if (key == 49 || key == 13)
    {
      document.getElementById(id).checked = true;
      document.upd.submit()
    }
  }
  function changeDisplay(id,c,cond1,cond2)
  {
    var d = document.getElementById(id);
    var val = c.options[c.selectedIndex].value;
    if (val == cond1 || val == cond2)
      {d.style.display='inline';}
    else
      {d.style.display='none';}
  }
  function changeFocus(event,id)
  {
    var key = event.keyCode ? event.keyCode : event.which ? event.which : event.charCode;
    var charKey = String.fromCharCode(key);
    if (charKey == "*" || charKey == "_")
    {
      document.getElementById(id).focus();
      return false;
    }
  }
  function changeFocusDeath(event)
  {
    var d1 = document.getElementById('content_death');
    var d2 = document.getElementById('content_burial');
    var d3 = document.getElementById('content_death_reason');
    var id = document.getElementById('death');
    var key = event.keyCode ? event.keyCode : event.which ? event.which : event.charCode;
    var charKey = String.fromCharCode(key);
    if (charKey == "*" || charKey == "+" || charKey == "_")
    {
      id.value = "Death";
      d1.style.display='inline';
      d2.style.display='inline';
      d3.style.display='inline';
      document.getElementById('death_d1').focus();
      return false;
    }
  }
  function changeFocusFn(event,id,id1,id2)
  {
    var key = event.keyCode ? event.keyCode : event.which ? event.which : event.charCode;
    var charKey = String.fromCharCode(key);
    if (charKey == "*" || charKey == "_")
    {
      if (document.getElementById(id).checked == true)
      {
        document.getElementById(id2).focus();
        return false;
      }
      else
      {
        document.getElementById(id1).focus();
        return false;
      }
    }
  }
  function compute(id,prec)
  {
    var x = document.getElementById(id);
    var y = document.getElementById(prec);
    if (x.value != '' && x.value != eval(x.value))
    {
      x.value = eval(x.value);
      y.value = 'about'
    }
  }
  function computeSD(id)
  {
    var x = document.getElementById(id);
    if (x.value != '' && x.value != eval(x.value))
    {
      x.value = '/' + eval(x.value) + '/';
    }
  }
  function female(id)
  {
    document.getElementById(id).className='background_sex_1';
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
      case "3": xx.value = document.upd.bapt_place.value;break;
      case "4": xx.value = document.upd.death_place.value;break;
      case "5": xx.value = document.upd.burial_place.value;break;
    }
  }

  
  function linkCreateVisible(xx,yy,zz)
  {
    if(xx == 'pa')
    {
      document.getElementById('pa' + yy + '_6').style.visibility=zz;
    }
    if(xx == 'witn')
    {
      document.getElementById('witn' + yy + '_5').style.visibility=zz;
    }
    if(xx == 'ch')
    {
      document.getElementById('ch' + yy + '_5').style.visibility=zz;
      document.getElementById('ch' + yy + '_6').style.visibility=zz;
    }
  }
  function male(id)
  {
    document.getElementById(id).className='background_sex_0';
  }
  function no_sex(id)
  {
    document.getElementById(id).className='background_sex_2';
  }
  function orYear(event,id1,id2,id3)
  {
    var key = event.keyCode ? event.keyCode : event.which ? event.which : event.charCode;
    var charKey = String.fromCharCode(key);
    var x1 = document.getElementById(id1);
    var x2 = document.getElementById(id2);
    var x3 = document.getElementById(id3);
    if (charKey == "." || charKey == ":")
    {
      x1.value = 'yearint';
      x2.style.display='inline';
      x3.focus();
      return false;
    }
    if (charKey == ";")
    {
      x1.value = 'oryear';
      x2.style.display='inline';
      x3.focus();
      return false;
    }
  }
  function setDead(c) 
  {
    var doc = document.upd;
    var d1 = document.getElementById('content_death');
    var d2 = document.getElementById('content_burial');
    var d3 = document.getElementById('content_death_reason');
    var val = c.options[c.selectedIndex].value;
      if (val == 'Auto' || val == 'NotDead' || val == 'DontKnowIfDead' || val == 'OfCourseDead')
      {
        d1.style.display='none';
        d2.style.display='none';
        d3.style.display='none';
        doc.burial.value = "UnknownBurial";
        doc.burial_dd.value = "";
        doc.burial_mm.value = "";
        doc.burial_yyyy.value = "";
        doc.burial_prec.value = "-";
        doc.burial_oryear.value = "";
        doc.burial_text.value = "";
        doc.burial_place.value = "";
        doc.burial_src.value = "";
      }
      else
      {
        d1.style.display='inline';
        d2.style.display='inline';
        d3.style.display='inline';
      }
  }