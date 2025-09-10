/* $Id: js_upd_jq.js,v 7.00 2016/01/26 10:49:18 mr Exp $ */

var ldlLen = 0;
var ldlTim;
var ldlVal = "";
function ldl(z1,z2,z3)
{
  var a1 = "o" + z2;
  var a2 = z1.value.length;
  var a3 = document.getElementById(a1).value;
  var a4 = z1.value.slice(0,3);
  var a5 = z1.value.charAt(a2 - 1).toUpperCase();
  var key = z3.keyCode ? z3.keyCode : z3.which ? z3.which : z3.charCode;
  var charKey = String.fromCharCode(key).toUpperCase();
  if(a3 > 20 && ldlVal == a4)ldlVal = "";
  if(ldlVal != a4 && a2 != ldlLen && a2 > 2 && charKey == a5)
  {
    clearTimeout(ldlTim);
    ldlVal = a4;
    ldlLen = a2;
    ldlTim = setTimeout(
    function()
    {
      var b1 = encodeURI(z1.value);
      var b2 = "#dl" + z2;
      var b3 = g_prefix + "m=MOD_DATA;data=" + z2 + ";s=" + b1 + ";datalist=on;";
      $(b2).load(b3);
    },1000);
  }
}
function jq1(z1,z2)
{
  var e7 = document.getElementById(z1 + "_occ");
  var v7 = e7.value;
  var e8 = document.getElementById(z1 + "_fn");
  var v8 = e8.value;
  if (v7 != "" || v8 != "")
  {
    var e1 = document.getElementById(z1 + "_jq1");
    var e9 = document.getElementById(z1 + "_sn");
    var e10 = (document.getElementById(z1 + "_i")) ? document.getElementById(z1 + "_i"): "";
    var v9 = e9.value;
    var u8 = encodeURI(v8);
    var u9 = encodeURI(v9);
    t1 = v7 + "." + u8 + "." + u9;
    if (e1.title != t1)
    {
      e1.innerHTML = "";
      e1.title = t1;
      if (v7 != "")
      {
        var l = (u8 == "" || u8 == "?" || u9 == "?") ? g_prefix + "m=U;jq1=on;i=" + v7: g_prefix + "m=U;jq1=on;oc=" + v7 + ";p=" + u8 + ";n=" + u9;
        $("#jq").load(l, function()
        {
          if(document.getElementById("jql"))
          {
            e1.innerHTML = document.getElementById("jql").firstChild.data;
            e7.value = document.getElementById("jq7").firstChild.data;
            e8.value = document.getElementById("jq8").firstChild.data;
            e9.value = document.getElementById("jq9").firstChild.data;
            if (e10 != "") e10.value = document.getElementById("jq10").firstChild.data;
            var d11 = document.getElementById("jq11").firstChild.data;
            if (z2 != "" && z2 != d11)
            {
              e7.value = ""; e8.value = ""; e9.value = "";
              if (e10 != "") e10.value = "";
              e1.innerHTML = e7.getAttribute("placeHolder","false") + " !";
              e7.focus();
            }
          }
          else {e1.innerHTML = " ??? ";}
        })
      }
      else
      {
        var l = g_prefix + "m=U;oc=0;p=" + u8 + ";n=" + u9 + ";jq1=on";
        $("#jq").load(l, function()
        {
          if(document.getElementById("jql"))
          {
            var l1 = g_prefix + "m=S;p="+ u8 +";n=" + u9;
            e1.innerHTML = ' <a onclick="window.open(this.href); return false;" href="'+ l1 +'">&lt;!&gt;<\/a>';
          }
        })
      }
    }
  }
}
function jq1a(z1)
{
  var e7 = document.getElementById(z1 + "_occ");
  var v7 = e7.value;
  var e8 = document.getElementById(z1 + "_fn");
  var v8 = e8.value;
  if (v7 != "" || v8 != "")
  {
    var e1 = document.getElementById(z1 + "_jq1");
    var e2 = document.getElementById(z1 + "_jq2");
    var e3 = document.getElementById(z1 + "_jq3");
    var e4 = document.getElementById(z1 + "_jq4");
    var e5 = document.getElementById(z1 + "b_pl");
    var e6 = document.getElementById(z1 + "d_pl");
    var e9 = document.getElementById(z1 + "_sn");
    var v9 = (e9.value == "") ? document.getElementById("pa1_sn").value : e9.value;
    var u8 = encodeURI(v8);
    var u9 = encodeURI(v9);
    t1 = v7 + "." + u8 + "." + u9;
    if (e1.title != t1 && (u8 != "?" || u9 != "?"))
    {
      e1.innerHTML = "";
      e1.title = t1;
      e2.innerHTML = "";
      e3.innerHTML = "";
      e4.innerHTML = "";
      if (v7 != "")
      {
        var l = (u8 == "" || u8 == "?" || u9 == "?") ? g_prefix + "m=U;jq1a=on;i=" + v7: g_prefix + "m=U;jq1a=on;oc=" + v7 + ";p=" + u8 + ";n=" + u9;
        $("#jq").load(l, function()
        {
          if(document.getElementById("jql"))
          {
            var d11 = document.getElementById("jq11").firstChild.data;
            var v12 = document.getElementById("nsck").checked;
            if (((z1 == "pa1" && d11 != "0") || (z1 == "pa2" && d11 != "1")) && v12 == false)
            {
              e7.value = ""; e8.value = ""; e9.value = "";
              e1.innerHTML = e7.getAttribute("placeHolder","false") + " !";
              e7.focus();
            }
            else
            {
              if (z1 == "pa1" || z1 == "pa2") e7.className = "occ" + d11;
              e2.innerHTML = document.getElementById("jq2").firstChild.data;
              e3.innerHTML = document.getElementById("jq3").firstChild.data;
              e4.innerHTML = document.getElementById("jq4").firstChild.data;
              e5.value = document.getElementById("jq5").firstChild.data;
              e6.value = document.getElementById("jq6").firstChild.data;
              e7.value = document.getElementById("jq7").firstChild.data;
              e8.value = document.getElementById("jq8").firstChild.data;
              e9.value = document.getElementById("jq9").firstChild.data;
            }
          }
          else{e1.innerHTML = "???";}
        })
      }
      else
      {
        var l = g_prefix + "m=U;oc=0;p=" + u8 + ";n=" + u9 + ";jq1a=on";
        $("#jq").load(l, function()
        {
          if(document.getElementById("jql"))
          {
            var l1 = g_prefix + "m=S;p="+ u8 +";n=" + u9;
            e1.innerHTML = ' <a onclick="window.open(this.href); return false;" href="'+ l1 +'">&lt;!&gt;<\/a>';
          }
        })
      }
    }
  }
}
function jq1b1(z1)
{
    var e1 = document.getElementById(z1 + "_jq1");
    var e2 = document.getElementById(z1 + "_jq2");
    var e3 = document.getElementById(z1 + "_jq3");
    var e4 = document.getElementById(z1 + "_jq4");
    var e5 = document.getElementById(z1 + "b_pl");
    var e6 = document.getElementById(z1 + "d_pl");
    var e7 = document.getElementById(z1 + "_occ");
    var v7 = e7.value;
    var e8 = document.getElementById(z1 + "_fn");
    var v8 = e8.value;
    var e9 = document.getElementById(z1 + "_sn"); 
    var v9 = e9.value;
    var u8 = encodeURI(v8);
    var u9 = encodeURI(v9);
    t1 = v7 + "." + u8 + "." + u9;
    e1.title = t1;
    e2.innerHTML = document.getElementById(z1 + "jq2").firstChild.data;
    e3.innerHTML = document.getElementById(z1 + "jq3").firstChild.data;
    e4.innerHTML = document.getElementById(z1 + "jq4").firstChild.data;
    e5.value = document.getElementById(z1 + "jq5").firstChild.data;
    e6.value = document.getElementById(z1 + "jq6").firstChild.data;
    e7.value = document.getElementById(z1 + "jq7").firstChild.data;
    e8.value = document.getElementById(z1 + "jq8").firstChild.data;
    e9.value = document.getElementById(z1 + "jq9").firstChild.data;
    if (z1 == "pa1" || z1 == "pa2")
    {
      var d11 = document.getElementById(z1 + "jq11").firstChild.data;
      e7.className = "occ" + d11;
    }
}
function jq1b(z1)
{
  var e8 = document.getElementById("pa1_fn");
  var v8 = e8.value;
  if (v8 != "")
  {
    var e7 = document.getElementById("pa1_occ");
    var v7 = e7.value;
    var e9 = document.getElementById("pa1_sn");
    var v9 = e9.value;
    var u8 = encodeURI(v8);
    var u9 = encodeURI(v9);
    if (u8 != "?" || u9 != "?")
    {
      var l = g_prefix + "m=U;jq1b=on;ifam=" + z1 + ";oc=" + v7 + ";p=" + u8 + ";n=" + u9;
      $("#jq").load(l, function()
      {
        if(document.getElementById("jql"))
        {
          if(document.getElementById("pa1jq7"))jq1b1("pa1");
          if(document.getElementById("pa2jq7"))jq1b1("pa2");
          var i = 1;
          while(document.getElementById("ch" + i + "jq7"))
          {
            jq1b1("ch" + i);
            i++;
          }
        }
      })
    }
  }
  else
  {
    if (document.getElementById("pa2_fn").value != "")jq1a('pa2');
  }
}