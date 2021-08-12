/* $Id: js.js,v 7.00 2015/03/08 22:24:48 mr Exp $ */

  function adv(z1,z2)
  {
    var v = z2.firstChild.nodeValue;
    var h = encodeURI(z1 + v);
    window.open(h);
  }
  function adv1(z1,z2)
  {
    var v = z2.firstChild.nodeValue;
    var h = encodeURI(z1 + v + "ss");
    window.open(h);
  }
  function changeDate(add,del,text,i0,i1,e1,i2,e2)
  {
    var v0 = document.getElementById(i0);
    var v1 = document.getElementById(i1);
    var v2 = document.getElementById(i2);
    if (v1.value == '' && v2.value == '')
    { 
      v0.firstChild.nodeValue = del + text;
      v1.value = e1;
      v2.value = e2;
    }
    else
    { 
      v0.firstChild.nodeValue = add + text;
      v1.value = '';
      v2.value = '';
    }
  }
  function computeSosa(event,id)
  {
    var x = document.getElementById(id);
    var key = event.keyCode ? event.keyCode : event.which ? event.which : event.charCode;
    var string = String.fromCharCode(key);
    
    if (x.value > 0)
    {
      switch (string)
      {
        case "*" : x.value = x.value * 2; return false; break;
        case "/" : if (x.value != '1') {x.value = parseInt(x.value / 2)}; return false; break;
        case "+" : x.value = x.value * 2 + 1; return false; break;
        case "-" : x.value = parseInt(x.value / 2) + 1; return false; break;
      }
    }
  }
  function deleted(x)
  {
    x.setAttribute("style","color:#C0C0C0;font-style:italic;","false");
  }
  function insertTags (tagOpen, tagClose, sampleText)
  {
    var t = document.forms[0].notes;
    // IE
    if(document.selection)
    {
      var theSelection = document.selection.createRange().text;
      if(!theSelection) { theSelection=sampleText;}
      t.focus();
      if(theSelection.charAt(theSelection.length - 1) == " ")
      {// exclude ending space char, if any
        theSelection = theSelection.substring(0, theSelection.length - 1);
        document.selection.createRange().text = tagOpen + theSelection + tagClose + " ";
      }
      else
      {
        document.selection.createRange().text = tagOpen + theSelection + tagClose;
      }
    }
    else
    {
      if(t.selectionStart || t.selectionStart == '0')
      {
        var replaced = false;
        var startPos = t.selectionStart;
        var endPos = t.selectionEnd;
        if(endPos-startPos) replaced=true;
        var scrollTop=t.scrollTop;
        var myText = (t.value).substring(startPos, endPos);
        if(!myText) {myText=sampleText;}
        if(myText.charAt(myText.length - 1) == " ")
        { // exclude ending space char, if any
          subst = tagOpen + myText.substring(0, (myText.length - 1)) + tagClose + " ";
        }
        else
        {
          subst = tagOpen + myText + tagClose;
        }
        t.value = t.value.substring(0, startPos) + subst +
                  t.value.substring(endPos, t.value.length);
        t.focus();
        //set new selection
        if(replaced)
        {
          var cPos=startPos+(tagOpen.length+myText.length+tagClose.length);
          t.selectionStart=cPos;
          t.selectionEnd=cPos;
        }
        else
        {
          t.selectionStart=startPos+tagOpen.length;
          t.selectionEnd=startPos+tagOpen.length+myText.length;
        }
        t.scrollTop=scrollTop;
      }
    }
    if (t.createTextRange) t.caretPos = document.selection.createRange().duplicate();
    return false;
  }
  function valid(id)
  {
    document.getElementById(id).focus();
    document.getElementById(id).checked = true;
    document.getElementById(id).click();
  }
  function wikiPlace(z1,z2,z3)
  {
    var v = z3;
    var v1 = v.replace(/, /g,",");
    var v2 = v1.replace(/ /g,"+");
    var s = v2.split(",");
    var l = s.length;
    for (var i = 0; i < l; i++)
    {
      if (s[i] != "")
      {
        var e1 = document.createElement("li");
        var e2 = document.createElement("a");
        var a1 = document.createAttribute("href");
        a1.nodeValue = "http:\/\/" + z2 + ".wikipedia.org\/wiki\/Special:Search?search=" + s[i];
        e2.setAttributeNode(a1);
        var t1 = document.createTextNode("http:\/\/" + z2 + ".wikipedia.org\/wiki\/Special:Search?search=" + s[i]);
        e1.appendChild(e2);
        e2.appendChild(t1);
        document.getElementById(z1).appendChild(e1);
      }
    }
  }