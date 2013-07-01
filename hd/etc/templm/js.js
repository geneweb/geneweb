/* js.js mickroue(a)yahoo.fr 20130624 templ=templm */

  function adv(z1,z2)
  {
    var x2 = z2.innerHTML;
    parent.location.href = z1 + x2;
  }
  function adv1(z1,z2)
  {
    var x2 = z2.innerHTML;
    var x3 = x2.charAt(0)
    parent.location.href = z1 + x3 + x2;
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
  function changeTri(xx)
  {
    var labelTriA = document.getElementById('labelTriA')
    var triA = document.getElementById('triA')
    var mNG = document.getElementById('mNG')
    var tP = document.getElementById('tP')
    var tN = document.getElementById('tN')
    var n = document.getElementById('n')
    triA.style.visibility=xx;
    labelTriA.style.visibility=xx;
    if(xx == 'visible' && triA.checked == true)
    {
      if(tP.checked == true) mNG.value='P';
      if(tN.checked == true) mNG.value='N';
      n.name='k';
    }
    if(xx == 'hidden' || triA.checked == false)
    {
      mNG.value='NG';
      n.name='n';
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
        case "/" : x.value = parseInt(x.value / 2); return false; break;
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
  function TchangeTri(xx) {
     var TlabelTriA = document.getElementById('TlabelTriA')
     var TtriA = document.getElementById('TtriA')
     var TmNG = document.getElementById('TmNG')
     var TtP = document.getElementById('TtP')
     var TtN = document.getElementById('TtN')
     var Tn = document.getElementById('Tn')
     
     TtriA.style.visibility=xx;
     TlabelTriA.style.visibility=xx;
     if(xx == 'visible' && TtriA.checked == true)
    {
      if(TtP.checked == true) TmNG.value='P';
      if(TtN.checked == true) TmNG.value='N';
      Tn.name='k';
    }
     if(xx == 'hidden' || TtriA.checked == false)
    {
     TmNG.value='NG';
     Tn.name='n';
    }
  }
  function valid(id)
  {
    document.getElementById(id).focus();
    document.getElementById(id).checked = true;
  }