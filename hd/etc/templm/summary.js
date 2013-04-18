/* summary.js mickroue(a)yahoo.fr 20130211 templ=templm */
/* This code has been borrowed from Wikipedia */
function showTocToggle()
{
  show="[visualize/show/hide/summary]1";
  hide="[visualize/show/hide/summary]2";
  if (document.getElementById)
  {
    show_disp=' style="display:none;"';
    hide_disp='';
    document.writeln('<span class="toctoggle">' +
    '(<a href="javascript:toggleToc()" class="internal">' +
    '<span id="showlink"' + show_disp + '>' + show + '</span>' +
    '<span id="hidelink"' + hide_disp + '>' + hide + '</span>'
    + '</a>)</span>');
  }
}
function setTocToggle()
{
  var cookiePos = document.cookie.indexOf("hidetoc=");
  if (cookiePos > -1 && document.cookie.charAt(cookiePos + 8) == 1)
    toggleToc();
}
function toggleToc() {
  var toc = document.getElementById('tocinside');
  var showlink = document.getElementById('showlink');
  var hidelink = document.getElementById('hidelink');
  if (toc.style.display == 'none')
  {
    toc.style.display = tocWas;
    hidelink.style.display='';
    showlink.style.display='none';
    document.cookie = "hidetoc=0";
  }
  else
  {
    tocWas = toc.style.display;
    toc.style.display = 'none';
    hidelink.style.display='none';
    showlink.style.display='';
    document.cookie = "hidetoc=1";
  }
}