function copyToClipboard(text) {
  if (navigator.clipboard && navigator.clipboard.writeText) {
    navigator.clipboard.writeText(text).catch(function(err) {
      console.warn("Clipboard write failed.", err);
    });
    return;
  }
  // Fallback legacy
  var textarea = document.createElement("textarea");
  textarea.textContent = text;
  textarea.style.position = "fixed";
  document.body.appendChild(textarea);
  textarea.select();
  try {
    document.execCommand("copy");
  } catch (ex) {
    console.warn("Copy to clipboard failed.", ex);
  } finally {
    document.body.removeChild(textarea);
  }
}

// simple-copy / full-copy (optionnels)
var simplecopybtn = document.querySelector(".simple-copy");
var fullcopybtn   = document.querySelector(".full-copy");

if (simplecopybtn) {
  simplecopybtn.addEventListener("click", function() {
    copyToClipboard(simplecopybtn.dataset.wikilink);
  });
}
if (fullcopybtn) {
  fullcopybtn.addEventListener("click", function() {
    copyToClipboard(fullcopybtn.dataset.wikilink);
  });
}

// permalink buttons — délégation sur document pour couvrir menubar ET copyright
document.addEventListener("click", function(event) {
  var btn = event.target.closest(".permalink-copy, .permalink-friend-copy, .permalink-wizard-copy");
  if (!btn) return;

  var permacopybtn = document.querySelector(".permalink-copy");
  var query  = permacopybtn.dataset.query;
  var bname  = permacopybtn.dataset.bname;
  var origin = window.location.origin;

  var permaurl  = origin + (bname !== "" ? "/" + bname   : window.location.pathname) + "?" + query;
  var permaurlf = origin + "/" + bname + "_f?" + query;
  var permaurlw = origin + "/" + bname + "_w?" + query;

  if (btn.classList.contains("permalink-friend-copy")) {
    copyToClipboard(permaurlf);
  } else if (btn.classList.contains("permalink-wizard-copy")) {
    copyToClipboard(permaurlw);
  } else {
    copyToClipboard(permaurl);
  }
});