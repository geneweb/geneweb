function copyToClipboard(text) {
    if (window.clipboardData && window.clipboardData.setData) {
        return clipboardData.setData("Text", text);

    } else if (document.queryCommandSupported && document.queryCommandSupported("copy")) {
        var textarea = document.createElement("textarea");
        textarea.textContent = text;
        textarea.style.position = "fixed";
        document.body.appendChild(textarea);
        textarea.select();
        try {
            return document.execCommand("copy");
        } catch (ex) {
            console.warn("Copy to clipboard failed.", ex);
            return false;
        } finally {
            document.body.removeChild(textarea);
        }
    }
}

    var simplecopybtn = document.querySelector(".simple-copy");
    var simpledata = simplecopybtn.dataset.wikilink;
    var fullcopybtn = document.querySelector(".full-copy");
    var fulldata = fullcopybtn.dataset.wikilink;

    simplecopybtn.addEventListener("click", function(event) {
    var result = copyToClipboard(simpledata);
});
    fullcopybtn.addEventListener("click", function(event) {
    var result = copyToClipboard(fulldata);
});

    var permacopybtn = document.querySelector(".permalink-copy");
    var permafcopybtn = document.querySelector(".permalink-friend-copy");
    var permawcopybtn = document.querySelector(".permalink-wizard-copy");
    var query = permacopybtn.dataset.query;
    var bname = permacopybtn.dataset.bname;
    var origin = window.location.origin;
    var permaurl = origin + (bname != "" ? "/" + bname : window.location.pathname) + "?" + query;
    var permaurlf = origin + "/" + bname + "_f?" + query;
    var permaurlw = origin + "/" + bname + "_w?" + query;

    permacopybtn.addEventListener("click", function(event) {
    var result = copyToClipboard(permaurl);
});
    permafcopybtn.addEventListener("click", function(event) {
    var result = copyToClipboard(permaurlf);
});
    permawcopybtn.addEventListener("click", function(event) {
    var result = copyToClipboard(permaurlw);
});