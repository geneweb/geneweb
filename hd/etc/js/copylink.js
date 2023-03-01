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
    var permacopybtn = document.querySelector(".permalink-copy");
    var permafcopybtn = document.querySelector(".permalink-friend-copy");
    var permawcopybtn = document.querySelector(".permalink-wizard-copy");
    var permadata = permacopybtn.dataset.wikilink;
    var cururl = window.location;
    var permaurl = cururl.origin + "/" + permadata;
    var permaurlf = permaurl + "&w=f";
    var permaurlw = permaurl + "&w=w";

    simplecopybtn.addEventListener("click", function(event) {
    var result = copyToClipboard(simpledata);
    console.log("copied?", result);
});

    fullcopybtn.addEventListener("click", function(event) {
    var result = copyToClipboard(fulldata);
    console.log("copied?", result);
});

    permacopybtn.addEventListener("click", function(event) {
    var result = copyToClipboard(permaurl);
    console.log("copied?", result);
});

    permafcopybtn.addEventListener("click", function(event) {
    var result = copyToClipboard(permaurlf);
    console.log("copied?", result);
});

    permawcopybtn.addEventListener("click", function(event) {
    var result = copyToClipboard(permaurlw);
    console.log("copied?", result);
});