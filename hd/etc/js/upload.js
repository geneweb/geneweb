// Upload zone of the carrousel page (m=SND_IMAGE_C).
// Files dropped on the zone, pasted (Ctrl+V) or picked wait on a wall where
// the user chooses their destination and name, then they are sent one by one
// to m=UPLOAD_OK. Needs dropzone-min.js. The configuration and the messages
// come from the data-* attributes of #upload-zone.
(function () {
  "use strict";

  const zone = document.getElementById("upload-zone");
  if (!zone || !window.Dropzone) return;

  const cfg = zone.dataset;
  const wall = document.getElementById("upload-wall");
  const sendBtn = document.getElementById("upload-send");
  const finishBtn = document.getElementById("upload-finish");
  const maxImage = Number(cfg.maxImage);
  const maxDocument = Number(cfg.maxDocument);
  const units = cfg.units.split("/");

  const IMAGE_EXT = [".jpg", ".png", ".gif"];
  // Extensions that the server removes from a typed name.
  const TYPED_EXT = IMAGE_EXT.concat([".jpeg", ".pdf", ".docx", ".odt", ".txt"]);

  let sending = false;
  let subdirTimer = null;

  // ── Names ───────────────────────────────────────────────────────────────

  function rawExtension(name) {
    const dot = name.lastIndexOf(".");
    return dot > 0 ? name.slice(dot).toLowerCase() : "";
  }

  function withoutExtension(name) {
    const dot = name.lastIndexOf(".");
    return dot > 0 ? name.slice(0, dot) : name;
  }

  // Extension the server gives to the stored file.
  function storedExtension(name) {
    const ext = rawExtension(name);
    return ext === ".jpeg" ? ".jpg" : ext;
  }

  // Name without extension that the server keeps from a typed name.
  function typedStem(value) {
    const name = value.trim().normalize("NFC");
    return TYPED_EXT.includes(rawExtension(name)) ? withoutExtension(name) : name;
  }

  // Same rules as Image.safe_name on the server.
  function validName(name) {
    const reserved = /^(con|prn|aux|nul|com[1-9]|lpt[1-9])$/;
    return name !== ""
      && new TextEncoder().encode(name).length <= 200
      && !name.startsWith(".")
      && !name.endsWith(".")
      && !name.endsWith(" ")
      && !/[\/\\:*?"<>|\u0000-\u001f\u007f]/.test(name)
      && !reserved.test(name.split(".")[0].toLowerCase());
  }

  function validSubdir(path) {
    return path === "" || path.split("/").every(validName);
  }

  function megabytes(bytes) {
    return Math.max(1, Math.floor(bytes / 1e6)) + " " + (units[2] || "MB");
  }

  // ── Names already used in a destination (m=UPLOAD_LIST) ─────────────────

  const listings = new Map();

  function fillDirs(dest, dirs) {
    const list = document.getElementById("upload-dirs-" + dest);
    if (!list || list.options.length > 0) return;
    for (const dir of dirs) {
      const option = document.createElement("option");
      option.value = dir;
      list.appendChild(option);
    }
  }

  // Lowercase names of the files of a destination, null until they arrive.
  function existingFiles(dest, subdir) {
    const key = dest + "/" + subdir;
    if (!listings.has(key)) {
      const entry = { files: null };
      listings.set(key, entry);
      fetch(cfg.list + "&dest=" + dest + "&subdir=" + encodeURIComponent(subdir))
        .then(response => (response.ok ? response.json() : { files: [], dirs: [] }))
        .then(data => {
          entry.files = data.files.map(name => name.toLowerCase());
          fillDirs(dest, data.dirs);
          refresh();
        })
        .catch(() => {
          entry.files = [];
          refresh();
        });
    }
    return listings.get(key).files;
  }

  // ── State of a file of the wall ─────────────────────────────────────────

  // Destination, subdirectory and name chosen in the preview of a file.
  function choice(file) {
    const els = file.gw.els;
    const dest = els.dest.value;
    const named = dest !== "portrait" && dest !== "blason";
    const filed = dest === "images" || dest === "albums";
    return {
      dest: dest,
      named: named,
      filed: filed,
      subdir: filed ? els.subdir.value.trim().normalize("NFC") : "",
      stem: named ? typedStem(els.name.value) : ""
    };
  }

  // What two files must not share: an entry of a person is its name without
  // extension, a file of the base is its full name in its directory.
  function identity(file, c) {
    const stem = c.stem.toLowerCase();
    return c.dest === "person"
      ? "person/" + stem
      : c.dest + "/" + c.subdir + "/" + stem + file.gw.ext;
  }

  function existsOnServer(file, c, files) {
    const stem = c.stem.toLowerCase();
    return c.dest === "person"
      ? files.some(name => withoutExtension(name) === stem)
      : files.includes(stem + file.gw.ext);
  }

  function accepts(dest, file) {
    if (dest === "images") return true;
    if (dest === "person") return file.gw.ext !== ".txt";
    return file.gw.image;
  }

  function show(el, visible) {
    el.classList.toggle("d-none", !visible);
  }

  // Updates every preview and the Send button after any change on the wall.
  function refresh() {
    const queued = dz.getQueuedFiles();
    const slots = {};
    for (const file of queued) {
      const dest = file.gw.els.dest.value;
      if ((dest === "portrait" || dest === "blason") && !slots[dest]) {
        slots[dest] = file;
      }
    }
    const seen = new Set();
    let ready = queued.length > 0;

    for (const file of queued) {
      const els = file.gw.els;
      for (const option of els.dest.options) {
        const taken = slots[option.value] && slots[option.value] !== file;
        const refused = !accepts(option.value, file);
        option.disabled = refused || taken;
        option.title = refused ? cfg.typeRefused : taken ? cfg.slotTaken : "";
      }

      const c = choice(file);
      const nameOk = !c.named || validName(c.stem);
      const subdirOk = validSubdir(c.subdir);
      let pending = false;
      let conflict = false;
      let duplicate = false;
      if (c.named && nameOk && subdirOk) {
        const id = identity(file, c);
        duplicate = seen.has(id);
        seen.add(id);
        const files = existingFiles(c.dest, c.subdir);
        pending = files === null;
        conflict = !pending && existsOnServer(file, c, files);
      }

      show(els.nameCol, c.named);
      show(els.subdirRow, c.filed);
      if (c.filed) els.subdir.setAttribute("list", "upload-dirs-" + c.dest);
      els.name.classList.toggle("is-invalid", !nameOk);
      els.subdir.classList.toggle("is-invalid", !subdirOk);
      show(els.conflict, duplicate || (conflict && !file.gw.replace));
      // Replacing does not settle two files of the wall with the same name.
      show(els.replace, conflict && !duplicate);
      show(els.replaces, conflict && !duplicate && file.gw.replace);

      const unresolved = duplicate || (conflict && !file.gw.replace);
      if (!nameOk || !subdirOk || pending || unresolved) ready = false;
    }

    show(sendBtn, queued.length > 0);
    sendBtn.disabled = !ready || sending;
  }

  // ── Dropzone ────────────────────────────────────────────────────────────

  // A file over the limit of its kind is refused before being sent.
  function checkSizeByKind(file, done) {
    const image = IMAGE_EXT.includes(storedExtension(file.name));
    const max = image ? maxImage : maxDocument;
    if (file.size > max) done(cfg.tooBig.replace("{max}", megabytes(max)));
    else done();
  }

  const options = {
    url: cfg.action,
    paramName: "file",
    uploadMultiple: false,
    parallelUploads: 1,
    autoProcessQueue: false,
    maxFilesize: null,
    acceptedFiles: ".jpg,.jpeg,.png,.gif,.pdf,.docx,.odt,.txt",
    accept: checkSizeByKind,
    previewsContainer: wall,
    previewTemplate: document.getElementById("upload-preview").innerHTML,
    dictInvalidFileType: cfg.typeRefused
  };
  if (units.length === 4) {
    const [b, kb, mb, gb] = units;
    options.dictFileSizeUnits = { b: b, kb: kb, mb: mb, gb: gb, tb: "TB" };
  }
  const dz = new Dropzone(zone, options);

  dz.on("addedfile", file => {
    // Nothing joins the wall while it is being sent.
    if (sending) {
      dz.removeFile(file);
      return;
    }
    const preview = file.previewElement;
    const find = selector => preview.querySelector(selector);
    const ext = storedExtension(file.name);
    file.gw = {
      ext: ext,
      image: IMAGE_EXT.includes(ext),
      replace: false,
      els: {
        controls: find(".upload-controls"),
        dest: find(".upload-dest"),
        nameCol: find(".upload-name-col"),
        name: find(".upload-name"),
        subdirRow: find(".upload-subdir-row"),
        subdir: find(".upload-subdir"),
        conflict: find(".upload-conflict"),
        replace: find(".upload-replace"),
        replaces: find(".upload-replaces")
      }
    };
    const els = file.gw.els;
    find(".upload-ext").textContent = ext;
    els.name.value = withoutExtension(file.name);
    els.dest.value = ext === ".txt" ? "images" : "person";

    const changed = () => {
      file.gw.replace = false;
      refresh();
    };
    els.dest.addEventListener("change", changed);
    els.name.addEventListener("input", changed);
    // The names of a subdirectory are asked once the typing pauses.
    els.subdir.addEventListener("input", () => {
      file.gw.replace = false;
      sendBtn.disabled = true;
      clearTimeout(subdirTimer);
      subdirTimer = setTimeout(refresh, 400);
    });
    els.replace.addEventListener("click", () => {
      file.gw.replace = true;
      refresh();
    });
    find(".upload-skip").addEventListener("click", () => dz.removeFile(file));
    find(".upload-rename").addEventListener("click", () => {
      els.name.focus();
      els.name.select();
    });

    // Dropzone accepts or refuses the file right after this event.
    setTimeout(() => {
      if (file.status === Dropzone.ERROR) els.controls.hidden = true;
      refresh();
    }, 0);
  });

  dz.on("removedfile", refresh);

  dz.on("processing", file => {
    file.gw.els.controls.disabled = true;
  });

  dz.on("sending", (file, xhr, formData) => {
    const c = choice(file);
    formData.append("m", "UPLOAD_OK");
    if (cfg.base) formData.append("b", cfg.base);
    formData.append("i", cfg.index);
    formData.append("dest", c.dest);
    if (c.named) formData.append("name", c.stem);
    if (c.subdir) formData.append("subdir", c.subdir);
    if (file.gw.replace) formData.append("replace", "on");
  });

  dz.on("success", (file, response) => {
    if (response && response.name) {
      const name = file.previewElement.querySelector("[data-dz-name]");
      name.textContent = response.name;
    }
  });

  // An answer that is not the JSON of m=UPLOAD_OK is not shown as is.
  dz.on("error", (file, message, xhr) => {
    if (!xhr || typeof message !== "string") return;
    const el = file.previewElement.querySelector("[data-dz-errormessage]");
    el.textContent = cfg.error + " (" + xhr.status + ")";
  });

  // Without autoProcessQueue, Dropzone stops after each file.
  dz.on("complete", () => {
    if (sending) dz.processQueue();
  });

  dz.on("queuecomplete", () => {
    if (!sending) return;
    sending = false;
    if (dz.files.every(file => file.status === Dropzone.SUCCESS)) {
      window.location.href = cfg.done;
    } else {
      show(sendBtn, false);
      show(finishBtn, true);
    }
  });

  sendBtn.addEventListener("click", () => {
    sending = true;
    sendBtn.disabled = true;
    dz.processQueue();
  });

  finishBtn.addEventListener("click", () => {
    window.location.href = cfg.done;
  });

  // ── Paste ───────────────────────────────────────────────────────────────

  function stemTaken(stem) {
    const onWall = dz.files.some(
      file => file.gw && typedStem(file.gw.els.name.value) === stem
    );
    const files = existingFiles("person", "");
    return onWall
      || (files !== null && files.some(name => withoutExtension(name) === stem));
  }

  // image-YYYYMMDD-HHmmss, one second later as long as the name is taken.
  function timestampStem() {
    const date = new Date();
    const two = n => String(n).padStart(2, "0");
    for (;;) {
      const day = date.getFullYear() + two(date.getMonth() + 1) + two(date.getDate());
      const time = two(date.getHours()) + two(date.getMinutes()) + two(date.getSeconds());
      const stem = "image-" + day + "-" + time;
      if (!stemTaken(stem)) return stem;
      date.setSeconds(date.getSeconds() + 1);
    }
  }

  // A bitmap of the clipboard has no name of its own.
  function named(file) {
    if (file.name && !/^image\.(png|jpe?g|gif)$/i.test(file.name)) return file;
    const types = { "image/png": ".png", "image/jpeg": ".jpg", "image/gif": ".gif" };
    const ext = types[file.type] || storedExtension(file.name);
    return new File([file], timestampStem() + ext, { type: file.type });
  }

  document.addEventListener("paste", event => {
    const data = event.clipboardData;
    if (!data || data.files.length === 0 || sending) return;
    // Text copied with its picture is pasted as text in a field.
    const field = event.target.closest && event.target.closest("input, textarea");
    if (field && Array.from(data.types).includes("text/plain")) return;
    event.preventDefault();
    for (const file of Array.from(data.files)) dz.addFile(named(file));
  });

  // ── Dropped URL ─────────────────────────────────────────────────────────

  // An image dragged from another tab often comes as its address only: it
  // goes to the URL form of the page.
  dz.on("drop", event => {
    const data = event.dataTransfer;
    const urlField = document.getElementById("image_url");
    if (!data || data.files.length > 0 || !urlField || urlField.disabled) return;
    const lines = data.getData("text/uri-list").split(/\r?\n/);
    const url = lines.find(line => line && !line.startsWith("#"));
    if (!url) return;
    urlField.value = url;
    urlField.dispatchEvent(new Event("input"));
    document.getElementById("image_name").focus();
    show(document.getElementById("upload-url-hint"), true);
  });

  // The names of the images of the person are known before the first drop.
  existingFiles("person", "");
})();
