// Media page (m=MEDIA), of a person (the carrousel page, mode "person") or
// of the base (mode "base", media.txt).
// Files dropped on the zone, pasted (Ctrl+V) or picked wait on a wall where
// the user chooses their name, and for a person their destination, then they
// are sent one by one to m=MEDIA_SEND_OK. For the base, the files go to the
// images or the album shown in the list of the page, where an image is only
// loaded on the hover of its icon. Needs dropzone-min.js and Bootstrap. The
// place of the files comes from the data-* attributes of #media, the limits
// and the messages from those of #upload-zone (media_zone.txt).
(function () {
  "use strict";

  const media = document.getElementById("media");
  const zone = document.getElementById("upload-zone");
  if (!media || !zone || !window.Dropzone) return;

  const cfg = Object.assign({}, zone.dataset, media.dataset);
  const baseMode = cfg.mode === "base";
  // Place shown by the page of the base.
  const place = { dest: "images", subdir: "" };
  const wall = document.getElementById("upload-wall");
  const sendBtn = document.getElementById("upload-send");
  const finishBtn = document.getElementById("upload-finish");
  const maxImage = Number(cfg.maxImage);
  const maxDocument = Number(cfg.maxDocument);
  const units = cfg.units.split("/");
  const lang = document.documentElement.lang || undefined;

  const IMAGE_EXT = [".jpg", ".png", ".gif"];
  // Extensions that the server removes from a typed name.
  const TYPED_EXT = IMAGE_EXT.concat([".jpeg", ".pdf", ".docx", ".odt", ".txt"]);

  let sending = false;

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

  function humanSize(bytes) {
    const steps = [1, 1e3, 1e6, 1e9];
    let i = steps.length - 1;
    while (i > 0 && bytes < steps[i]) i--;
    const value = bytes / steps[i];
    const text = i === 0
      ? String(value)
      : value.toLocaleString(lang, { maximumFractionDigits: 1 });
    return text + " " + (units[i] || "");
  }

  function show(el, visible) {
    el.classList.toggle("d-none", !visible);
  }

  // ── Names already used in a place (m=MEDIA_LIST) ────────────────────────

  const listings = new Map();

  function listUrl(dest, subdir) {
    return cfg.list + "&dest=" + dest + "&subdir=" + encodeURIComponent(subdir);
  }

  // Lowercase names of the files of a place, null until they arrive.
  function existingFiles(dest, subdir) {
    const key = dest + "/" + subdir;
    if (!listings.has(key)) {
      const entry = { files: null };
      listings.set(key, entry);
      fetch(listUrl(dest, subdir))
        .then(response => (response.ok ? response.json() : { files: [] }))
        .then(data => {
          entry.files = data.files.map(file => file.name.toLowerCase());
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

  // Place and name of a file, as chosen in its preview.
  function choice(file) {
    if (baseMode) {
      return {
        dest: place.dest,
        subdir: place.subdir,
        named: true,
        stem: typedStem(file.gw.els.name.value)
      };
    }
    const dest = file.gw.els.dest.value;
    const named = dest !== "portrait" && dest !== "blason";
    return {
      dest: dest,
      subdir: "",
      named: named,
      stem: named ? typedStem(file.gw.els.name.value) : ""
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

  // The root of the albums receives no file.
  function noAlbum() {
    return baseMode && place.dest === "albums" && place.subdir === "";
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
    let ready = queued.length > 0 && !noAlbum();

    for (const file of queued) {
      const els = file.gw.els;
      for (const option of els.dest.options) {
        const taken = slots[option.value] && slots[option.value] !== file;
        const refused = !accepts(option.value, file);
        option.disabled = refused || taken;
        option.title = refused ? cfg.typeRefused : taken ? cfg.slotTaken : "";
      }

      const c = choice(file);
      const refused = baseMode && !accepts(c.dest, file);
      const nameOk = !c.named || validName(c.stem);
      let pending = false;
      let conflict = false;
      let duplicate = false;
      if (c.named && nameOk) {
        const id = identity(file, c);
        duplicate = seen.has(id);
        seen.add(id);
        const files = existingFiles(c.dest, c.subdir);
        pending = files === null;
        conflict = !pending && existsOnServer(file, c, files);
      }

      show(els.nameCol, c.named);
      show(els.refused, refused);
      els.name.classList.toggle("is-invalid", !nameOk);
      show(els.conflict, duplicate || (conflict && !file.gw.replace));
      // Replacing does not settle two files of the wall with the same name.
      show(els.replace, conflict && !duplicate);
      show(els.replaces, conflict && !duplicate && file.gw.replace);

      const unresolved = duplicate || (conflict && !file.gw.replace);
      if (refused || !nameOk || pending || unresolved) ready = false;
    }

    if (baseMode) show(document.getElementById("media-album-hint"), noAlbum());
    show(sendBtn, queued.length > 0);
    sendBtn.disabled = !ready || sending;
  }

  // ── Dropzone ────────────────────────────────────────────────────────────

  // A file over the limit of its kind is refused before being sent.
  function checkSizeByKind(file, done) {
    const image = IMAGE_EXT.includes(storedExtension(file.name));
    const max = image ? maxImage : maxDocument;
    if (file.size > max) done(cfg.tooBig.replace("{max}", humanSize(max)));
    else done();
  }

  const options = {
    url: cfg.action,
    paramName: "file",
    uploadMultiple: false,
    parallelUploads: 1,
    autoProcessQueue: false,
    maxFilesize: null,
    acceptedFiles: cfg.accepted,
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
        conflict: find(".upload-conflict"),
        replace: find(".upload-replace"),
        replaces: find(".upload-replaces"),
        refused: find(".upload-refused")
      }
    };
    const els = file.gw.els;
    find(".upload-ext").textContent = ext;
    find(".upload-size").textContent = humanSize(file.size);
    els.name.value = withoutExtension(file.name);
    els.dest.value = "person";
    if (baseMode) find(".upload-dest-col").remove();

    const changed = () => {
      file.gw.replace = false;
      refresh();
    };
    els.dest.addEventListener("change", changed);
    els.name.addEventListener("input", changed);
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
    formData.append("m", "MEDIA_SEND_OK");
    if (cfg.base) formData.append("b", cfg.base);
    for (const [key, value] of new URLSearchParams(cfg.access || "")) {
      formData.append(key, value);
    }
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

  // An answer that is not the JSON of m=MEDIA_SEND_OK is not shown as is.
  dz.on("error", (file, message, xhr) => {
    if (!xhr || typeof message !== "string") return;
    const el = file.previewElement.querySelector("[data-dz-errormessage]");
    el.textContent = cfg.error + " (" + xhr.status + ")";
  });

  // Without autoProcessQueue, Dropzone stops after each file.
  dz.on("complete", () => {
    if (sending) dz.processQueue();
  });

  // The page of a person is reloaded; for the base, the wall is emptied and
  // the list of the place updated.
  function finish() {
    if (!baseMode) {
      window.location.href = cfg.done;
      return;
    }
    dz.removeAllFiles(true);
    show(finishBtn, false);
    load();
  }

  dz.on("queuecomplete", () => {
    if (!sending) return;
    sending = false;
    if (dz.files.every(file => file.status === Dropzone.SUCCESS)) {
      finish();
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

  finishBtn.addEventListener("click", finish);

  // ── Paste ───────────────────────────────────────────────────────────────

  function stemTaken(stem) {
    const onWall = dz.files.some(
      file => file.gw && typedStem(file.gw.els.name.value) === stem
    );
    const files = baseMode
      ? existingFiles(place.dest, place.subdir)
      : existingFiles("person", "");
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

  if (!baseMode) {
    // The names of the images of the person are known before the first drop.
    existingFiles("person", "");
    return;
  }

  // ── Files of the base ───────────────────────────────────────────────────

  const rows = document.getElementById("media-rows");
  const crumbs = document.getElementById("media-crumbs");
  const albumGallery = document.getElementById("media-album-gallery");
  const filter = document.getElementById("media-filter");
  const more = document.getElementById("media-more");
  const empty = document.getElementById("media-empty");
  const status = document.getElementById("media-status");
  const newBtn = document.getElementById("media-new");
  const newForm = document.getElementById("media-new-form");
  const newName = document.getElementById("media-new-name");
  const STEP = 200;

  let listing = { files: [], dirs: [] };
  let limit = STEP;
  let tooltips = [];

  function element(tag, className, text) {
    const el = document.createElement(tag);
    if (className) el.className = className;
    if (text !== undefined) el.textContent = text;
    return el;
  }

  function icon(className) {
    const i = element("i", className);
    i.setAttribute("aria-hidden", "true");
    return i;
  }

  function path(name) {
    return place.subdir === "" ? name : place.subdir + "/" + name;
  }

  // Path of a file in a gallery: relative to the images, or albums/….
  function galleryPath(name) {
    return place.dest === "albums" ? "albums/" + path(name) : name;
  }

  function fileUrl(name) {
    const mode = place.dest === "albums" ? "m=IMA&s=" : "m=IM&s=";
    return cfg.prefix + mode + encodeURIComponent(path(name));
  }

  function galleryUrl(query) {
    return cfg.prefix + "m=MOD_GALLERY&new&" + query;
  }

  function fileIcon(name) {
    const ext = storedExtension(name);
    if (IMAGE_EXT.includes(ext)) return "fa-file-image";
    if (ext === ".pdf") return "fa-file-pdf";
    if (ext === ".docx" || ext === ".odt") return "fa-file-word";
    if (ext === ".txt") return "fa-file-lines";
    return "fa-file";
  }

  function navigate(dest, subdir) {
    place.dest = dest;
    place.subdir = subdir;
    limit = STEP;
    // The lines of the previous place are not clicked again while loading.
    rows.replaceChildren();
    status.textContent = "";
    for (const btn of document.querySelectorAll("[data-media-dest]")) {
      const on = btn.dataset.mediaDest === dest;
      btn.classList.toggle("active", on);
      btn.setAttribute("aria-pressed", String(on));
    }
    closeNewAlbum();
    load();
    refresh();
  }

  // The listing also tells the wall the names of the place.
  function load() {
    const key = place.dest + "/" + place.subdir;
    listings.set(key, { files: null });
    fetch(listUrl(place.dest, place.subdir))
      .then(response => (response.ok ? response.json() : { files: [], dirs: [] }))
      .catch(() => ({ files: [], dirs: [] }))
      .then(data => {
        if (key !== place.dest + "/" + place.subdir) return;
        listing = data;
        listings.set(key, { files: data.files.map(file => file.name.toLowerCase()) });
        render();
        refresh();
      });
  }

  // The path starts with the directory of the images or of the albums and
  // reads as one path of the server.
  crumbs.style.setProperty("--bs-breadcrumb-divider", JSON.stringify(cfg.sep));
  crumbs.style.setProperty("--bs-breadcrumb-item-padding-x", "0");

  function renderPath() {
    crumbs.replaceChildren();
    const segments = place.subdir === "" ? [] : place.subdir.split("/");
    const root = place.dest === "albums" ? cfg.albumsDir : cfg.imagesDir;
    const items = [{ name: root, subdir: "" }].concat(
      segments.map((name, i) => ({ name: name, subdir: segments.slice(0, i + 1).join("/") }))
    );
    items.forEach((item, i) => {
      const li = element("li", "breadcrumb-item");
      const label = document.createTextNode(item.name);
      if (i === items.length - 1) {
        li.classList.add("active");
        li.setAttribute("aria-current", "page");
        li.appendChild(label);
      } else {
        const link = element("button", "btn btn-link p-0 align-baseline");
        link.type = "button";
        link.appendChild(label);
        link.addEventListener("click", () => navigate("albums", item.subdir));
        li.appendChild(link);
      }
      crumbs.appendChild(li);
    });
    const images = listing.files.some(file => IMAGE_EXT.includes(storedExtension(file.name)));
    show(albumGallery, place.subdir !== "" && images);
    albumGallery.href = galleryUrl("folder=" + encodeURIComponent(place.subdir));
  }

  // The preview of an image is only loaded when its tooltip is shown.
  function preview(link, file) {
    let content = null;
    const tooltip = new bootstrap.Tooltip(link, {
      html: true,
      placement: "left",
      title: () => {
        if (content) return content;
        content = element("div");
        content.appendChild(element("strong", "text-break", file.name));
        content.appendChild(element("div", "small", humanSize(file.size)));
        if (IMAGE_EXT.includes(storedExtension(file.name))) {
          const img = element("img", "rounded my-1");
          const size = element("div", "small");
          img.alt = "";
          img.width = 185;
          img.addEventListener("load", () => {
            size.textContent = img.naturalWidth + " × " + img.naturalHeight + " px";
            tooltip.update();
          });
          img.src = fileUrl(file.name);
          content.append(img, size);
        }
        return content;
      }
    });
    tooltips.push(tooltip);
  }

  function deleteLink(name, album) {
    const del = element("a");
    del.href = "#";
    del.setAttribute("role", "button");
    del.title = rows.dataset.delete;
    del.setAttribute("aria-label", rows.dataset.delete + " " + name);
    del.appendChild(icon("far fa-trash-can text-warning"));
    del.addEventListener("click", event => {
      event.preventDefault();
      remove(name, album);
    });
    return del;
  }

  function dirRow(dir) {
    const row = element("div", "d-flex align-items-center gap-1 mt-1");
    row.appendChild(icon("fa fa-folder fa-fw text-warning"));
    const link = element("button", "btn btn-link p-0 text-truncate", dir.name);
    link.type = "button";
    const target = path(dir.name);
    link.addEventListener("click", () => navigate("albums", target));
    row.appendChild(link);
    if (dir.empty) row.appendChild(deleteLink(dir.name, true)).classList.add("ms-auto");
    return row;
  }

  function fileRow(file) {
    const row = element("div", "d-flex align-items-center gap-1 mt-1");
    const open = element("a");
    open.href = fileUrl(file.name);
    open.target = "_blank";
    open.rel = "noopener";
    open.setAttribute("aria-label", file.name);
    open.appendChild(icon("far fa-fw text-primary " + fileIcon(file.name)));
    preview(open, file);
    row.appendChild(open);
    if (IMAGE_EXT.includes(storedExtension(file.name))) {
      const gallery = element("a", "me-1");
      gallery.href = galleryUrl("src=" + encodeURIComponent(galleryPath(file.name)));
      gallery.title = rows.dataset.gallery;
      gallery.appendChild(icon("fa-regular fa-images fa-xs text-info"));
      row.appendChild(gallery);
    }
    const name = element("button", "btn p-0 border-0 text-truncate text-start");
    name.type = "button";
    name.title = file.name;
    name.appendChild(element("samp", "", file.name));
    name.addEventListener("click", () => copy(file.name));
    row.appendChild(name);
    row.appendChild(element("span", "ms-auto ps-2 small text-body-secondary text-nowrap",
      humanSize(file.size)));
    row.appendChild(deleteLink(file.name, false)).classList.add("ms-1");
    return row;
  }

  function render() {
    renderPath();
    tooltips.forEach(tooltip => tooltip.dispose());
    tooltips = [];
    const word = filter.value.trim().toLowerCase();
    const match = name => word === "" || name.toLowerCase().includes(word);
    const byName = (a, b) => a.localeCompare(b, lang, { numeric: true, sensitivity: "base" });
    const dirs = listing.dirs.filter(dir => match(dir.name))
      .sort((a, b) => byName(a.name, b.name));
    const files = listing.files.filter(file => match(file.name))
      .sort((a, b) => byName(a.name, b.name));
    rows.replaceChildren(...dirs.map(dirRow), ...files.slice(0, limit).map(fileRow));
    show(more, files.length > limit);
    show(empty, dirs.length === 0 && files.length === 0);
  }

  function post(params) {
    const body = new URLSearchParams(params);
    if (cfg.base) body.append("b", cfg.base);
    return fetch(cfg.action, { method: "POST", body: body })
      .then(response => response.json().then(data => ({ ok: response.ok, data: data })))
      .catch(() => ({ ok: false, data: { error: cfg.error } }));
  }

  function report(ok, text) {
    status.className = "mt-2 " + (ok ? "text-success" : "text-danger");
    status.textContent = text;
  }

  // Without the clipboard API (old browser, http), a hidden selection is copied.
  function copy(name) {
    (navigator.clipboard ? navigator.clipboard.writeText(name) : Promise.reject())
      .catch(() => {
        const area = element("textarea");
        area.value = name;
        area.readOnly = true;
        area.style.position = "fixed";
        area.style.opacity = "0";
        document.body.appendChild(area);
        area.select();
        try {
          if (!document.execCommand("copy")) throw new Error("copy");
        } finally {
          area.remove();
        }
      })
      .then(() => report(true, name + " " + rows.dataset.copied), () => report(false, cfg.error));
  }

  // An empty album is deleted without confirmation.
  function remove(name, album) {
    if (!album && !window.confirm(cfg.deleteConfirm + "\n" + name)) return;
    post({ m: "MEDIA_DEL_OK", dest: place.dest, subdir: place.subdir, name: name })
      .then(({ ok, data }) => {
        report(ok, ok ? (album ? "" : cfg.deleted) : data.error);
        load();
      });
  }

  for (const btn of document.querySelectorAll("[data-media-dest]")) {
    btn.addEventListener("click", () => navigate(btn.dataset.mediaDest, ""));
  }

  filter.addEventListener("input", () => {
    limit = STEP;
    render();
  });

  more.addEventListener("click", () => {
    limit += STEP;
    render();
  });

  function closeNewAlbum() {
    show(newForm, false);
    show(newBtn, place.dest === "albums");
  }

  newBtn.addEventListener("click", () => {
    show(newBtn, false);
    show(newForm, true);
    newName.value = "";
    newName.classList.remove("is-invalid");
    newName.focus();
  });

  newForm.addEventListener("submit", event => {
    event.preventDefault();
    const name = newName.value.trim().normalize("NFC");
    if (!validName(name) || name === "saved") {
      newName.classList.add("is-invalid");
      return;
    }
    post({ m: "MEDIA_MKDIR_OK", subdir: path(name) })
      .then(({ ok, data }) => (ok ? navigate("albums", data.subdir) : report(false, data.error)));
  });

  newName.addEventListener("input", () => newName.classList.remove("is-invalid"));
  newName.addEventListener("keydown", event => {
    if (event.key === "Escape") closeNewAlbum();
  });

  navigate("images", "");
})();
