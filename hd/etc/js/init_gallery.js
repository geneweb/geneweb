function init_gallery() {
  const title  = document.getElementById('ig_page_title').value.trim();
  const fname  = document.getElementById('ig_fname').value.trim();
  const folder = document.getElementById('ig_folder').value.trim();
  const notes  = document.getElementById('notes_comments');
  const modal  = document.getElementById('init_gallery');
  const hide   = () => bootstrap.Modal.getInstance(modal)?.hide();

  const write = (t, images) => {
  const data = { title: t, images };
  notes.value = 'TITLE=' + t + '\nTYPE=gallery\n'
    + JSON.stringify(data, null, 2);
    hide();
  };

  if (folder) {
    fetch(GW.prefix + 'm=FOLDER_IMAGES&folder='
      + encodeURIComponent(folder))
      .then(r => { if (!r.ok) throw new Error('HTTP ' + r.status); return r.json(); })
      .then(files => {
        files = Array.isArray(files) ? files : (files.images || []);
        write(title || folder,
          files.map(f => ({ img: f, desc: '', map: [], groups: [] })));
      })
      .catch(() => alert('[*loading/error]1'));
  } else {
    write(title, [{ img: fname, desc: '', map: [], groups: [] }]);
  }
}

document.addEventListener('DOMContentLoaded', () => {
  const fname  = document.getElementById('ig_fname');
  const folder = document.getElementById('ig_folder');
  const modal  = document.getElementById('init_gallery');

  fname?.addEventListener('input', function () {
    if (folder) folder.disabled = this.value.trim() !== '';
  });
  folder?.addEventListener('input', function () {
    if (fname) fname.disabled = this.value.trim() !== '';
  });
  modal?.addEventListener('show.bs.modal', () => {
    ['ig_fname', 'ig_folder', 'ig_page_title'].forEach(id => {
      const el = document.getElementById(id);
      if (el) { el.value = ''; el.disabled = false; }
    });
  });
  document.getElementById('ig_ok_btn')
    ?.addEventListener('click', init_gallery);
});