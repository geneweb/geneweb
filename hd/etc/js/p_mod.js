/* $Id: p_mod.js, v 7.00 2017/12/05 01:29:45 hg a2 $ */

var modules = [];
var options = [];
for (i = 0; i < 13; i++)
{ options[i] = [];
}

modules[1] = "individu";
options[1][0] = "i";
options[1][1] = "standard";
options[1][2] = "centered";
options[1][3] = "two cols";
modules[2] = "parents";
options[2][0] = "p";
options[2][1] = "simple";
options[2][2] = "simple + photos";
options[2][3] = "evolved";
options[2][4] = "complete";
options[2][5] = "complete + photos";
modules[3] = "unions";
options[3][0] = "u";
options[3][1] = "simple";
options[3][2] = "simple + photos"
options[3][3] = "evolved";
options[3][4] = "complete";
options[3][5] = "complete + photos";
modules[4] = "fratrie";
options[4][0] = "f";
options[4][1] = "simple";
options[4][2] = "simple + photos";
options[4][3] = "complete";
options[4][4] = "complete + photos";
modules[5] = "relations";
options[5][0] = "r";
options[5][1] = "simple";
options[5][2] = "complete";
modules[6] = "chronologie";
options[6][0] = "c";
options[6][1] = "simple";
options[6][2] = "simple + events";
modules[7] = "notes";
options[7][0] = "n";
options[7][1] = "simple";
options[7][2] = "complete";
modules[8] = "sources";
options[8][0] = "s";
options[8][1] = "simple";
options[8][2] = "complete";
modules[9] = "arbres";
options[9][0] = "a";
options[9][1] = "ascendants";
options[9][2] = "horizontal";
options[9][3] = "compact";
options[9][4] = "+3-3 gen.";
options[9][5] = "famille";
options[9][6] = "7 gen";
options[9][7] = "9 gen";
options[9][8] = "HI";
options[9][9] = "descendants";
modules[10] = "gr_parents";
options[10][0] = "g";
options[10][1] = "standard";
options[10][2] = "three cols";
modules[11] = "ligne";
options[11][0] = "l";
options[11][1] = "standard";
modules[12] = "data_3col";
options[12][0] = "d";
options[12][1] = "standard";

var queryString = window.location.search;
var urlParams = new URLSearchParams(queryString);
var p_mod = urlParams.get('p_mod');
if (p_mod==null || p_mod=="z2") { p_mod = "z2i3u2c1n1s2a3f1r1"};

function ik_in_p_mod (i, k){
  let pos_letter = p_mod.search(options[i][0]);
  let option = p_mod.substring(pos_letter+1, pos_letter+2);
  if (pos_letter==-1) {return false}
  else {if (option==k) {return true} else {return false}};
};

var img_prfx = $('#img_prfx').text();

function button (i, k, mode){
  var image = "<img class='w-100' src='"+img_prfx+"/"+modules[i]+"_"+k+".jpg'>";
  if (k == 0 ){
    var str = '<button class="btn '+mode+' btn-sm" \
      type="button" id="'+modules[i][0]+k+'" title="'+modules[i]+' reset"\
      data-toggle="popover" data-trigger="hover" data-container="body" \
      data-placement="bottom" data-html="true" ">'+options[i][k]+'</button>';
  } else {
    var str = '<button class="btn '+mode+' btn-sm" \
      type="button" id="'+modules[i][0]+k+'" title="'+modules[i]+" "+options[i][k]+'"\
      data-toggle="popover" data-trigger="hover" data-container="body" \
      data-placement="bottom"\
      data-html="true" data-content="'+image+'">'+options[i][k]+'</button>';
  };
  return str
};

var p_mod_init = '<div id="p_mod_builder">\n</div>\n';

function build_p_mod_image(){
  $('#p_mod_builder').replaceWith(p_mod_init);
  var p_mod_build = '<div id="p_mod_builder">';
  for (i=0;i<=p_mod.length-2;i=i+2){
    var mod = "";
    for (j=1;j<=12;j++) {if (p_mod[i] == modules[j][0]) {mod = modules[j];}};
    var opt = p_mod[i+1];
    if ((p_mod[0]=="z" && p_mod[1]=="2" || p_mod=="") && i==0 && j==1)
      {$('#p_mod_builder').append('<div id="p_mod_builder"><img id="rm" src="'+img_prfx+'/zz_1.jpg">\n');}
    else if (opt>=1 && opt<=9 && mod!="") {$('#p_mod_builder').append('<img id="rm" src="'+img_prfx+'/'+mod+'_'+opt+'.jpg">\n');}
    else {i-=1};
  };
  $('#p_mod_builder').append('</div>');
};

function reset_all_buttons (){
  for (var i=1; i<options.length; i++){
    for (var k=1; k<options[i].length; k++){
      var letter = options[i][0];
      var elt = '#'+letter+k;
      $(elt).removeClass("btn-outline-primary").removeClass("btn-outline-success").addClass("btn-outline-secondary")
    };
  };
  p_mod = '';
  $('#p_mod').val(p_mod);
  build_p_mod_image();
};

function create_option_reset_button (i){
  var letter = options[i][0];
  var new_p_mod = p_mod;
  $('#'+letter+'0').on('click', function ()
  { for (j = 1;j<options[i].length;j++){
      $('#'+letter+j).removeClass("btn-outline-primary").removeClass("btn-outline-success").addClass("btn-outline-secondary")
      regexp = RegExp (letter+j);
      new_p_mod = new_p_mod.replace (regexp, '');
    }
    p_mod = new_p_mod+letter+'0';
    $('#p_mod').val(p_mod);
    build_p_mod_image();
  });
};

function create_option_button (i, k){
  var letter = options[i][0];
  var regexp = RegExp (letter+k);
  $('#'+letter+k).on('click', function ()
  { /* si un bouton est déjà actif, le désactiver */
    /* si un autre bouton (sauf 0) est déjà actif, rajouter le nouveau */
    var index = p_mod.search (regexp);
    if (index>0) {
      /* on reset */
      var new_p_mod = p_mod.replace (regexp, '');
      $('#'+letter+k).removeClass("btn-outline-primary").removeClass("btn-outline-success").addClass("btn-outline-secondary")
      $('#p_mod').val(new_p_mod)
    } else { 
      /* on active et on ajoute */
      $('#'+letter+k).removeClass("btn-outline-primary").removeClass("btn-outline-secondary").addClass("btn-outline-success")
      p_mod = p_mod+letter+k;
      $('#p_mod').val(p_mod);
    }
    build_p_mod_image();
  });
};

var pos_zz = p_mod.search("z");
if (pos_zz == -1) {var zx = "z0"}
else {var zx = p_mod.substring(pos_zz, pos_zz+2)};
if (zx == "z0") {var bz1="secondary"; var bz2="secondary"};
if (zx == "z1") {var bz1="primary"; var bz2="secondary"};
if (zx == "z2") {var bz1="secondary"; var bz2="primary"};

var mode = "";
var p_mod_table = '\
<table class="table table-sm table-hover mt-2 mb-0">\
  <thead class="thead-default">\
    <tr>\
      <th>Module</th>\
      <th>Options \
      <button class="btn btn-outline-'+bz1+' ml-5" \
         type="button" id="1col" title="one column">1\
      </button>\
      <button class="btn btn-outline-'+bz2+'" \
         type="button" id="2col" title="two columns">2\
      </button> (Colonnes)\
      </th>\
    </tr>\
  </thead>\
  <tbody>';
  for (var i=1; i<options.length; i++){
    p_mod_table += '\
      <tr>\
        <td class="align-middle pmod">'+modules[i]+'\
        </td>\
        <td>\
          <div class="btn-group small">';
          for (var k=0; k<options[i].length; k++){
            if (ik_in_p_mod(i, k)) { mode = "btn-outline-primary"}
            else { mode = "btn-outline-secondary"};
            p_mod_table += button (i, k, mode)
          };
          p_mod_table += '\
          </div>\
        </td>\
      </tr>\n'
    };
  p_mod_table += '\
  </tbody>\
</table>';

build_p_mod_image(); /* from currrent p_mod */

$('#p_mod_table').replaceWith(p_mod_table);

$('[data-toggle="popover"]').popover();

$('#p_mod').on('keyup', function ()
{ build_p_mod_image()
});

$('#p_mod_bvar').on('click', function ()
{ $('#p_mod').val($('#p_mod_bvar').val())
});

$('#p_mod_rm').on('click', function ()
{ p_mod = $('#p_mod').val().slice(0, -2);
  $('#p_mod').val(p_mod);
  $('#rm:last-child').remove();
});

$('#p_mod_ok').on('click', function ()
{ p_mod = $('#p_mod').val();
  var regexp = RegExp ('z');
  var index = p_mod.search (regexp);
  if (index<0) {p_mod = 'z2'+p_mod};
  $('#p_mod').val(p_mod);
  $('#rm:last-child').remove();
});

$('#p_mod_clear').on('click', function ()
{ p_mod = '';
  $('#p_mod').val('');
  $('#p_mod_builder').replaceWith(p_mod_init);
  reset_all_buttons();
  $('#1col').removeClass("btn-outline-primary").removeClass("btn-outline-success").addClass("btn-outline-secondary")
  $('#2col').removeClass("btn-outline-primary").removeClass("btn-outline-success").addClass("btn-outline-secondary")
});

$('#1col').on('click', function ()
{  p_mod = 'z1'
  $('#p_mod').val(p_mod);
  $('#1col').removeClass("btn-outline-primary").removeClass("btn-outline-secondary").addClass("btn-outline-success");
});

$('#2col').on('click', function ()
{  p_mod = 'z2'
  $('#p_mod').val(p_mod);
  $('#2col').removeClass("btn-outline-primary").removeClass("btn-outline-secondary").addClass("btn-outline-success");
});

$('#zz').on('click', function ()
{ p_mod = 'z2'
  $('#p_mod').val(p_mod);
  $('#2col').removeClass("btn-outline-primary").removeClass("btn-outline-secondary").addClass("btn-outline-success");
});

for (var i=1; i<options.length; i++){
  create_option_reset_button(i);
  for (var k=1; k<options[i].length; k++){
    create_option_button (i, k);
  };
};
