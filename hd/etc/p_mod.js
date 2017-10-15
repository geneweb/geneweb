/* $Id: p_mod.js, v 7.00 10/04/2017 01:29:45 hg a2 $ */

var modules = [];
var options = [];
for (i = 0; i < 13; i++)
{ options[i] = [];
}

modules[1] = "individu";
options[1][1] = "standard";
options[1][2] = "centered";
options[1][3] = "two cols";
modules[2] = "parents";
options[2][1] = "simple";
options[2][2] = "simple + photos";
options[2][3] = "evolved";
options[2][4] = "complete";
options[2][5] = "complete + photos";
modules[3] = "unions";
options[3][1] = "simple";
options[3][2] = "simple + photos"
options[3][3] = "evolved";
options[3][4] = "complete";
options[3][5] = "complete + photos";
modules[4] = "fratrie";
options[4][1] = "simple";
options[4][2] = "simple + photos";
options[4][3] = "complete";
options[4][4] = "complete + photos";
modules[5] = "relations";
options[5][1] = "simple";
options[5][2] = "complete";
modules[6] = "chronologie";
options[6][1] = "simple";
options[6][2] = "simple + events";
modules[7] = "notes";
options[7][1] = "simple";
options[7][2] = "complete";
modules[8] = "sources";
options[8][1] = "simple";
options[8][2] = "complete";
modules[9] = "arbres";
options[9][1] = "ascendants";
options[9][2] = "horizontal";
options[9][3] = "compact";
options[9][4] = "+3-3 gen.";
options[9][5] = "famille";
options[9][6] = "h-tree";
modules[10] = "gr_parents";
options[10][1] = "standard";
options[10][2] = "three cols";
modules[11] = "ligne";
options[11][1] = "standard";
modules[12] = "data_3col";
options[12][1] = "standard";

var img_prfx = $('#img_prfx').text();
var p_mod_table = '\
<table class="table table-sm table-hover mt-2 mb-0">\
  <thead class="thead-default">\
    <tr>\
      <th>Module</th>\
      <th>Options</th>\
    </tr>\
  </thead>\
  <tbody>';
  for (var i=1; i<=12; i++)
    { p_mod_table += '\
      <tr>\
        <td class="align-middle pmod">'+modules[i]+'\
        </td>\
        <td>\
          <div class="d-inline-flex small">';
          for (var k=1; k<options[i].length; k++)
            { var image = "<img class='w-100' src='"+img_prfx+"/"+modules[i]+"_"+k+".jpg'>";
              var button  = '<button class="btn btn-outline-secondary btn-sm" type="button" id="'+modules[i][0]+k+'" title="'+modules[i]+" "+options[i][k]+'"\
                           data-toggle="popover" data-trigger="hover" data-container="body" data-placement="bottom"\
                           data-html="true" data-content="'+image+'">'+options[i][k]+'</button>';
              p_mod_table += button;
            }
          p_mod_table += '\
          </div>\
        </td>\
      </tr>\n'
    }
  p_mod_table += '\
  </tbody>\
</table>';

function upd_p_mod_build () {
  var p_mod = $('#p_mod').val();
  var p_mod_build = '<div id="p_mod_builder">';
  for(i=0;i<=p_mod.length-2;i=i+2){
    var mod = "";
    for(j=1;j<=12;j++){if (p_mod[i] == modules[j][0]){mod = modules[j];}};
    var opt = p_mod[i+1];
    if(p_mod[0]=="z"){p_mod_build='<div id="p_mod_builder"><img id="rm" src="'+img_prfx+'/zz_1.jpg">\n';}
    else if(opt>=1&&opt<=9){p_mod_build+='<img id="rm" src="'+img_prfx+'/'+mod+'_'+opt+'.jpg">\n';}
    else{i-=1};
  };
  p_mod_build += '</div>';
  $('#p_mod_builder').replaceWith(p_mod_build);
};

$('#p_mod_table').replaceWith(p_mod_table);

upd_p_mod_build ();

$('[data-toggle="popover"]').popover();

$('#p_mod_rm').on('click', function ()
{ $('#p_mod').val($('#p_mod').val().slice(0, -2));
  $('#rm:last-child').remove();
});

$('#p_mod_bvar').on('click', function ()
{ $('#p_mod').val($('#p_mod_bvar').val());
  upd_p_mod_build ();
  $('#p_mod_builder').replaceWith(p_mod_build);
});

$('#p_mod').on('keyup', function ()
{ upd_p_mod_build ();
  $('#p_mod_builder').replaceWith(p_mod_build);
});

$('#zz').on('click', function ()
{ $('#p_mod').val('zz');
});

$('#p_mod_clear').on('click', function ()
{ $('#p_mod').val('');
  upd_p_mod_build ();
});

$('#i1').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'i1');
  $('#p_mod_builder').append('<img id="rm" src="'+img_prfx+'/individu_1.jpg">\n');
});

$('#i2').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'i2');
  $('#p_mod_builder').append('<img id="rm" src="'+img_prfx+'/individu_2.jpg">\n');
});

$('#i3').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'i3');
  $('#p_mod_builder').append('<img id="rm" src="'+img_prfx+'/individu_3.jpg">\n');
});

$('#p1').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'p1');
  $('#p_mod_builder').append('<img id="rm" src="'+img_prfx+'/parents_1.jpg">\n');
});

$('#p2').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'p2');
  $('#p_mod_builder').append('<img id="rm" src="'+img_prfx+'/parents_2.jpg">\n');
});

$('#p3').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'p3');
  $('#p_mod_builder').append('<img id="rm" src="'+img_prfx+'/parents_3.jpg">\n');
});

$('#p4').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'p4');
  $('#p_mod_builder').append('<img id="rm" src="'+img_prfx+'/parents_4.jpg">\n');
});

$('#p5').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'p5');
  $('#p_mod_builder').append('<img id="rm" src="'+img_prfx+'/parents_5.jpg">\n');
});

$('#u1').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'u1');
  $('#p_mod_builder').append('<img id="rm" src="'+img_prfx+'/unions_1.jpg">\n');
});

$('#u2').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'u2');
  $('#p_mod_builder').append('<img id="rm" src="'+img_prfx+'/unions_2.jpg">\n');
});

$('#u3').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'u3');
  $('#p_mod_builder').append('<img id="rm" src="'+img_prfx+'/unions_3.jpg">\n');
});

$('#u4').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'u4');
  $('#p_mod_builder').append('<img id="rm" src="'+img_prfx+'/unions_4.jpg">\n');
});

$('#u5').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'u5');
  $('#p_mod_builder').append('<img id="rm" src="'+img_prfx+'/unions_5.jpg">\n');
});

$('#f1').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'f1');
  $('#p_mod_builder').append('<img id="rm" src="'+img_prfx+'/fratrie_1.jpg">\n');
});

$('#f2').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'f2');
  $('#p_mod_builder').append('<img id="rm" src="'+img_prfx+'/fratrie_2.jpg">\n');
});

$('#f3').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'f3');
  $('#p_mod_builder').append('<img id="rm" src="'+img_prfx+'/fratrie_3.jpg">\n');
});

$('#f4').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'f4');
  $('#p_mod_builder').append('<img id="rm" src="'+img_prfx+'/fratrie_4.jpg">\n');
});

$('#r1').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'r1');
  $('#p_mod_builder').append('<img id="rm" src="'+img_prfx+'/relations_1.jpg">\n');
});

$('#r2').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'r2');
  $('#p_mod_builder').append('<img id="rm" src="'+img_prfx+'/relations_2.jpg">\n');
});

$('#c1').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'c1');
  $('#p_mod_builder').append('<img id="rm" src="'+img_prfx+'/chronologie_1.jpg">\n');
});

$('#c2').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'c2');
  $('#p_mod_builder').append('<img id="rm" src="'+img_prfx+'/chronologie_2.jpg">\n');
});

$('#n1').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'n1');
  $('#p_mod_builder').append('<img id="rm" src="'+img_prfx+'/notes_1.jpg">\n');
});

$('#n2').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'n2');
  $('#p_mod_builder').append('<img id="rm" src="'+img_prfx+'/notes_2.jpg">\n');
});

$('#s1').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'s1');
  $('#p_mod_builder').append('<img id="rm" src="'+img_prfx+'/sources_1.jpg">\n');
});

$('#s2').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'s2');
  $('#p_mod_builder').append('<img id="rm" src="'+img_prfx+'/sources_2.jpg">\n');
});

$('#a1').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'a1');
  $('#p_mod_builder').append('<img id="rm" src="'+img_prfx+'/arbres_1.jpg">\n');
});

$('#a2').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'a2');
  $('#p_mod_builder').append('<img id="rm" src="'+img_prfx+'/arbres_2.jpg">\n');
});

$('#a3').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'a3');
  $('#p_mod_builder').append('<img id="rm" src="'+img_prfx+'/arbres_3.jpg">\n');
});

$('#a4').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'a4');
  $('#p_mod_builder').append('<img id="rm" src="'+img_prfx+'/arbres_4.jpg">\n');
});

$('#a5').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'a5');
  $('#p_mod_builder').append('<img id="rm" src="'+img_prfx+'/arbres_5.jpg">\n');
});

$('#a6').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'a6');
  $('#a6_wide').val('on');
  $('#p_mod_builder').append('<img id="rm" src="'+img_prfx+'/arbres_6.jpg">\n');
});

$('#g1').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'g1');
  $('#p_mod_builder').append('<img id="rm" src="'+img_prfx+'/gr_parents_1.jpg">\n');
});

$('#g2').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'g2');
  $('#p_mod_builder').append('<img id="rm" src="'+img_prfx+'/gr_parents_2.jpg">\n');
});

$('#l1').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'l1');
  $('#p_mod_builder').append('<img id="rm" src="'+img_prfx+'/ligne_1.jpg">\n');
});

$('#d1').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'d1');
  $('#p_mod_builder').append('<img id="rm" src="'+img_prfx+'/data_3col_1.jpg">\n');
});