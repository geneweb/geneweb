/* $Id: p_mod.js, v 7.00 07/04/2017 01:14:50 hg $ */

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
options[4][1] = "simple + photos";
options[4][1] = "complete";
options[4][1] = "complete + photos";
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
options[9][1] = "ascendants tree";
options[9][2] = "horizontal tree";
options[9][3] = "compact tree";
options[9][4] = "+3 -3 gen.";
modules[10] = "gr_parents";
options[10][1] = "standard";
options[10][2] = "three cols";
modules[11] = "ligne";
options[11][1] = "standard";
modules[12] = "data_3col";
options[12][1] = "standard";

var newHtml1 = '\
<table class="table table-sm table-hover mt-2 mb-0 mx-2">\
  <thead class="thead-default">\
    <tr>\
      <th>Module</th>\
      <th>Options</th>\
    </tr>\
  </thead>\
  <tbody>';
  for (var i=1; i<=12; i++)
    { newHtml1 += '\
      <tr>\
        <td class="align-middle pmod">'+modules[i]+'\
        </td>\
        <td>\
          <div class="d-inline-flex small">';
          for (var k=1; k<options[i].length; k++)
            { var image = "<img class='w-100' src='images/"+modules[i]+"_"+k+".jpg'>";
              var html  = '<button class="btn btn-secondary btn-sm" type="button" id="'+modules[i][0]+k+'" title="'+modules[i]+" "+options[i][k]+'"\
                           data-toggle="popover" data-trigger="hover" data-container="body" data-placement="bottom"\
                           data-html="true" data-content="'+image+'">'+options[i][k]+'</button>';
              newHtml1 += html;
            }
          newHtml1 += '\
          </div>\
        </td>\
      </tr>\n'
    }
  newHtml1 += '\
  </tbody>\
</table>';

document.getElementById("p_mod_table").innerHTML = newHtml1;

$('[data-toggle="popover"]').popover();

$('#zz').on('click', function ()
{ $('#p_mod').val('zz');
});

$('#p_mod-clear').on('click', function ()
{ $('#p_mod').val('');
  $('#p_mod_builder').val('');
  $('#p_mod_builder').html('<img src="images/menubar_1.jpg">');
});

$('#i1').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'i1');
  $('#p_mod_builder').html($('#p_mod_builder').html()+'<img src="images/individu_1.jpg" width="300px">\n');
});

$('#i2').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'i2');
  $('#p_mod_builder').html($('#p_mod_builder').html()+'<img src="images/individu_2.jpg" width="300px">\n');
});

$('#i3').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'i3');
  $('#p_mod_builder').html($('#p_mod_builder').html()+'<img src="images/individu_3.jpg" width="300px">\n');
});

$('#p1').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'p1');
  $('#p_mod_builder').html($('#p_mod_builder').html()+'<img src="images/parents_1.jpg" width="300px">\n');
});

$('#p2').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'p2');
  $('#p_mod_builder').html($('#p_mod_builder').html()+'<img src="images/parents_2.jpg" width="300px">\n');
});

$('#p3').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'p3');
  $('#p_mod_builder').html($('#p_mod_builder').html()+'<img src="images/parents_3.jpg" width="300px">\n');
});

$('#p4').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'p4');
  $('#p_mod_builder').html($('#p_mod_builder').html()+'<img src="images/parents_4.jpg" width="300px">\n');
});

$('#p5').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'p5');
  $('#p_mod_builder').html($('#p_mod_builder').html()+'<img src="images/parents_5.jpg" width="300px">\n');
});

$('#u1').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'u1');
  $('#p_mod_builder').html($('#p_mod_builder').html()+'<img src="images/unions_1.jpg" width="300px">\n');
});

$('#u2').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'u2');
  $('#p_mod_builder').html($('#p_mod_builder').html()+'<img src="images/unions_2.jpg" width="300px">\n');
});

$('#u3').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'u3');
  $('#p_mod_builder').html($('#p_mod_builder').html()+'<img src="images/unions_3.jpg" width="300px">\n');
});

$('#u4').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'u4');
  $('#p_mod_builder').html($('#p_mod_builder').html()+'<img src="images/unions_4.jpg" width="300px">\n');
});

$('#u5').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'u5');
  $('#p_mod_builder').html($('#p_mod_builder').html()+'<img src="images/unions_5.jpg" width="300px">\n');
});

$('#f1').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'f1');
  $('#p_mod_builder').html($('#p_mod_builder').html()+'<img src="images/fratrie_1.jpg" width="300px">\n');
});

$('#f2').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'f2');
  $('#p_mod_builder').html($('#p_mod_builder').html()+'<img src="images/fratrie_2.jpg" width="300px">\n');
});

$('#f3').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'f3');
  $('#p_mod_builder').html($('#p_mod_builder').html()+'<img src="images/fratrie_3.jpg" width="300px">\n');
});

$('#f4').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'f4');
  $('#p_mod_builder').html($('#p_mod_builder').html()+'<img src="images/fratrie_4.jpg" width="300px">\n');
});

$('#r1').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'r1');
  $('#p_mod_builder').html($('#p_mod_builder').html()+'<img src="images/relations_1.jpg" width="300px">\n');
});

$('#r2').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'r2');
  $('#p_mod_builder').html($('#p_mod_builder').html()+'<img src="images/relations_2.jpg" width="300px">\n');
});

$('#c1').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'c1');
  $('#p_mod_builder').html($('#p_mod_builder').html()+'<img src="images/chronologie_1.jpg" width="300px">\n');
});

$('#c2').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'c2');
  $('#p_mod_builder').html($('#p_mod_builder').html()+'<img src="images/chronologie_2.jpg" width="300px">\n');
});

$('#n1').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'n1');
  $('#p_mod_builder').html($('#p_mod_builder').html()+'<img src="images/notes_1.jpg" width="300px">\n');
});

$('#n2').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'n2');
  $('#p_mod_builder').html($('#p_mod_builder').html()+'<img src="images/notes_2.jpg" width="300px">\n');
});

$('#s1').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'s1');
  $('#p_mod_builder').html($('#p_mod_builder').html()+'<img src="images/sources_1.jpg" width="300px">\n');
});

$('#s2').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'s2');
  $('#p_mod_builder').html($('#p_mod_builder').html()+'<img src="images/sources_2.jpg" width="300px">\n');
});

$('#a1').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'a1');
  $('#p_mod_builder').html($('#p_mod_builder').html()+'<img src="images/arbres_1.jpg" width="300px">\n');
});

$('#a2').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'a2');
  $('#p_mod_builder').html($('#p_mod_builder').html()+'<img src="images/arbres_2.jpg" width="300px">\n');
});

$('#a3').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'a3');
  $('#p_mod_builder').html($('#p_mod_builder').html()+'<img src="images/arbres_3.jpg" width="300px">\n');
});

$('#a4').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'a4');
  $('#p_mod_builder').html($('#p_mod_builder').html()+'<img src="images/arbres_4.jpg" width="300px">\n');
});

$('#g1').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'g1');
  $('#p_mod_builder').html($('#p_mod_builder').html()+'<img src="images/gr_parents_1.jpg" width="300px">\n');
});

$('#g2').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'g2');
  $('#p_mod_builder').html($('#p_mod_builder').html()+'<img src="images/gr_parents_2.jpg" width="300px">\n');
});

$('#l1').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'l1');
  $('#p_mod_builder').html($('#p_mod_builder').html()+'<img src="images/ligne_1.jpg" width="300px">\n');
});

$('#d1').on('click', function ()
{ $('#p_mod').val($('#p_mod').val()+'d1');
  $('#p_mod_builder').html($('#p_mod_builder').html()+'<img src="images/data_3col_1.jpg" width="300px">\n');
});
