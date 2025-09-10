/* $Id: js_perso_accesskey.js,v 7.00 2015/02/12 21:59:05 mr Exp $ */
    function acc_key(thi,event,xsib,xspo,xchi,xmod_ind,xadd_par,xadd_fam,xmod_fam) {
      var key = event.keyCode ? event.keyCode : event.which ? event.which : event.charCode;
      var charKey = String.fromCharCode(key);
      var acckey = (document.getElementById("acckey")) ? document.getElementById("acckey") : thi;
      var add_par = (document.getElementById(xadd_par)) ? document.getElementById(xadd_par) : thi;
      var add_fam = (document.getElementById(xadd_fam)) ? document.getElementById(xadd_fam) : thi;
      var anc_tree = (document.getElementById("anc_tree")) ? document.getElementById("anc_tree") : thi;
      var chi = (document.getElementById(xchi)) ? document.getElementById(xchi) : thi;
      var hist = (document.getElementById("hist")) ? document.getElementById("hist") : thi;
      var mod_fam = (document.getElementById(xmod_fam)) ? document.getElementById(xmod_fam) : thi;
      var mod_fam2 = (document.getElementById("mod_fam2")) ? document.getElementById("mod_fam2") : thi;
      var mod_fam3 = (document.getElementById("mod_fam3")) ? document.getElementById("mod_fam3") : thi;
      var mod_ind = (document.getElementById(xmod_ind)) ? document.getElementById(xmod_ind) : thi;
      var nav_sosa_ref = (document.getElementById("nav_sosa_ref")) ? document.getElementById("nav_sosa_ref") : thi;
      var par_2 = (document.getElementById("par_2")) ? document.getElementById("par_2") : thi;
      var par_3 = (document.getElementById("par_3")) ? document.getElementById("par_3") : thi;
      var par_4 = (document.getElementById("par_4")) ? document.getElementById("par_4") : thi;
      var par_5 = (document.getElementById("par_5")) ? document.getElementById("par_5") : thi;
      var par_6 = (document.getElementById("par_6")) ? document.getElementById("par_6") : thi;
      var par_7 = (document.getElementById("par_7")) ? document.getElementById("par_7") : thi;
      var rela_comp = (document.getElementById("rela_comp")) ? document.getElementById("rela_comp") : thi;
      var snd_image = (document.getElementById("snd_image")) ? document.getElementById("snd_image") : thi;
      var spo = (document.getElementById(xspo)) ? document.getElementById(xspo) : thi;
      var NGn = (document.getElementById("NGn")) ? document.getElementById("NGn") : thi;
      if(document.getElementById(xsib))
        {var sib = document.getElementById(xsib);}
      else
        {var sib = document.getElementById("sib_1") ? document.getElementById("sib_1") : thi;}
      var sosa = "";
      if(document.getElementsByName("sosa")[0])
      {
        if(thi.getAttribute("name") == "sosa")
        {
           var nb_sosa = document.getElementsByName("sosa").length;
           var i = 0;
           var j = 0;
           var sosa0 = "";
           while (j < 2 && i < nb_sosa)
           {
             sosa0 = document.getElementsByName("sosa")[i].id;
             if(j == 1)
               {j = 2;}
             if(sosa0 == thi.id)
               {j = 1;}
             if(sosa0 == thi.id && i == nb_sosa -1)
               {sosa0 = document.getElementsByName("sosa")[0].id;}
             i++;
           }
           sosa = document.getElementById(sosa0);
        }
        else
        {  var sosa0 = document.getElementsByName("sosa")[0].id;
           sosa = document.getElementById(sosa0);
        }
      }
      else
      {sosa = thi;}
      
      switch (charKey)
      {
        //shift = on
        case "Z": acckey.focus(); break;  // menu2 (z)
        case "W": mSn.focus(); break;     // trl : search input
        case "1": sib.focus(); break;
        case "2": par_2.focus(); break;
        case "3": par_3.focus(); break;
        case "4": par_4.focus(); break;
        case "5": par_5.focus(); break;
        case "6": par_6.focus(); break;
        case "7": par_7.focus(); break;
        case "8": chi.focus(); break;
        case "9": sosa.focus(); break;
        case "0": spo.focus(); break;
        case "D": hist.click(); break;
        case "P": mod_ind.click(); break;
        case "L": add_par.click(); break;
        case "A": add_fam.click(); break;
        case "F": mod_fam.click(); break;
        case "G": mod_fam_2.click(); break;
        case "H": mod_fam_3.click(); break;
        case "I": snd_image.click(); break;
        case "R": rela_comp.click(); break;
        case "S": nav_sosa_ref.click(); break;
        case "X": maxlev.select(); break;
        case "Y": anc_tree.click(); break;
        //shift = off
        case "z": acckey.focus(); break;  // menu2 (z)
        case "w": mSn.focus(); break;     // trl : search input
        case "&": sib.focus(); break; break; 
        case "é": par_2.focus(); break;
        case "\"": par_3.focus(); break;
        case "'": par_4.focus(); break;
        case "(": par_5.focus(); break;
        case "-": par_6.focus(); break;
        case "§": par_6.focus(); break;   // macbook key
        case "è": par_7.focus(); break;
        case "_": chi.focus(); break;
        case "!": chi.focus(); break;     // macbook key
        case "ç": sosa.focus(); break;
        case "à": spo.focus(); break;
        case "d": hist.click(); break;
        case "p": mod_ind.click(); break;
        case "l": add_par.click(); break;
        case "a": add_fam.click(); break;
        case "f": mod_fam.click(); break;
        case "g": mod_fam_2.click(); break;
        case "h": mod_fam_3.click(); break;
        case "i": snd_image.click(); break;
        case "r": rela_comp.click(); break;
        case "s": nav_sosa_ref.click(); break;
        case "x": maxlev.select(); break;
        case "y": anc_tree.click(); break;
        default: return true; break;
      }
      return false;
    }