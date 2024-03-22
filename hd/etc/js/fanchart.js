const fanchart = document.getElementById( "fanchart" );
const places_list = document.getElementById( "places_list" );
var sheet;
for( var i in document.styleSheets ) {
	if( document.styleSheets[i].title == "fc-auto" ) {
        	sheet = document.styleSheets[i];
		break;
	}
}
const root = document.documentElement;

var pixel = document.getElementById( "pixel" ).getContext( "2d" );
function relativeLuminance( color ) {
	pixel.fillStyle = color;
	pixel.fillRect( 0, 0, 1, 1 );
	const data = pixel.getImageData(0, 0, 1, 1).data;
	const rsrgb = data[0] / 255;
	const gsrgb = data[1] / 255;
	const bsrgb = data[2] / 255;
	const r = rsrgb <= 0.03928 ? rsrgb / 12.92 : Math.pow((rsrgb + 0.055) / 1.055, 2.4);
	const g = gsrgb <= 0.03928 ? gsrgb / 12.92 : Math.pow((gsrgb + 0.055) / 1.055, 2.4);
	const b = bsrgb <= 0.03928 ? bsrgb / 12.92 : Math.pow((bsrgb + 0.055) / 1.055, 2.4);
	return r * 0.2126 + g * 0.7152 + b * 0.0722;
}
function contrastRatio( color1, color2 ) {
	return (relativeLuminance(color1) + 0.05) / (relativeLuminance(color2) + 0.05);
}

function pos_x( r, a ) {
	return center_x + r * Math.cos( Math.PI / 180 * a );
}
function pos_y( r, a ) {
	return center_y + r * Math.sin( Math.PI / 180 * a );
}
function up( g, r, a1, a2, sosa, p ) {
	l = path1( g, "tpiS"+sosa, r, a1, a2 );
	link( g, "tpiS"+sosa, p, l );
}
function no_up( g, r, a1, a2, sosa, p ) {
	l = path1( g, "tpiS"+sosa, r, a1, a2 );
	no_link( g, "tpiS"+sosa, p, l );
}

function g( id ) {
	var g = document.createElementNS("http://www.w3.org/2000/svg", "g");
	g.setAttribute( "id", id );
	fanchart.append(g);

	return g;
}

function deathAgeClass( age ) {
	var n = Math.trunc( age / 15 );
	if( n > 7 ) {
		n = 7;
	}
	return "DA"+n;
}

function pie_bg( g, r1, r2, a1, a2, p ) {
	var path = document.createElementNS("http://www.w3.org/2000/svg", "path");
	path.setAttribute( "d",
		 'M ' + pos_x(r2, a1) + ',' + pos_y(r2, a1) +
		' A ' + r2 + ' ' + r2 + ' 0 ' + (a2 - a1 > 180 ? 1 : 0) + ' 1 ' + pos_x(r2, a2) + ',' + pos_y(r2, a2) +
		' L ' + pos_x(r1, a2) + ',' + pos_y(r1, a2) +
		' A ' + r1 + ' ' + r1 + ' 0 ' + (a2 - a1 > 180 ? 1 : 0) + ' 0 ' + pos_x(r1, a1) + ',' + pos_y(r1, a1) +
		' Z'
	);
	var c = "bg";
	if( p.birth_place !== undefined && p.birth_place != "" ) {
		c += " bi-"+lieux[p.birth_place].c;
	}
	if( p.baptism_place !== undefined && p.baptism_place != "" ) {
		c += " ba-"+lieux[p.baptism_place].c;
	}
	if( p.death_place !== undefined && p.death_place != "" ) {
		c += " de-"+lieux[p.death_place].c;
	}
	if( p.burial_place !== undefined && p.burial_place != "" ) {
		c += " bu-"+lieux[p.burial_place].c;
	}
	if( p.death_age !== undefined && p.death_age != "" ) {
		c += " "+deathAgeClass(p.death_age);
	}
	path.setAttribute( "class", c );
	g.append(path);
}
function pie( g, r1, r2, a1, a2, p ) {
	var path = document.createElementNS("http://www.w3.org/2000/svg", "path");
	path.setAttribute( "d",
		 'M ' + pos_x(r2, a1) + ',' + pos_y(r2, a1) +
		' A ' + r2 + ' ' + r2 + ' 0 ' + (a2 - a1 > 180 ? 1 : 0) + ' 1 ' + pos_x(r2, a2) + ',' + pos_y(r2, a2) +
		' L ' + pos_x(r1, a2) + ',' + pos_y(r1, a2) +
		' A ' + r1 + ' ' + r1 + ' 0 ' + (a2 - a1 > 180 ? 1 : 0) + ' 0 ' + pos_x(r1, a1) + ',' + pos_y(r1, a1) +
		' Z'
	);
	path.setAttribute( "class", "link" );
	g.append(path);
	path.onclick = function( e ) {
		if( true == e.shiftKey ) {
			var oc = p.oc;
			if( oc != "" && oc != 0 ) { oc = "&oc=" + oc } else { oc = "" }
			window.location = link_to_person + "p=" + p.fnk + "&n=" + p.snk + oc
		}
	};
	path.onmouseenter = function() {
		if( p.birth_place !== undefined && p.birth_place != "" ) {
			document.getElementById( "bi-" + lieux[p.birth_place].c ).classList.remove("hidden");
		}
		if( p.baptism_place !== undefined && p.baptism_place != "" ) {
			document.getElementById( "ba-" + lieux[p.baptism_place].c ).classList.remove("hidden");
		}
		if( p.marriage_place !== undefined && p.marriage_place != "" ) {
			document.getElementById( "ma-" + lieux[p.marriage_place].c ).classList.remove("hidden");
		}
		if( p.death_place !== undefined && p.death_place != "" ) {
			document.getElementById( "de-" + lieux[p.death_place].c ).classList.remove("hidden");
		}
		if( p.burial_place !== undefined && p.burial_place != "" ) {
			document.getElementById( "bu-" + lieux[p.burial_place].c ).classList.remove("hidden");
		}
		if( p.death_age !== undefined && p.death_age != "" ) {
			var c = deathAgeClass(p.death_age);
			document.getElementById( c ).classList.add("hl");
		}
	};
	path.onmouseleave = function() {
		if( p.birth_place !== undefined && p.birth_place != "" ) {
			document.getElementById( "bi-" + lieux[p.birth_place].c ).classList.add("hidden");
		}
		if( p.baptism_place !== undefined && p.baptism_place != "" ) {
			document.getElementById( "ba-" + lieux[p.baptism_place].c ).classList.add("hidden");
		}
		if( p.marriage_place !== undefined && p.marriage_place != "" ) {
			document.getElementById( "ma-" + lieux[p.marriage_place].c ).classList.add("hidden");
		}
		if( p.death_place !== undefined && p.death_place != "" ) {
			document.getElementById( "de-" + lieux[p.death_place].c ).classList.add("hidden");
		}
		if( p.burial_place !== undefined && p.burial_place != "" ) {
			document.getElementById( "bu-" + lieux[p.burial_place].c ).classList.add("hidden");
		}
		if( p.death_age !== undefined && p.death_age != "" ) {
			var c = deathAgeClass(p.death_age);
			document.getElementById( c ).classList.remove("hl");
		}
	};

	if( p.fn == "=" ) {
		path.addEventListener( "mouseenter", function() {
			var ref = document.getElementById( "S"+p.sn );
			ref.classList.add( "same_hl" );
		});
		path.addEventListener( "mouseout", function() {
			var ref = document.getElementById( "S"+p.sn );
			ref.classList.remove( "same_hl" );
		});
	}
}
function pie_m_bg( g, r1, r2, a1, a2, p ) {
	var path = document.createElementNS("http://www.w3.org/2000/svg", "path");
	path.setAttribute( "d",
		 'M ' + pos_x(r2, a1) + ',' + pos_y(r2, a1) +
		' A ' + r2 + ' ' + r2 + ' 0 ' + (a2 - a1 > 180 ? 1 : 0) + ' 1 ' + pos_x(r2, a2) + ',' + pos_y(r2, a2) +
		' L ' + pos_x(r1, a2) + ',' + pos_y(r1, a2) +
		' A ' + r1 + ' ' + r1 + ' 0 ' + (a2 - a1 > 180 ? 1 : 0) + ' 0 ' + pos_x(r1, a1) + ',' + pos_y(r1, a1) +
		' Z'
	);
	var c = "";
	if( p.marriage_place !== undefined && p.marriage_place != "" ) {
		c += " ma-"+lieux[p.marriage_place].c;
	}
	path.setAttribute( "class", c );
	g.append(path);
}
function pie_m( g, r1, r2, a1, a2, p ) {
	var path = document.createElementNS("http://www.w3.org/2000/svg", "path");
	path.setAttribute( "d",
		 'M ' + pos_x(r2, a1) + ',' + pos_y(r2, a1) +
		' A ' + r2 + ' ' + r2 + ' 0 ' + (a2 - a1 > 180 ? 1 : 0) + ' 1 ' + pos_x(r2, a2) + ',' + pos_y(r2, a2) +
		' L ' + pos_x(r1, a2) + ',' + pos_y(r1, a2) +
		' A ' + r1 + ' ' + r1 + ' 0 ' + (a2 - a1 > 180 ? 1 : 0) + ' 0 ' + pos_x(r1, a1) + ',' + pos_y(r1, a1) +
		' Z'
	);
	g.append(path);
	path.onmouseenter = function() {
		if( p.marriage_place !== undefined && p.marriage_place != "" ) {
			document.getElementById( "ma-" + lieux[p.marriage_place].c ).classList.remove("hidden");
		}
	};
	path.onmouseleave = function() {
		if( p.marriage_place !== undefined && p.marriage_place != "" ) {
			document.getElementById( "ma-" + lieux[p.marriage_place].c ).classList.add("hidden");
		}
	};
}
function pie_contour( g, r1, r2, a1, a2 ) {
	var path = document.createElementNS("http://www.w3.org/2000/svg", "path");
	path.setAttribute( "d",
		 'M ' + pos_x(r2, a1) + ',' + pos_y(r2, a1) +
		' A ' + r2 + ' ' + r2 + ' 0 ' + (a2 - a1 > 180 ? 1 : 0) + ' 1 ' + pos_x(r2, a2) + ',' + pos_y(r2, a2) +
		' L ' + pos_x(r1, a2) + ',' + pos_y(r1, a2) +
		' A ' + r1 + ' ' + r1 + ' 0 ' + (a2 - a1 > 180 ? 1 : 0) + ' 0 ' + pos_x(r1, a1) + ',' + pos_y(r1, a1) +
		' Z'
	);
	path.setAttribute( "class", "contour" );
	g.append(path);
}
function pie_middle( g, r1, r2, a ) {
	var path = document.createElementNS("http://www.w3.org/2000/svg", "path");
	path.setAttribute( "d",
		 'M ' + pos_x(r2, a) + ',' + pos_y(r2, a) +
		' L ' + pos_x(r1, a) + ',' + pos_y(r1, a)
	);
	path.setAttribute( "class", "middle" );
	g.append(path);
}
function circle_bg( g, r, cx, cy, p ) {
	var circle = document.createElementNS("http://www.w3.org/2000/svg", "circle");
	circle.setAttribute( "cx", cx );
	circle.setAttribute( "cy", cy );
	circle.setAttribute( "r", r );
	var c = "bg";
	if( p.birth_place !== undefined && p.birth_place != "" ) {
		c += " bi-"+lieux[p.birth_place].c;
	}
	if( p.baptism_place !== undefined && p.baptism_place != "" ) {
		c += " ba-"+lieux[p.baptism_place].c;
	}
	if( p.death_place !== undefined && p.death_place != "" ) {
		c += " de-"+lieux[p.death_place].c;
	}
	if( p.burial_place !== undefined && p.burial_place != "" ) {
		c += " bu-"+lieux[p.burial_place].c;
	}
	if( p.death_age !== undefined && p.death_age != "" ) {
		c += " "+deathAgeClass(p.death_age);
	}
	circle.setAttribute( "class", c );
	g.append(circle);
}
function circle( g, r, cx, cy, p ) {
	var circle = document.createElementNS("http://www.w3.org/2000/svg", "circle");
	circle.setAttribute( "cx", cx );
	circle.setAttribute( "cy", cy );
	circle.setAttribute( "r", r );
	circle.setAttribute( "class", "link" );
	g.append(circle);
	circle.onclick = function( e ) {
		if( true == e.shiftKey ) {
			var oc = p.oc;
			if( oc != "" && oc != 0 ) { oc = "&oc=" + oc } else { oc = "" }
			window.location = link_to_person + "p=" + p.fnk + "&n=" + p.snk + oc
		}
	};
	circle.onmouseenter = function() {
		if( p.birth_place !== undefined && p.birth_place != "" ) {
			document.getElementById( "bi-" + lieux[p.birth_place].c ).classList.remove("hidden");
		}
		if( p.baptism_place !== undefined && p.baptism_place != "" ) {
			document.getElementById( "ba-" + lieux[p.baptism_place].c ).classList.remove("hidden");
		}
		if( p.death_place !== undefined && p.death_place != "" ) {
			document.getElementById( "de-" + lieux[p.death_place].c ).classList.remove("hidden");
		}
		if( p.burial_place !== undefined && p.burial_place != "" ) {
			document.getElementById( "bu-" + lieux[p.burial_place].c ).classList.remove("hidden");
		}
		if( p.death_age !== undefined && p.death_age != "" ) {
			var c = deathAgeClass(p.death_age);
			document.getElementById( c ).classList.add("hl");
		}
	};
	circle.onmouseleave = function() {
		if( p.birth_place !== undefined && p.birth_place != "" ) {
			document.getElementById( "bi-" + lieux[p.birth_place].c ).classList.add("hidden");
		}
		if( p.baptism_place !== undefined && p.baptism_place != "" ) {
			document.getElementById( "ba-" + lieux[p.baptism_place].c ).classList.add("hidden");
		}
		if( p.death_place !== undefined && p.death_place != "" ) {
			document.getElementById( "de-" + lieux[p.death_place].c ).classList.add("hidden");
		}
		if( p.burial_place !== undefined && p.burial_place != "" ) {
			document.getElementById( "bu-" + lieux[p.burial_place].c ).classList.add("hidden");
		}
		if( p.death_age !== undefined && p.death_age != "" ) {
			var c = deathAgeClass(p.death_age);
			document.getElementById( c ).classList.remove("hl");
		}
	};
}
function text_S1( g, x, y, p ) {
	var text = document.createElementNS("http://www.w3.org/2000/svg", "text");
	text.setAttribute( "x", x );
	text.setAttribute( "y", y );
	var c = "";
	if( p.birth_place !== undefined && p.birth_place != "" ) {
		c += " bi-t"+lieux[p.birth_place].c;
	}
	if( p.baptism_place !== undefined && p.baptism_place != "" ) {
		c += " ba-t"+lieux[p.baptism_place].c;
	}
	if( p.death_place !== undefined && p.death_place != "" ) {
		c += " de-t"+lieux[p.death_place].c;
	}
	if( p.burial_place !== undefined && p.burial_place != "" ) {
		c += " bu-t"+lieux[p.burial_place].c;
	}
	text.setAttribute( "class", c );
	var ts1 = 100;
        standard.textContent = p.fn;
	if( standard.getBBox().width > 2*a_r[0]*security ) {
		ts1 = Math.round( 100 * 2*a_r[0]*security / standard.getBBox().width );
	}
	var ts2 = 100;
        standard.textContent = p.sn;
	if( standard.getBBox().width > 2*a_r[0]*security ) {
		ts2 = Math.round( 100 * 2*a_r[0]*security / standard.getBBox().width );
	}
	text.innerHTML = '<tspan style="font-size:'+ts1+'%">' + p.fn + '</tspan><tspan x="' + x + '" dy="15" style="font-size:'+ts2+'%">' + p.sn + '</tspan><tspan class="dates" x="' + x + '" dy="15">' + p.dates + '</tspan>';
	g.append(text);
}

function path1( g, id, r, a1, a2 ) {
	var path = document.createElementNS("http://www.w3.org/2000/svg", "path");
	path.setAttribute( "class", "none" );
	path.setAttribute( "d",
		 'M ' + pos_x(r, a1) + ',' + pos_y(r, a1) +
		' A ' + r + ' ' + r + ' 0 ' + (a2 - a1 > 180 ? 1 : 0) + ' 1 ' + pos_x(r, a2) + ',' + pos_y(r, a2)
	);
	path.setAttribute( "id", id );
	g.append(path);

	return Math.abs(a2-a1)/360*2*Math.PI*r;
}
function path2( g, id, r1, r2, a ) {
	var path = document.createElementNS("http://www.w3.org/2000/svg", "path");
	path.setAttribute( "class", "none" );
	path.setAttribute( "d",
		 'M ' + pos_x(r1, a) + ',' + pos_y(r1, a) +
		' L ' + pos_x(r2, a) + ',' + pos_y(r2, a)
	);
	path.setAttribute( "id", id );
	g.append(path);

	return Math.abs(r2-r1);
}
function text2( g, pid, t, c, l, h ) {
        standard.textContent = t;
	var ts_l = 100;
	if( standard.getBBox().width > l*security ) {
		ts_l = Math.round( 100 * l*security / standard.getBBox().width );
	}
	var ts_h = 100;
	if( standard.getBBox().height > h*security ) {
		ts_h = Math.round( 100 * h*security / standard.getBBox().height );
	}

	var text = document.createElementNS("http://www.w3.org/2000/svg", "text");
	text.setAttribute( "class", "text "+c  );
	text.innerHTML = '<textPath xlink:href="#' + pid + '" startOffset="50%" style="font-size:'+Math.min(ts_l,ts_h)+'%;">' + t+ '</textPath>';
	g.append(text);
}
function link( g, pid, p, l ) {
	var ts = 100;
	if( 2 * standard_width > l ) {
		ts = Math.round( 100 * l / 2 / standard_width );
	}

	var text = document.createElementNS("http://www.w3.org/2000/svg", "text");
	text.setAttribute( "class", "link icon"  );
	text.innerHTML = '<textPath xlink:href="#' + pid + '" startOffset="50%" style="font-size:'+ts+'%;">&#x25B2;</textPath>';
	g.append(text);
	text.onclick = function ( e ) {
		var oc = p.oc;
		if( oc != "" && oc != 0 ) { oc = "&oc=" + oc } else { oc = "" }
		window.location = link_to_fanchart + "p=" + p.fnk + "&n=" + p.snk + oc + "&v=" + max_gen + "&tool=" + tool +
			(has_ba ? "&ba=on" : "") +
			(has_bu ? "&bu=on" : "");
	};
}
function no_link( g, pid, p, l ) {
	var ts = 100;
	if( 2 * standard_width > l ) {
		ts = Math.round( 100 * l / 2 / standard_width );
	}

	var text = document.createElementNS("http://www.w3.org/2000/svg", "text");
	text.setAttribute( "class", "no-link"  );
	text.innerHTML = '<textPath xlink:href="#' + pid + '" startOffset="50%" style="font-size:'+ts+'%;">&#x2716;</textPath>';
	g.append(text);
}
function text_C3( g, r1, r2, a1, a2, sosa, p, c ) {
	var l, h;
	h = Math.abs(r2-r1)/3;
	l = path1( g, "tp1S"+sosa, (r2-r1)*3/4 + r1, a1, a2 );
	text2( g, "tp1S"+sosa, p.fn, c, l, h );
	l = path1( g, "tp2S"+sosa, (r2-r1)*2/4 + r1, a1, a2 );
	text2( g, "tp2S"+sosa, p.sn, c, l, h );
	l = path1( g, "tp3S"+sosa, (r2-r1)/4 + r1, a1, a2 );
	text2( g, "tp3S"+sosa, p.dates, c+" dates", l, h );
}
function text_R3( g, r1, r2, a1, a2, sosa, p, c ) {
	var my_r1, my_r2, my_a1, my_a2, my_a3, l, h;
	if( a1 >= -90 ) {
		my_r1 = r1;
		my_r2 = r2;
		my_a3 = a2 - (a2-a1)/4;
		my_a2 = a2 - (a2-a1)*2/4;
		my_a1 = a2 - (a2-a1)*3/4;
	} else {
		my_r1 = r2;
		my_r2 = r1;
		my_a3 = a1 + (a2-a1)/4;
		my_a2 = a1 + (a2-a1)*2/4;
		my_a1 = a1 + (a2-a1)*3/4;
	}
	h = Math.abs(a2-a1)/360*2*Math.PI*r1 / 3;
	l = path2( g, "tp1S"+sosa, my_r1, my_r2, my_a1 );
	text2( g, "tp1S"+sosa, p.fn, c, l, h );
	l = path2( g, "tp2S"+sosa, my_r1, my_r2, my_a2 );
	text2( g, "tp2S"+sosa, p.sn, c, l, h );
	l = path2( g, "tp3S"+sosa, my_r1, my_r2, my_a3 );
	text2( g, "tp3S"+sosa, p.dates, c+" dates", l, h );
}
function text_R2( g, r1, r2, a1, a2, sosa, p, c ) {
	var my_r1, my_r2, my_a1, my_a2, m, l;
	if( a1 >= -90 ) {
		my_r1 = r1;
		my_r2 = r2;
		my_a2 = a2 - (a2-a1)/3;
		my_a1 = a2 - (a2-a1)*2/3;
	} else {
		my_r1 = r2;
		my_r2 = r1;
		my_a2 = a1 + (a2-a1)/3
		my_a1 = a1 + (a2-a1)*2/3;
	}
	h = Math.abs(a2-a1)/360*2*Math.PI*r1 / 2;
	l = path2( g, "tp1S"+sosa, my_r1, my_r2, my_a1 );
	text2( g, "tp1S"+sosa, p.fn + ' ' + p.sn, c, l, h );
	l = path2( g, "tp2S"+sosa, my_r1, my_r2, my_a2 );
	text2( g, "tp2S"+sosa, p.dates, c+" dates", l, h );
}
function text_R1( g, r1, r2, a1, a2, sosa, p, c ) {
	var my_r1, my_r2, my_a1, my_a2, l, h;
	if( a1 >= -90 ) {
		my_r1 = r1;
		my_r2 = r2;
		my_a1 = a2 - (a2-a1)/2;
	} else {
		my_r1 = r2;
		my_r2 = r1;
		my_a1 = a1 + (a2-a1)/2;
	}
	h = Math.abs(a2-a1)/360*2*Math.PI*r1;
	l = path2( g, "tp1S"+sosa, my_r1, my_r2, my_a1 );
	text2( g, "tp1S"+sosa, p.fn + ' ' + p.sn, c, l, h );
}

function zoom (zx, zy, factor, direction) {
	var w = svg_viewbox_w;
	var h = svg_viewbox_h;
	if( direction > 0 ) {
		h = Math.round(h/factor);
		w = Math.round(w/factor);
	} else {
		h = Math.round(h*factor);
		w = Math.round(w*factor);
	}
	set_svg_viewbox(
		svg_viewbox_x + Math.round(zx * (svg_viewbox_w - w) / window_w),
		svg_viewbox_y + Math.round(zy * (svg_viewbox_h - h) / window_h),
		w, h );
}

fanchart.addEventListener( "wheel", function( event ) {
	zoom( event.clientX, event.clientY, zoom_factor, (event.deltaY < 0 ? +1 : -1) );
}, { passive: false });

var drag_state = false;
fanchart.onmousedown = function(e) {
	e.preventDefault();
	drag_state = true;
};
fanchart.onmouseup = function() {
	drag_state = false;
};
fanchart.onmousemove = function(e) {
	if( drag_state ) {
		e.preventDefault();
		set_svg_viewbox( svg_viewbox_x - Math.round(e.movementX * svg_viewbox_w / window_w),
                                 svg_viewbox_y - Math.round(e.movementY * svg_viewbox_h / window_h),
                                 svg_viewbox_w, svg_viewbox_h );
	}
};

const security = 0.95;
const zoom_factor = 1.25;
const d_all = 220;
const a_r = [   50,   50,   50,   50,   80,   70,  100,  150,  130,   90 ];
const a_m = [ "S1", "C3", "C3", "C3", "R3", "R3", "R2", "R1", "R1", "R1" ];

var ak = Object.keys(ancestor)
max_gen = Math.trunc(Math.log(Number(ak[ak.length-1].replace( /^S/, "")))/Math.log(2));

var max_r = 0 ;
for( var i = 0 ; i < max_gen+1 && i < a_r.length ; i++ ) {
	max_r += a_r[i];
}

const svg_margin = 5;
const center_x = max_r + svg_margin;
const center_y = max_r + svg_margin;

const svg_w = 2 * center_x;
const svg_h = 2 * svg_margin + max_r +
              Math.max( a_r[0], Math.round( max_r * Math.sin(Math.PI/180*(d_all-180)/2) ) );
const svg_ratio = svg_w / svg_h;

var svg_viewbox_x, svg_viewbox_y, svg_viewbox_w, svg_viewbox_h;
function set_svg_viewbox( x, y, w, h ) {
	svg_viewbox_x = x;
	svg_viewbox_y = y;
	svg_viewbox_w = w;
	svg_viewbox_h = h;
	fanchart.setAttribute( "viewBox", x + " " + y + " " + w + " " + h );
}
function fitScreen() {
	set_svg_viewbox( 0, 0, svg_w, svg_h );
}

var lieux = {};
var has_bi = false;
var has_ba = false;
var has_ma = false;
var has_de = false;
var has_bu = false;
ak.forEach( function(s) {
	var p = ancestor[s];
	if( p.birth_place !== undefined ) {
		has_bi = true;
		ancestor[s].birth_place = p.birth_place.replace( /^\?, /, "" );
	}
	if( p.baptism_place !== undefined ) {
		has_ba = true;
		ancestor[s].baptism_place = p.baptism_place.replace( /^\?, /, "" );
	}
	if( p.marriage_place !== undefined ) {
		has_ma = true;
		ancestor[s].marriage_place = p.marriage_place.replace( /^\?, /, "" );
	}
	if( p.death_place !== undefined ) {
		has_de = true;
		ancestor[s].death_place = p.death_place.replace( /^\?, /, "" );
	}
	if( p.burial_place !== undefined ) {
		has_bu = true;
		ancestor[s].burial_place = p.burial_place.replace( /^\?, /, "" );
	}
	if( p.death_age !== undefined ) {
		ancestor[s].death_age = p.death_age.replace( /[^0123456789]/g, "" );
	}
	ancestor[s].dates = p.dates.replace( /\s?<\/?bdo[^>]*>/g, "" );
	p = ancestor[s];

	if( p.birth_place !== undefined && p.birth_place != "" ) {
		if( lieux[p.birth_place] === undefined ) {
			lieux[p.birth_place] = { "cnt": 1, "bi": true };
		} else {
			lieux[p.birth_place].cnt++;
			lieux[p.birth_place].bi = true;
		}
	}
	if( p.baptism_place !== undefined && p.baptism_place != "" ) {
		if( lieux[p.baptism_place] === undefined ) {
			lieux[p.baptism_place] = { "cnt": 1, "ba": true };
		} else {
			lieux[p.baptism_place].cnt++;
			lieux[p.baptism_place].ba = true;
		}
	}
	if( p.marriage_place !== undefined && p.marriage_place != "" ) {
		if( lieux[p.marriage_place] === undefined ) {
			lieux[p.marriage_place] = { "cnt": 1, "ma": true };
		} else {
			lieux[p.marriage_place].cnt++;
			lieux[p.marriage_place].ma = true;
		}
	}
	if( p.death_place !== undefined && p.death_place != "" ) {
		if( lieux[p.death_place] === undefined ) {
			lieux[p.death_place] = { "cnt": 1, "de": true };
		} else {
			lieux[p.death_place].cnt++;
			lieux[p.death_place].de = true;
		}
	}
	if( p.burial_place !== undefined && p.burial_place != "" ) {
		if( lieux[p.burial_place] === undefined ) {
			lieux[p.burial_place] = { "cnt": 1, "bu": true };
		} else {
			lieux[p.burial_place].cnt++;
			lieux[p.burial_place].bu = true;
		}
	}
});
var lieux_a = [];
for( var key in lieux ) {
	lieux_a.push([key, lieux[key]]);
}
lieux_a.sort( function(e1,e2) {
	return e2[1].cnt - e1[1].cnt
});
var c_h = 0;
var c_dh = 60;
var c_l = 90;
lieux_a.forEach( function( l, i ) {
	lieux[l[0]].c = "L"+i;
	var li = document.createElement( "li" );
	li.innerHTML =
		(has_bi ? '<span id="bi-L'+i+'" class="hidden">N</span>' : '') +
		(has_ba ? '<span id="ba-L'+i+'" class="hidden">B</span>' : '') +
		(has_ma ? '<span id="ma-L'+i+'" class="hidden">M</span>' : '') +
		(has_de ? '<span id="de-L'+i+'" class="hidden">D</span>' : '') +
		(has_bu ? '<span id="bu-L'+i+'" class="hidden">S</span>' : '') +
		'<span class="square">■</span> ' + l[0];
	li.setAttribute( "id", "L"+i );
	li.setAttribute( "title", lieux[l[0]].cnt + " occurence(s)" );
	li.onmouseenter = function() {
		[ "bi", "ba", "ma", "de", "bu" ].forEach( function(ev) {
			var a = document.getElementsByClassName( ev+"-L"+i );
			for( var e of a ) {
				e.classList.add( "highlight" );
			}
			var a = document.getElementsByClassName( ev+"-tL"+i );
			for( var e of a ) {
				e.classList.add( "text_highlight" );
			}
		});
		if( lieux[l[0]].bi !== undefined ) {
			document.getElementById( "bi-L"+i ).classList.remove( "hidden" );
		}
		if( lieux[l[0]].ba !== undefined ) {
			document.getElementById( "ba-L"+i ).classList.remove( "hidden" );
		}
		if( lieux[l[0]].ma !== undefined ) {
			document.getElementById( "ma-L"+i ).classList.remove( "hidden" );
		}
		if( lieux[l[0]].de !== undefined ) {
			document.getElementById( "de-L"+i ).classList.remove( "hidden" );
		}
		if( lieux[l[0]].bu !== undefined ) {
			document.getElementById( "bu-L"+i ).classList.remove( "hidden" );
		}
	};
	li.onmouseleave = function() {
		[ "bi", "ba", "ma", "de", "bu" ].forEach( function(ev) {
			var a = document.getElementsByClassName( ev+"-L"+i );
			for( var e of a ) {
				e.classList.remove( "highlight" );
			}
			var a = document.getElementsByClassName( ev+"-tL"+i );
			for( var e of a ) {
				e.classList.remove( "text_highlight" );
			}
		});
		if( lieux[l[0]].bi !== undefined ) {
			document.getElementById( "bi-L"+i ).classList.add( "hidden" );
		}
		if( lieux[l[0]].ba !== undefined ) {
			document.getElementById( "ba-L"+i ).classList.add( "hidden" );
		}
		if( lieux[l[0]].ma !== undefined ) {
			document.getElementById( "ma-L"+i ).classList.add( "hidden" );
		}
		if( lieux[l[0]].de !== undefined ) {
			document.getElementById( "de-L"+i ).classList.add( "hidden" );
		}
		if( lieux[l[0]].bu !== undefined ) {
			document.getElementById( "bu-L"+i ).classList.add( "hidden" );
		}
	};
	places_list.append( li );

	sheet.insertRule( 'body.place_color svg.bi .bi-L'+i+'  {fill: var(--fc-color-'+i+', transparent);}' );
	sheet.insertRule( 'body.place_color svg.bi .bi-tL'+i+'  {fill: var(--fc-text-color-'+i+', black);}' );
	if( has_ba ) {
		sheet.insertRule( 'body.place_color svg.ba .ba-L'+i+'  {fill: var(--fc-color-'+i+', transparent);}' );
		sheet.insertRule( 'body.place_color svg.ba .ba-tL'+i+'  {fill: var(--fc-text-color-'+i+', black);}' );
	}
	sheet.insertRule( 'body.place_color svg.ma .ma-L'+i+'  {fill: var(--fc-color-'+i+', transparent);}' );
	sheet.insertRule( 'body.place_color svg.ma .ma-tL'+i+'  {fill: var(--fc-text-color-'+i+', black);}' );
	sheet.insertRule( 'body.place_color svg.de .de-L'+i+'  {fill: var(--fc-color-'+i+', transparent);}' );
	sheet.insertRule( 'body.place_color svg.de .de-tL'+i+'  {fill: var(--fc-text-color-'+i+', black);}' );
	if( has_bu ) {
		sheet.insertRule( 'body.place_color svg.bu .bu-L'+i+'  {fill: var(--fc-color-'+i+', transparent);}' );
		sheet.insertRule( 'body.place_color svg.bu .bu-tL'+i+'  {fill: var(--fc-text-color-'+i+', black);}' );
	}
	sheet.insertRule( 'body.place_color #L'+i+' .square  { color: var(--fc-color-'+i+', transparent); }' );
	root.style.setProperty( '--fc-color-'+i, 'hsl('+c_h+',100%,'+c_l+'%)' );
	var rb = contrastRatio( 'hsl('+c_h+',100%,'+c_l+'%)', 'black' );
	var rw = contrastRatio( 'white', 'hsl('+c_h+',100%,'+c_l+'%)' );
	if( rw > rb ) {
		root.style.setProperty( '--fc-text-color-'+i, 'white' );
	}
	c_h += c_dh;
	if( c_h >= 360 ) {
		c_dh = Math.round( c_dh / 2 );
		c_h = c_dh;
		c_l -= 15;
	}
});

[ "DA0", "DA1", "DA2", "DA3", "DA4", "DA5", "DA6", "DA7" ].forEach( function( id ) {
	var el = document.getElementById( id );
	el.onmouseenter = function() {
		var a = document.getElementsByClassName( id );
		for( var e of a ) {
			e.classList.add( "highlight" );
		}
		document.getElementById( id ).classList.add("hl");
	};
	el.onmouseleave = function() {
		var a = document.getElementsByClassName( id );
		for( var e of a ) {
			e.classList.remove( "highlight" );
		}
		document.getElementById( id ).classList.remove("hl");
	};
});

var standard_height, standard_width;
var standard = document.createElementNS("http://www.w3.org/2000/svg", "text");
standard.textContent = "ABCDEFGHIJKLMNOPQRSTUVW abcdefghijklmnopqrstuvwxyz";
standard.setAttribute( "id", "standard" );
standard.setAttribute( "x", center_x );
standard.setAttribute( "y", center_y );
fanchart.append(standard);
standard_width = standard.getBBox().width / standard.textContent.length;
standard_height = standard.getBBox().height;

var gen = 1;
var sosa = 1;
var r1 = 0;
var r2 = a_r[0];
var a1, a2;
var delta = d_all;

// Sosa 1
var g1 = g( "S"+sosa );
circle_bg( g1, r2, center_x, center_y, ancestor["S"+sosa] );
text_S1( g1, center_x, center_y-10, ancestor["S"+sosa] );
circle( g1, r2, center_x, center_y, ancestor["S"+sosa] );

while( true ) {
	sosa++;
	if( sosa >= (2 ** gen) ) {
		gen++;
		if( gen >= a_r.length+1 ) {
			break;
		}
		delta = delta / 2;
		r1 = r2;
		r2 = r1 + a_r[gen-1];
		a1 = -90 - d_all/2;
		a2 = a1 + delta;
	} else {
		a1 += delta;
		a2 += delta;
	}
	var p = ancestor["S"+sosa];
	if( p !== undefined ) {
		var pg = g( "S"+sosa );
		var same = (p.fn == "=" ? true : false);
		if( same &&  implex != "" ) {
			var p2 = ancestor["S"+(2 * p.sn)];
			if( p2 !== undefined ) {
				ancestor["S"+(2*sosa)] = { "fn" : "=", "sn": 2*p.sn, "fnk": p2.fnk, "snk": p2.snk, "oc": p2.oc, "dates": "", "has_parents": p2.has_parents };
			}
			p2 = ancestor["S"+(2*p.sn+1)];
			if( p2 !== undefined ) {
				ancestor["S"+(2*sosa+1)] = { "fn" : "=", "sn": 2*p.sn+1, "fnk": p2.fnk, "snk": p2.snk, "oc": p2.oc, "dates": "", "has_parents": p2.has_parents };
			}
			p = ancestor["S"+p.sn];
			same = false;
		}
		pie_bg( pg, r1+10, r2, a1, a2, p );
		if( p.fn != "?" ) {
			var c = "";
			if( p.birth_place !== undefined && p.birth_place != "" ) {
				c += " bi-t"+lieux[p.birth_place].c;
			}
			if( p.baptism_place !== undefined && p.baptism_place != "" ) {
				c += " ba-t"+lieux[p.baptism_place].c;
			}
			if( p.death_place !== undefined && p.death_place != "" ) {
				c += " de-t"+lieux[p.death_place].c;
			}
			if( p.burial_place !== undefined && p.burial_place != "" ) {
				c += " bu-t"+lieux[p.burial_place].c;
			}
			if( a_m[gen-1] == "C3" ) {
				text_C3( pg, r1+10, r2, a1, a2, sosa, p, c );
			} else if( a_m[gen-1] == "R3" && !same) {
				text_R3( pg, r1+10, r2, a1, a2, sosa, p, c );
			} else if( a_m[gen-1] == "R2" && !same) {
				text_R2( pg, r1+10, r2, a1, a2, sosa, p, c );
			} else if( a_m[gen-1] == "R1" || same) {
				text_R1( pg, r1+10, r2, a1, a2, sosa, p, c );
			}
		}
		if( sosa % 2 == 0 ) {
			pie_m_bg( pg, r1, r1+10, a1, a2+delta, p );
			if( p.marriage_date !== undefined ) {
				var c = "";
				if( p.marriage_place !== undefined && p.marriage_place != "" ) {
					c += " ma-t"+lieux[p.marriage_place].c;
				}
				var l = path1( pg, "pmS"+sosa, r1+5, a1, a2+delta );
				text2( pg, "pmS"+sosa, p.marriage_date, c, l, 10 );
			}
			pie_contour( pg, r1, r2, a1, a2+delta );
			pie_middle( pg, r1+10, r2, a2 );
			pie_m( pg, r1, r1+10, a1, a2+delta, p );
		} else {
			ancestor["S"+sosa].marriage_place = ancestor["S"+(sosa-1)].marriage_place;
		}
		pie( pg, r1+10, r2, a1, a2, p );
		if( p.has_parents) {
			up( pg, r1+10, a1, a2, sosa, p );
		} else {
			no_up( pg, r1+10, a1, a2, sosa, p );
		}
	}
}

//document.documentElement.style.overflow = 'hidden';

const window_h = window.innerHeight;
const window_w = Math.round( window_h * svg_ratio );
const window_cx = Math.round( window_w / 2 );
const window_cy = Math.round( window_h / 2 );
root.style.setProperty( '--fc-tool-size', (window.innerWidth - window_w) + "px" );

fanchart.setAttribute( "height", window_h );
fanchart.setAttribute( "width", window_w );

// Tools
document.getElementById("b-home").onclick = function() {
	window.location = link_to_person;
};
document.getElementById("b-refresh").onclick = function() {
	fitScreen();
};
document.getElementById("b-zoom-in").onclick = function() {
	zoom( window_cx, window_cy, zoom_factor, +1 );
};
document.getElementById("b-zoom-out").onclick = function() {
	zoom( window_cx, window_cy, zoom_factor, -1 );
};
document.getElementById("b-gen-add").onclick = function() {
	if( max_gen < 10 ) {
		var p = ancestor["S1"];
		var oc = p.oc;
		if( oc != "" && oc != 0 ) { oc = "&oc=" + oc } else { oc = "" }
		window.location = link_to_person + "m=A&t=FC&mono=" + mono + "&tool=" + tool + "&implex=" + implex + "&p=" + p.fnk + "&n=" + p.snk + oc + "&v=" + (max_gen+1);
	}
};
document.getElementById("b-gen-del").onclick = function() {
	if( max_gen > 1 ) {
		var p = ancestor["S1"];
		var oc = p.oc;
		if( oc != "" && oc != 0 ) { oc = "&oc=" + oc } else { oc = "" }
		window.location = link_to_person + "m=A&t=FC&mono=" + mono + "&tool=" + tool + "&implex=" + implex + "&p=" + p.fnk + "&n=" + p.snk + oc + "&v=" + (max_gen-1);
	}
};
document.getElementById("b-implex").onclick = function() {
	if( implex == "" ) {
		implex = "off";
	} else {
		implex = "";
	}
	var p = ancestor["S1"];
	var oc = p.oc;
	if( oc != "" && oc != 0 ) { oc = "&oc=" + oc } else { oc = "" }
	window.location = link_to_person + "m=A&t=FC&mono=" + mono + "&tool=" + tool + "&implex=" + implex + "&p=" + p.fnk + "&n=" + p.snk + oc + "&v=" + max_gen;
}
document.getElementById("b-places-hl").onclick = function() {
	document.body.className = "places-list place_hl";
	tool = "place_hl";
};
document.getElementById("b-places-colorise").onclick = function() {
	document.body.className = "places-list place_color";
	tool = "place_color";
	fanchart.classList.add( "bi" );
	fanchart.classList.add( "ba" );
	fanchart.classList.add( "ma" );
	fanchart.classList.add( "de" );
	fanchart.classList.add( "bu" );
	document.getElementById( "bi" ).checked = true;
	document.getElementById( "ba" ).checked = true;
	document.getElementById( "ma" ).checked = true;
	document.getElementById( "de" ).checked = true;
	document.getElementById( "bu" ).checked = true;
};
document.getElementById( "bi" ).checked = true;
document.getElementById( "ba" ).checked = true;
document.getElementById( "ma" ).checked = true;
document.getElementById( "de" ).checked = true;
document.getElementById( "bu" ).checked = true;
if( !has_ba ) {
	document.getElementById( "ba" ).classList.add( "none" );
}
if( !has_bu ) {
	document.getElementById( "bu" ).classList.add( "none" );
}
document.getElementById("bi").onclick = function() {
	fanchart.classList.toggle( "bi" );
};
document.getElementById("ba").onclick = function() {
	fanchart.classList.toggle( "ba" );
};
document.getElementById("ma").onclick = function() {
	fanchart.classList.toggle( "ma" );
};
document.getElementById("de").onclick = function() {
	fanchart.classList.toggle( "de" );
};
document.getElementById("bu").onclick = function() {
	fanchart.classList.toggle( "bu" );
};
document.getElementById("b-death-age").onclick = function() {
	document.body.className = "death-age";
	tool = "death-age";
};
document.getElementById("b-no-tool").onclick = function() {
	document.body.className = "";
	tool = "";
};
document.getElementById("b-no-buttons").onclick = function() {
	document.getElementById("buttons").style.display = "none";
};

// Initial state for tools
if( tool == "place_hl" ) {
	document.body.className = "places-list place_hl";
} else if( tool == "place_color" ) {
	document.body.className = "places-list place_color";
} else if( tool == "death-age" ) {
	document.body.className = "death-age";
}

fitScreen();
