
(function() {

document.body.className += " " + localStorage.getItem('floating-toc');
document.body.className += " " + localStorage.getItem('small');

function foo(v) {
	var cn  = document.body.className
	if(cn.match(v)) { document.body.className  = cn.replace(v,''); localStorage.setItem(v, "") }
	else            { document.body.className += " " + v         ; localStorage.setItem(v, v ) }
}

function reposition(chapter) {
	window.scrollTo(0, chapter.offsetTop)
}

function getChapter() {
	var y = window.scrollY
	var chapters = document.getElementsByTagName("h1")
	for(var i = 0; i < chapters.length; i++) {
		yc = chapters[i].parentNode.offsetTop
		if( yc > y + 200 ) {
			return chapters[i-1]
		}
	}
	return chapters[chapters.length - 1]
}

document.onkeypress = function(e) {

	e = e || window.event;
	var charCode = (typeof e.which == "number") ? e.which : e.keyCode;
	if (charCode) {
		var key = String.fromCharCode(charCode)
		     if(key === "t") { foo("floating-toc") }
		else if(key === "o") { var h1 = getChapter(); foo("small"); reposition(h1) }
	}
}

var toc = document.getElementById('toc').cloneNode(true)

toc.className = "floating-toc"
toc.id        = "floating-toc"

document.body.appendChild(toc)

})();
