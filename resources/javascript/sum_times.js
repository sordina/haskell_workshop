
(function() {

var table = document.getElementById('toc')

var tds = table.getElementsByTagName('tr')

var total = 0

var sum = function(e){
	var m = e.innerText.match(/(\d+)(h|m)/)
	if(m) {
		var n = Number(m[1])
		if(m[2] == 'h') {
			total += 60 * n
		}
		else if (m[2] == 'm') {
			total += n
		}
	}
}

for(var i = 0; i < tds.length; i++) {
	sum(tds[i].children[2])
}

var heading = document.getElementById("table-of-contents")

heading.innerText += "  ~  " + Math.floor(total/60) + " Hours"

})();
